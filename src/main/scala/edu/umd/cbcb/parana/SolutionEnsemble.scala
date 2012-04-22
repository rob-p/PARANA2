package edu.umd.cbcb.parana

import scala.collection.mutable.{ ArrayBuffer, HashSet, OpenHashMap => HashMap, PriorityQueue }
import scala.collection.JavaConversions._
import scala.math._
import scala.sys.exit

import scalaa.datastruct.SlimGraph

import scalax.io._
import processing.Processor

import org.forester.phylogeny.{ Phylogeny, PhylogenyNode }

import PhylogenyUtils.TreeInfo

object SolutionEnsemble {

case class CostAction( cost: Double, act: String )
case class ScoreCount( var score: Double, var count: BigInt )        

type CountDictT = HashMap[Int, ArrayBuffer[ScoreCount]]
type DvsT = Tuple2[Double, ArrayBuffer[Int]]
type SlnDictT = HashMap[Int, HashMap[Int, Derivation]]

val none = (false,false)
val fw = (true,false)
val rev = (false,true)
val both = (true,true)

val flipStrMap = Map( none -> "n", fw -> "f", rev -> "r", both -> "b" )

val flipDict = Map(
    none -> Map( both -> "b+", fw -> "f+", rev -> "r+", none -> "n" ),
    fw ->   Map( both -> "r+", rev -> "f-r+", none -> "f-", fw -> "n" ),
    rev ->  Map( both -> "f+", rev -> "n", none -> "r-", fw -> "f+r-" ),
    both -> Map( both -> "n", fw -> "r-", rev -> "f-", none -> "b-" )
)

def keyForAction[N <: Comparable[N]]( fk: FlipKey[N], ft: String ) = ft match {   
    case "n" => fk
    case "b+" | "b-" => FlipKey.flipBoth(fk)
    case "f+" | "f-" => FlipKey.flipForward(fk)
    case "r+" | "r-" => FlipKey.flipReverse(fk)
}

def flipType[N <: Comparable[N]]( hvert: FlipKey[N], tvert: FlipKey[N] ) = {
    flipDict(hvert.f, hvert.r)(tvert.f, tvert.r)
}

def getSelfLoopCostDict ( cc: Double,  dc: Double,  directed: Boolean ) = {
    val undirected = ! directed;
    val none = (false,false); val fw = (true,false)
    val rev = (false,true); val both = (true,true)

    Map(
    ( none, true ) -> CostAction( cc, "b+" ),
    ( none, false ) -> CostAction( 0.0, "n" ),

    ( rev, true ) -> CostAction( 0.0, "n" ),
    ( rev, false ) -> CostAction( dc, "b-" ),

    ( fw, true ) -> CostAction( 0.0, "n" ),
    ( fw, false ) -> CostAction( dc, "b-" ),

    ( both, true ) -> CostAction( 0.0, "n" ),
    ( both, false ) -> CostAction( dc, "b-" )
    )
}

def getSelfLoopCostFunDict ( cc: Double, dc: Double, directed: Boolean ) = {
    val undirected = !directed;
    val none = ((false,false)); val fw = ((true,false));
    val rev = ((false,true)); val both = ((true,true));

    val selfLoopCostDict = Map(
		(none, true) -> ((p: Double) => CostAction(p*cc, "b+")),
		(none, false) -> ((p: Double) => CostAction(p*dc, "n")),
        (rev, true) -> ((p: Double) => CostAction( (1.0-p)*dc, "n" )),
        (rev, false) -> ((p: Double) => CostAction(p*dc, "b-")),
        (fw, true) -> ((p: Double) => CostAction( (1.0-p)*dc, "n")),
        (fw, false) -> ((p: Double) => CostAction( p*dc, "b-")),
        (both, true) -> ((p: Double) => CostAction( (1.0-p)*dc, "n")),
	    (both, false) -> ((p: Double) => CostAction( (1.0-p)*cc, "b-"))
	    )
    selfLoopCostDict
}

def getCostDict ( cc: Double,  dc: Double, directed: Boolean ) = {
    val undirected = ! directed;
    val none = (false,false); val fw = (true,false)
    val rev = (false,true); val both = (true,true)

    Map(
    ( none , none ) -> CostAction( 0.0, "n" ),
    ( none , fw )  -> CostAction( cc, "f+" ),
    ( none , rev )  -> CostAction( cc, "r+" ),
    ( none , both )  -> CostAction( if (undirected) {cc} else {2*cc} , "b+" ),

    ( rev , none )  -> CostAction(dc, "r-" ),
    ( rev , fw )  -> CostAction(cc+dc, "f+r-" ),
    ( rev , rev )  -> CostAction(0.0, "n" ),
    ( rev , both )  -> CostAction( cc , "f+" ),

    ( fw , none )  -> CostAction( dc, "f-" ),
    ( fw , fw )  -> CostAction( 0.0, "n" ),
    ( fw , rev )  -> CostAction( cc+dc, "f-r+" ),
    ( fw , both )  -> CostAction( cc , "r+" ),

    ( both , none ) -> CostAction( if (undirected) {dc} else {2*dc}, "b-" ),
    ( both , fw )  -> CostAction( dc, "r-" ),
    ( both , rev )  -> CostAction( dc, "f-" ),
    ( both , both )  -> CostAction( 0.0, "n")
    )

}

def getCostFunDict ( cc: Double, dc: Double, directed: Boolean ) = {
    val undirected = ! directed;
    val none = (false,false); val fw = (true,false)
    val rev = (false,true); val both = (true,true)

    val costDict = Map(
    ( none, none) -> ((pf: Double, pr: Double ) => CostAction( 0.0, "n" )),
    ( none, fw ) -> ((pf: Double, pr: Double ) => CostAction( pf*cc, "f+" )),
    ( none, rev ) -> ((pf: Double, pr: Double ) => CostAction( pr*cc, "r+" )),
    ( none, both)  -> ((pf: Double, pr: Double ) => CostAction( if (undirected) { pf*cc } else { (pf+pr)*cc },  "b+" )),


    ( rev , none)  -> ((pf: Double, pr: Double ) => CostAction( pr*dc, "r-" )),
    ( rev , fw  ) -> ((pf: Double, pr: Double ) => CostAction( pf*cc + pr*dc, "f+r-" )),
    ( rev , rev ) -> ((pf: Double, pr: Double ) => CostAction( (1.0-pr)*dc, "n")),
    ( rev , both ) -> ((pf: Double, pr: Double ) => CostAction( pf*cc + (1.0-pr)*dc , "f+")),

    ( fw, none ) -> ((pf: Double, pr: Double ) => CostAction( pf*dc, "f-")),
    ( fw, fw  ) -> ((pf: Double, pr: Double ) => CostAction( (1.0-pf)*dc, "n")),
    ( fw, rev ) -> ((pf: Double, pr: Double ) => CostAction( pf*dc + pr*cc, "f-r+")),
    ( fw, both ) -> ((pf: Double, pr: Double ) => CostAction( (1.0-pf)*dc + pr*cc, "r+")),

    ( both, none) -> ((pf: Double, pr: Double ) => CostAction( if (undirected) {(1.0-pf)*dc} else { (pf+pr)*dc },  "b-" )), // was p),
    ( both, fw ) -> ((pf: Double, pr: Double ) => CostAction( (1.0-pf)*dc + pr*dc, "r-" )),
    ( both, rev ) -> ((pf: Double, pr: Double ) => CostAction( pf*dc + (1.0-pr)*dc, "f-" )),
    ( both, both) -> ((pf: Double, pr: Double ) => CostAction( if (undirected) {(1.0-pf)*cc} else { (1.0-pf)*dc + (1.0-pr)*dc}, "n" ))
    )
	costDict
}

def projectToReversedGraph[N <: Comparable[N]]( H: ForwardHypergraph[N] ) = {
    val M = H.size
    val G = SlimGraph.empty[Int]

    H.edgeList.foreach{ e =>
        //(0 until M).foreach{ eid =>
        //val e = H.edge(eid)
        val h = e.head
        e.tail.foreach{ t =>
            // Add the *directed* edges
            G.insertEdge(t,h,true)
        }
    }
    println("G size = "+G.size+", G order = "+G.order)
    G
}

def topologicalOrder[N <: Comparable[N]]( H: ForwardHypergraph[N] ) = {
    val g = projectToReversedGraph( H )    
    val order = g.topologicalOrdering
    val diff = (0 until H.order).toSet &~ g.nodes.toSet
    //diff.foreach{ d => order += d }
    //println("ORDER SIZE = "+order.size)
    order
}


def isLost(n: PhylogenyNode) = { n.getName.contains("LOST") }

 def buildSolutionSpaceGraph( 
   t : Phylogeny,
   ti : TreeInfo,
   cc : Double, 
   dc : Double,
   penalty : Double,
   directed : Boolean ) = {
     
	def differentExtantNetworks(u: PhylogenyNode, v: PhylogenyNode): Boolean = { 
        if( u == v ) { return false }
        val venets = ti.enets(v)
        ti.enets(u).foreach{ ue =>
            if (venets contains ue) { return false }
        }
        true
        //((ti.enets(u) & ti.enets(v)).size == 0) 
    }

    val costMap = getCostDict(cc,dc,directed)
    val selfLoopCostMap = getSelfLoopCostDict(cc,dc,directed)

	var slnSpaceGraph = new ForwardHypergraph[PhylogenyNode]

	def addIncomingHyperedge( k: FlipKey[PhylogenyNode], rnode: PhylogenyNode, onode: PhylogenyNode ) {
		val canonicalDerivCost = 0.0
        // Self loop
        if (k.arity == 1) {
            val selfNode = rnode;
            if ( selfNode.isInternal ) {

                // Get the children nodes
                val descendants = selfNode.getDescendants
                val (lrn, rrn) = (descendants.get(0), descendants.get(1))

                // This vertex has 2 incoming hyper edges
                // 1 -- we don't flip the self-loop
                val noFlipLL = FlipKey( lrn, lrn, k.f, k.r )
                val noFlipRR = FlipKey( rrn, rrn, k.f, k.r )
                val noFlipLR = FlipKey( lrn, rrn, k.f, k.r )
                // 2 -- we flip the self loop
                val dualFlipLL = FlipKey.flipBoth( noFlipLL )
                val dualFlipRR = FlipKey.flipBoth( noFlipRR )
                val dualFlipLR = FlipKey.flipBoth( noFlipLR )

                val noFlipEdge = ArrayBuffer.empty[FlipKey[PhylogenyNode]]
                val dualFlipEdge = ArrayBuffer.empty[FlipKey[PhylogenyNode]]

                if ( !isLost(lrn) ) {
                    noFlipEdge += noFlipLL; dualFlipEdge += dualFlipLL
                }
                if ( !isLost(rrn) ) {
					noFlipEdge += noFlipRR; dualFlipEdge += dualFlipRR
                }

                if (! differentExtantNetworks(lrn, rrn) && ! (isLost(lrn) || isLost(rrn)) ) {
                    noFlipEdge += noFlipLR
                    dualFlipEdge += dualFlipLR
                }
                if ( noFlipEdge.size > 0 ) { slnSpaceGraph.addEdge( noFlipEdge.toArray, k, 0.0 ) }
                if ( dualFlipEdge.size() > 0 ) {
					val w = selfLoopCostMap( ((k.f, k.r), (dualFlipLL.f || dualFlipLL.r)) ).cost                 
	                //w += existencePenalty(ti, k, penalty, w);
                    if ( !w.isInfinite ) { slnSpaceGraph.addEdge( dualFlipEdge.toArray, k, w ) }
                }
            }
        } else { // Not a self loop
            if ( rnode.isInternal ) {
                // Get the children nodes
                val descendants = rnode.getDescendants
                val (lrn, rrn) = (descendants.get(0), descendants.get(1))

                // This vertex has 2 incoming hyper edges
                // 1 -- we don't flip the self-loop
                val noFlipL = FlipKey( lrn, onode, k.f, k.r )
                val noFlipR = FlipKey( rrn, onode, k.f, k.r )
                // 2 -- we flip the self loop
                val dualFlipL = FlipKey.flipBoth( noFlipL )
                val dualFlipR = FlipKey.flipBoth( noFlipR )

                val noFlip = ArrayBuffer.empty[FlipKey[PhylogenyNode]]
                val dualFlip = ArrayBuffer.empty[FlipKey[PhylogenyNode]]


                if ( !differentExtantNetworks(lrn, onode) && !(isLost(lrn) || isLost(onode)) ){
                    noFlip += noFlipL; dualFlip += dualFlipL
                }
                if ( !differentExtantNetworks(rrn, onode) && !(isLost(rrn) || isLost(onode)) ){
                    noFlip += noFlipR; dualFlip += dualFlipR
                }

                if ( noFlip.size > 0 ) {
                    slnSpaceGraph.addEdge( noFlip.toArray, k, canonicalDerivCost)
                }
                if ( dualFlip.size > 0 ) {
                    val w = costMap( ( (k.f,k.r), (dualFlipL.f, dualFlipL.r) )	 ).cost
                    //w += existencePenalty(ti, k, penalty, w);
                    if ( !w.isInfinite ) { slnSpaceGraph.addEdge( dualFlip.toArray, k, w ); }
                }

                if ( directed ) {
                    val fwFlipL = FlipKey.flipForward(noFlipL); val fwFlipR = FlipKey.flipForward(noFlipR);
                    val revFlipL = FlipKey.flipReverse(noFlipL); val revFlipR = FlipKey.flipReverse(noFlipR);

                    val fwFlip = ArrayBuffer.empty[FlipKey[PhylogenyNode]]
                    val revFlip = ArrayBuffer.empty[FlipKey[PhylogenyNode]]

                    if ( !differentExtantNetworks(lrn, onode) ) {
                       fwFlip += fwFlipL; revFlip += revFlipL;
                    }

                    if ( !differentExtantNetworks(rrn, onode) ) {
                        fwFlip += fwFlipL; revFlip += revFlipR;
                    }

                    if ( fwFlip.size > 0 ) {
                        val w = costMap(  ( (k.f, k.r), (fwFlipL.f, fwFlipL.r)  )  ).cost
                        //w += existencePenalty(ti, k, penalty, w);
                        if ( !w.isInfinite ) { slnSpaceGraph.addEdge( fwFlip.toArray, k , w ) }
                    }
                    if ( revFlip.size > 0 ) {
                        val w = costMap( ( (k.f, k.r), (revFlipL.f, revFlipL.r)   )  ).cost
                        //w += existencePenalty(ti, k, penalty, w);
                        if ( !w.isInfinite ) { slnSpaceGraph.addEdge( revFlip.toArray, k, w ) }
                    }
                }

            }
        }
	}


	// Add nodes to the hypergraph
	val nodes = t.iteratorPreorder.toArray
    val numNodes = nodes.size
    
    var ctr1, ctr2, ctr3 = 0
    println("# of tree nodes "+nodes.size)
    (0 until numNodes).foreach{ i =>
		val u = nodes(i)
		(i until numNodes).foreach{ j =>
			val v = nodes(j)

			if ( (u == v) ||
				 (!differentExtantNetworks(u,v) &&
				  !( ti.subnodes(u).contains(v) || ti.subnodes(v).contains(u) ) &&
				  !( isLost(u) || isLost(v) ) ) ) {
                ctr1 += 1
                slnSpaceGraph.addVertex( FlipKey( u, v, false, false ) )
                if ( ! v.isRoot ) {
                    ctr2 += 1
					slnSpaceGraph.addVertex( FlipKey( u, v, true, true ) );
                }
                if ( directed && u!=v ) {
                    ctr3 += 1
                    slnSpaceGraph.addVertex( FlipKey( u, v, true, false ) );
                    slnSpaceGraph.addVertex( FlipKey( u, v, false, true ) );
                }
			}


		} // forall j
    } // forall i
    println("CTR1 = "+ctr1+", CTR2 = "+ctr2+", CTR3 = "+ctr3)
    val N = slnSpaceGraph.order
	println("\n|V(S)| = "+N)


	(0 until N).foreach{ i => 
		if ( !((i % 1000)==0) || i == N-1 ) {
	        print("\r\rProcessed "+i+"/"+N+" nodes")
        }
	    val k = slnSpaceGraph.vertex(i)
        val FlipKey(u,v,f,r) = k

        //if ( v.isInternal || (u==v) ) {
        addIncomingHyperedge( k, u, v )
        //}
        if ( k.arity() > 1 ) {
            //if ( u.isInternal ) {
            addIncomingHyperedge( k, v, u ) 
            //}
        }

    }

    println("|E(S)|"+slnSpaceGraph.size)

    def vstr(vert: FlipKey[PhylogenyNode]) = { 
        val uname = if ( isLost(vert.u) ) { vert.u.getName.replace( vert.u.getId.toString, ""  )} else { vert.u.getName }
        val vname = if ( isLost(vert.v) ) { vert.v.getName.replace( vert.v.getId.toString, ""  )} else { vert.v.getName }
        uname+"\t"+vname+"\t"+vert.f+"\t"+vert.r 
    }
    
    /*
    val fname = "HYPERGRAPH.txt"
    val output: Output = Resource.fromFile(fname)
    // This next example is the pattern most developers will likely be most comfortable with:
    for{
      // create a processor (signalling the start of a batch process)
      processor <- output.outputProcessor
      // create an output object from it
      out = processor.asOutput
    }{
        val nedge = slnSpaceGraph.size
        // write # of verts
        out.write(nedge+"\n")
        // for each vertex
        (0 until nedge).foreach{ ei =>
            val edge = slnSpaceGraph.edge(ei)
            // write out the head vertex, weight, # tail nodes and tail nodes
            out.write(vstr( slnSpaceGraph.vertex(edge.head) )+"\t"+edge.weight+"\t"+edge.tail.size+"\n")
            out.write(edge.tail.map{ ti => vstr(slnSpaceGraph.vertex(ti)) }.mkString("\n")+"\n ")
        }
    }
    */
    slnSpaceGraph
 }


def leafCostDict[N <: PhylogenyNode with Comparable[N]]( H: ForwardHypergraph[N], T: Phylogeny, G: SlimGraph[String], directed: Boolean, cc: Double, dc: Double, slnDict: SlnDictT ) {
    /*
      Given the duplication tree T, the root vertex rv, the extant graph G and
      the constraints, fill in the values for the leaf nodes of the hypergraph
    */
    val undirected = !directed
    // Cost of going from the state encoded by a node to the state of a true graph edge

    val none = (false,false); val fw = (true,false)
    val rev = (false,true); val both = (true,true)

    println("CC = "+cc+", DC = "+dc)
    val costFunDict = getCostFunDict(cc, dc, directed)
    val selfLoopCostFunDict = getSelfLoopCostFunDict(cc, dc, directed )

    val N = H.order
    val M = H.size

    println("total # of hypernodes = "+N)
    println("total # of hyperedges = "+M)

    // The list of all hypernodes with no descendants
    // We'll have either N^2 or (N^2)/2 leaf hypernodes (depending
    // on directedness)
    val numExtantNodes = G.order
    var numConn = ( numExtantNodes * numExtantNodes )
    // An undirected graph only has half the number of edges
    if (undirected) { numConn /= 2 }
    // Allocate an array to store the leaf hypernode inds
    val leafHypernodes = ArrayBuffer.empty[Int]

    // For every hypernode, it's a leaf <=> it has no incoming edges
    (0 until N).foreach{ i =>
        val elist = H.incident(i)
        val node = H.vertex(i)
        if ( elist.size == 0 ) { leafHypernodes += i }
        //if ( node.u.isExternal && node.v.isExternal ) { leafHypernodes += i }
    }

    println("# of leaf hypernodes = "+leafHypernodes.size)

    /** What was the original purpose of this?
    typedef typename GT::vertex_descriptor NodeT;
    typedef unordered_set<NodeT> NodeSetT;

    
    // Is the node e contained in the set s?
    auto contains = [] ( const NodeSetT& s, NodeT e ) { return s.find(e) != s.end(); };

    NodeSetT extantNodes;

    Google<int>::Set leafIds;
    leafIds.set_empty_key(-1);

    for ( auto l : T->getLeavesId() ) { leafIds.insert(l); }

    auto vp = boost::vertices(G);
    for ( auto it = vp.first; it != vp.second; ++it ) {
        auto v = *it;
        auto idx = G[v].idx;
        // found this node's id in the set of extant vertices
        if ( leafIds.find(idx) != leafIds.end() ) { extantNodes.insert(v); }
    }

    // Map from tree node ID to graph vertex ID
    unordered_map<int, NodeT> idToVertMap;
    for ( auto v = boost::vertices(G).first; v != boost::vertices(G).second; ++ v ){
        idToVertMap[ G[*v].idx ] = *v;
    }
    */
    implicit def booleanToInt(x: Boolean) = if ( x ) { 1 } else { 0 }

    var nlost = 0
    var nef = 0
    var tweight = 0.0
    var tcost = 0.0
    // For every leaf hypernode
    leafHypernodes.foreach{ n =>
        val FlipKey(u,v,f,r) = H.vertex(n);

        val nameU = u.getName
        val nameV = v.getName

        // Check to see if u, v, or both have been lost
        val lostU = isLost(u) || u.isInternal || !G.adjList.contains(u.getName)
        val lostV = isLost(v) || v.isInternal || !G.adjList.contains(v.getName)
        /*
        auto endOfMap = idToVertMap.end();
        bool lostU = ( idToVertMap.find( nd.u() ) == endOfMap ) || ( ! contains(extantNodes, idToVertMap[nd.u()]) );
        bool lostV = ( idToVertMap.find( nd.v() ) == endOfMap ) || ( ! contains(extantNodes, idToVertMap[nd.v()]) );
        */
        // The cost to / between lost nodes is always 0
        if ( lostU || lostV ) {
            val lostCost = 0.0;
            val ev = ArrayBuffer.empty[Int]
            val es = HashSet.empty[FlipT]
            slnDict(n) = HashMap( 0 -> Derivation(lostCost, n, ev, es) )
            nlost += 1
        } else {
            // Otherwise, u and v both exist in the extant
            // network, so get the appropriate info
            //val FlipKey(u, v, f, r) = nd // (nd.u, nd.v, nd.f, nd.r)

            if ( u != v ) {
                val d_f = G.hasEdge(u.getName, v.getName, directed)
                val d_r = G.hasEdge(v.getName, u.getName, directed)
                val w_f = if (d_f) { G.getWeight(u.getName, v.getName) } else { 0.0 }
                val w_r = if (d_r) { G.getWeight(v.getName, u.getName) } else { 0.0 }
                val w = (w_f+w_r)
                assert( w <= 2.0 )
                tweight += (w) * (d_f + d_r) * (f + r)
                if ( undirected ) { assert( w_f == w_r && f == r ) }

                //auto costFlipProb = costDict[ make_tuple(f,r) ][ make_tuple(d_f,d_r) ];
                val costFlipProb = costFunDict( ((f,r), (d_f,d_r)) )(w_f, w_r)

                /*
                if (costFlip != costFlipProb) {
                    cerr << "whoops for transition (" << f << ", " << r << ") => (" << d_f << ", " << d_r << "), and (w_f, w_r) = (" << w_f << ", " << w_r << ")\n";
                    cerr << "costFlip = (" << get<0>(costFlip) << ", " << get<1>(costFlip) << "), but costFlipProb = (" << get<0>(costFlipProb) << ", " << get<1>(costFlipProb) << ")\n";
                    exit(1);
                }
                */
                val CostAction(cost, flip) = costFlipProb
                tcost += cost
                val effectiveEdges = HashSet.empty[FlipT]
                if ( flip != "n" ) { nef += 1; effectiveEdges += FlipT( u, v, flip) }
                
                val ev = ArrayBuffer.empty[Int]
                slnDict(n) = HashMap( 0 -> Derivation(cost, n, ev, effectiveEdges) )

            } else {
                val hasSelfLoop = G.hasEdge(u.getName, v.getName, directed)
                val w_l = if (hasSelfLoop) { G.getWeight(u.getName, v.getName) } else { 0.0 }
                tweight += w_l * (hasSelfLoop) * (f + r)
                //auto costFlipProb = selfLoopCostDict[ make_tuple(f,r) ][ hasSelfLoop ];
                val costFlipProb = selfLoopCostFunDict( ((f,r), hasSelfLoop) )( w_l )
                /*
                if (costFlip != costFlipProb) {
                    cerr << "whoops for self loop transition (" << f << ", " << r << ") => (" << hasSelfLoop << "), and (w_l) = (" << w_l << ")\n";
                    cerr << "costFlip = (" << get<0>(costFlip) << ", " << get<1>(costFlip) << "), but costFlipProb = (" << get<0>(costFlipProb) << ", " << get<1>(costFlipProb) << ")\n";
                    exit(1);
                }
                */
                val CostAction(cost, flip) = costFlipProb
                tcost += cost
                val effectiveEdges = HashSet.empty[FlipT]

                if ( flip != "n" ) { nef += 1; effectiveEdges += FlipT(u, v, flip) }
                
                val ev = ArrayBuffer.empty[Int]
                slnDict(n) = HashMap( 0  -> Derivation(cost, n, ev, effectiveEdges) )

            } // ( u != v )
        } // ( lostU || lostV )
    } // loop over leaf hypernodes
    var sum = 0.0
    slnDict.foreach{ n => sum += n._2(0).cost }
    println("SUM = "+sum)
    println("NUM LOST= = "+nlost)
    println("NUM EFFECTIVE EDGES ="+nef)
    println("TOTAL WEIGHT = "+tweight)
    println("TOTAL COST = "+tcost)
    /*
    val fname = "BASECASES.txt"
    val output: Output = Resource.fromFile(fname)
    // This next example is the pattern most developers will likely be most comfortable with:
    for{
      // create a processor (signalling the start of a batch process)
      processor <- output.outputProcessor
      // create an output object from it
      out = processor.asOutput
    }{
        slnDict.foreach{ case(k,v) =>
            val vert = H.vertex(k)
            if (!(isLost(vert.u) || isLost(vert.v))) {
                out.write(vert.u.getName+"\t"+vert.v.getName+"\t"+vert.f+"\t"+vert.r+"\t"+v(0).cost+"\n")
            }
        }
    }
    */
}

/** Get the next sln (from "Faster Cube Pruning" paper)
*/
def appendNext(
    score: Double,
    inds: Array[Int],
    sizes: ArrayBuffer[Int],
    pq: PriorityQueue[(Double, Array[Int])],
    computeScore: (Array[Int]) => Double ): Boolean = {

    var i = 0
    while( i < inds.size ){ 
        val newInds = inds.clone()
        newInds(i) += 1
        if ( newInds(i) < sizes(i) ) {
            pq += (( computeScore(newInds), newInds )) 
        }
        if ( inds(i) != 0 ) { return true }
        i += 1
    }

    return true
}

def countEdgeSolutions( 
    ecost: Double,
    tailNodes: Array[Int],
    countDict: CountDictT,
    k: Int ) = {

    // product pointers
    val elemSizes = new ArrayBuffer[Int](tailNodes.size)
    var cost = ecost
    tailNodes.foreach{ t =>
        elemSizes += countDict(t).size
        cost += countDict(t).head.score
    }
    
    import scala.collection.mutable.PriorityQueue
    implicit val ord = Ordering.fromLessThan[(Double, Array[Int])]( (a,b) => a._1 > b._1 )
    val pq = PriorityQueue( (cost, Array.fill(tailNodes.size)(0)) )

    def computeScore( inds: Array[Int] ) = {
        val numNodes = tailNodes.size
        var cost = ecost
        (0 until numNodes).foreach{ i=>
            cost += countDict( tailNodes(i) )( inds(i) ).score
        }
        cost
    }

    val edgeSlns = new ArrayBuffer[ScoreCount](k)
    val epsilon = 2.0
    var numClasses = 0

    while ( pq.nonEmpty && numClasses <= k ) {
        // Get the next best solution score from the top of the queue
        val (cost, inds) = pq.dequeue

        // Compute the number of ways we can obtain this solution
        var numSlns = BigInt(1)
        (0 until inds.size).foreach{ i =>
            numSlns *= countDict( tailNodes(i) )( inds(i) ).count
        }

        // put this solution into our # sln dict
        if ( edgeSlns.size == 0 || abs(cost - edgeSlns.last.score) > epsilon ) {
            edgeSlns += ScoreCount(cost, numSlns)
        } else { // we found a solution of this score
            edgeSlns.last.count += numSlns
        }

        appendNext( cost, inds, elemSizes, pq, computeScore )
        numClasses = edgeSlns.size
    }

    if (edgeSlns.size == k ){ 
        edgeSlns.view(0, edgeSlns.size-1)
    } else {
        edgeSlns
    }
}

def computeAlphasDouble( slnVec: ArrayBuffer[ScoreCount], k: Int, total: BigInt ) = {
    
    val scores = new ArrayBuffer[Double](slnVec.size)
    val (bestScore, worstScore) = (slnVec.head.score, slnVec.last.score)
    
    if ( bestScore == worstScore && slnVec.size() > 1 ) {
        println("bestScore ("+bestScore+") == worstScore ("+worstScore+")")
        println("=== slnVec ===")
        slnVec.foreach{ e =>
            println("score = "+e.score+", count = "+e.count)
        }
        exit(-1)
    }

    val diff = if( worstScore == bestScore ) { 1.0 } else { worstScore - bestScore }
    val N = slnVec.size

    //double scale = (mpfr::log( J ) - mpfr::log( I )).toDouble() / diff;
    //double scale = 2.0 * estimateThermodynamicBeta( slnVec, bestScore ); // (6.9*k) / diff; // (1.25 * k) / diff;
    //double scale = 1.8;
    val scale = 60.0 / diff
    //double scale = (0.5 * k) / diff;//(2.0 * N) / diff;//(1.5*k) / diff;
    //std::cerr << " **** beta = " << scale << " **** \n";
    var sum = 0.0

    slnVec.foreach{ e =>
        val a = abs( bestScore - e.score ) * scale 
        val s = exp(-a)
        scores += s
        sum += s
    }

    val invSum = 1.0 / sum
    val alphas = new ArrayBuffer[Double]( slnVec.size )
    
    scores.foreach{ s =>
        alphas += (s * invSum)
    }
    alphas
}


def upDown( 
        H: ForwardHypergraph[PhylogenyNode],
        t: Phylogeny, 
        ti: TreeInfo,
        penalty: Double, 
        order: ArrayBuffer[Int],
        slnDict: SlnDictT, 
        countDict: CountDictT, 
        k: Int,
        outputName: String, 
        outputKeys: ArrayBuffer[FlipKey[PhylogenyNode]] ) {
        
        def vstr(vert: FlipKey[PhylogenyNode]) = { 
            val uname = if ( isLost(vert.u) ) { vert.u.getName.replace( vert.u.getId.toString, ""  )} else { vert.u.getName }
            val vname = if ( isLost(vert.v) ) { vert.v.getName.replace( vert.v.getId.toString, ""  )} else { vert.v.getName }
            uname+"\t"+vname+"\t"+vert.f+"\t"+vert.r 
        }
        // Compute the *weighted* probability of each edge being in
        // the top k distinct scoring solutions
        var costsum = 0.0
        // Each leaf has a single solution which is, by definition, of optimal cost
        order.foreach{ vit =>
            val vertex = H.vertex(vit)
            if ( H.incident(vit).size == 0) { 
                countDict.getOrElseUpdate(vit, ArrayBuffer.empty[ScoreCount]) += ScoreCount( slnDict(vit)(0).cost, BigInt(1) )
                costsum += slnDict(vit)(0).cost
            }
        }
        /*
        val cdname = "CDICT.txt"
        val cdout: Output = Resource.fromFile(cdname)
        // This next example is the pattern most developers will likely be most comfortable with:
        for{
        // create a processor (signalling the start of a batch process)
        processor <- cdout.outputProcessor
        // create an output object from it
        out = processor.asOutput
        }{
            out.write(countDict.size +"\n")
            countDict.foreach{ case (vit, scarray) =>
                val vert = H.vertex(vit)
                out.write( vstr(vert) +"\t"+ scarray.size +"\n" )
                scarray.foreach{ case ScoreCount(cost, num) =>
                    out.write(cost +"\t"+ num +"\n" )
                }
            }
        }
        */
        

        println("COSTSUM = "+costsum)
        println("ORDER SIZE = "+order.size)
        println("SIZE OF COUNT DICT = "+ countDict.size )
        type EdgeIdT = Int
        
        // For each edge, count the number of solutions having each score
        val edgeCountMap = HashMap.empty[EdgeIdT, HashMap[Double, BigInt]]
        val edgeProbMap = HashMap.empty[EdgeIdT, HashMap[Double, Double]]

        // A map holding which edges are used to obtain *an* optimal
        // solution for each vertex
        val usedEdges = HashMap.empty[Int, HashMap[Double, HashSet[Int]]]

        val N = order.size()//H.order
        println("# of vertices in order is "+N)
        var ctr = 0
        /*
    val oname = "EDGES.txt"
    val oput: Output = Resource.fromFile(oname)
    // This next example is the pattern most developers will likely be most comfortable with:
    for{
      // create a processor (signalling the start of a batch process)
      processor <- oput.outputProcessor
      // create an output object from it
      out = processor.asOutput
    }{
    */
        // For each vertex, in topological order (up)
        /*var vnum = 0
        val osize = order.size
        while( vnum < osize ) {
            val vit = order(vnum)
            vnum +=1*/
        order.foreach{ vit =>
            if ( ctr % 1000 == 0 || ctr == N-1 ) {
                print("\r\r Processed "+100.0*(ctr.toFloat/N)+"% of the vertices")
            }
            

            // Put the results in an ordered map -- the sorted
            // property will be useful later for maintaining only the
            // k-best score classes
            val vert = H.vertex(vit)
            var edgeCostMap = new java.util.TreeMap[Double, ArrayBuffer[(Int,BigInt)]]
            //println("SLN FOR NODE "+vert)
            //out.write( vstr(vert) +"\t")
            var nedgesln = ArrayBuffer.empty[BigInt]
            // loop over all incoming edges and compute the # of
            // solutions over each edge as well as that solution's cost
            H.incident(vit).foreach{ e => 

                val edge = H.edge(e)
                val hind = edge.head
                val w = edge.weight
                //val tvert = H.vertex(edge.tail(0))
                
                //println( vstr(vert) +", edge "+e )
                val printMe = false // ( vert.u.getName == "129064_Fr" )
                val currentEdgeSlns = countEdgeSolutions( w, edge.tail, countDict, k)
                //if ( printMe ) {
                //    println("EDGE: "+e+" counts = "+currentEdgeSlns.map{ x => x.count}.mkString(", ") )
                //}
                //out.write(w+"\t"+currentEdgeSlns.size+"\t"+currentEdgeSlns.map{ case ScoreCount(s,c) => s+"\t"+c }.mkString("\t")+"\n")
                nedgesln += currentEdgeSlns.map{ ec => ec.count }.sum           
                if (ctr == N-1) { println(H.vertex(vit)); println(currentEdgeSlns) }

                currentEdgeSlns.foreach{ case ScoreCount(score, count) =>
                    val edgeContrib = (e, count)
                    edgeCostMap.getOrElseUpdate( score, ArrayBuffer.empty[(Int, BigInt)] ) += edgeContrib
                    edgeCountMap.getOrElseUpdate( e, HashMap.empty[Double, BigInt] )(score) = count
                }
            }
            //out.write(nedgesln.sortWith{ (x,y) => x < y}.mkString("\t")+"\n")
            //out.write(nedgesln + "\n")
            // If we traversed any edges
            if ( edgeCostMap.size > 0 ) {
                type EdgeSlnT = Tuple3[Double, BigInt, Int]
                var minCost = Double.PositiveInfinity
                val mk = min( edgeCostMap.size, k )
                var ectr = 0

                // for all incoming score classes
                val ecmIt = edgeCostMap.iterator
                while( ecmIt.hasNext && ectr < mk ) {

                    val (score, providingEdges) = ecmIt.next
                    
                    // minimum cost incoming score
                    minCost = min( minCost, score )

                    // will count the number of solutions in this score class
                    var numSln = BigInt(0)
                
                    // Update the information at the derived vertices
                    providingEdges.foreach{ 
                        case (edgeInd, count) =>
                        // Add this edge to the set of used edges for
                        // the derived vertex for the given score class.
                        usedEdges.
                                  getOrElseUpdate(vit, HashMap(score -> HashSet.empty[Int])).
                                  getOrElseUpdate(score, HashSet.empty[Int]) += edgeInd

                        // update the total number of solutions of the
                        // derived vertex
                        numSln += count
                    }
                    if ( ctr == N-1 ) { println("Score = "+score+", providing edges = "+providingEdges+", numSln = "+numSln) }
                    
                    // There are 'numSln' derivations yielding vit at
                    // a score of 'score'
                    countDict.getOrElseUpdate(vit, ArrayBuffer.empty[ScoreCount]) += ScoreCount( score, numSln )

                    // Now that we have a total count for the derived
                    // vertex, compute each edge's contribution
                    /*var pec = 0
                    val npe = providingEdges.size
                    while( pec < npe ) {
                        val (edgeInd, count) = providingEdges(pec)
                        pec += 1
                        */
                    providingEdges.foreach{ case (edgeInd, count) =>
                        val edgeProb = ( BigDecimal(count) / BigDecimal(numSln) ).doubleValue
                        // The probability of traversing this edge for
                        // derivations with the given score
                        edgeProbMap.getOrElseUpdate(edgeInd, HashMap.empty[Double, Double])(score) = edgeProb
                    }    
                    ectr += 1                
                }

                // Find the minimum cost edge
                val fs = HashSet.empty[FlipT]
                slnDict.getOrElseUpdate(vit, HashMap.empty[Int, Derivation])(0) = Derivation(minCost, 0, ArrayBuffer.empty[Int], fs)
            }
            ctr += 1
        }
        // loop over verts
//} // write to file
        type ProbMapT = HashMap[Int, Double]
        
        /*auto getOrElse = [] ( probMapT& pm, const size_t& key, double alt ) {
            return (pm.find(key) == pm.end()) ? alt : pm[key];
        };
        */

        println("Down step")

        val probMap = new ProbMapT
        val outProbMap = new ProbMapT
        
        val rootKey = FlipKey( t.getRoot, t.getRoot, false, false )
        val rootInd = H.index(rootKey)

        // The root gets a probability of 1
        probMap(rootInd) = 1.0

        ctr = 0
        var tot = order.size

        val rootFlip = FlipKey.flipBoth(rootKey)
        H.addVertex( rootFlip )
        val rootIdNoFlip = H.index(rootKey)
        val rootIdFlip = H.index(rootFlip)
        
        println(countDict(rootInd))
        /*
        H.incident(rootIdNoFlip).foreach{ ei =>
            edgeCountMap(ei).foreach{ case (score, count) =>
                println("Edge "+ei+" has score "+score+" with count "+count)
            }
        }
        */

        // Compute the probabilities (outside)
        // Loop over all vertices in reverse topological order
        /*vnum = osize-1
        while( vnum >= 0 ){
            val vit = order(vnum)
            vnum -= 1*/
        order.reverseIterator.foreach{ vit =>
            print("\r\rprocessing node " + ctr + "/" + tot)
            ctr += 1 
            // The current vertex and it's probability
            val key = H.vertex(vit)
            val parentProb = probMap.getOrElse(vit, 0.0)

            // The total # of derivations of this vertex (over all
            // considered score classes)
            var total = BigInt(0)
            (0 until countDict(vit).size).foreach{ i => total += countDict(vit)(i).count }

            // Compute the weights for each of the score classes
            // considered at this node
            val alphas = computeAlphasDouble( countDict(vit), k, total )

            // for each top-k score class
            (0 until countDict(vit).size).foreach{ i =>
                // The score and it's count
                val ScoreCount(pScore, pCount) = countDict(vit)(i)

                var tprob = 0.0
                // for all incoming edges contributing to this score
                val incoming = usedEdges.
                                        getOrElse(vit, HashMap.empty[Double,HashSet[Int]]).
                                        getOrElse(pScore, HashSet.empty[Int])
                incoming.foreach{ e =>
                    // The conditional probability of taking edge 'e'
                    // given than we're deriving vertex *vit at score 'pScore'
                    val condProb = edgeProbMap(e)(pScore)
                    tprob += condProb
                    val tail = H.getTail(e)

                    val ft = flipType( H.vertex(vit), H.vertex(tail(0)) )
                    val outKey = keyForAction( H.vertex(vit), ft )
                    val outInd = H.index(outKey)
                    outProbMap(outInd) =  outProbMap.getOrElse(outInd, 0.0) + ( parentProb * (alphas(i) * condProb))
                    //outProbMap(outInd) += ( parentProb * (alphas(i) * condProb))

                    // for all tail vertices of this edge
                    tail.foreach{ tind =>
                        // For each vertex in the tail of e, it gets
                        // probability mass for deriving *vit
                        // proportional to e's contribution
                        probMap(tind) = probMap.getOrElse(tind, 0.0) + (parentProb * ( alphas(i) * condProb ))
                        //outProbMap[tind] += (parentProb * ( alphas[i] * condProb ));
                    }
                }
                /*
                #ifdef DEBUG
                if ( usedEdges[*vit][pScore].size() > 0 && std::abs(tprob - 1.0) > 1e-4 ) {
                    cerr << "ERROR : \n";
                    cerr << "cond. probs from [" << t->getNodeName(key.u()) << ", " << t->getNodeName(key.v()) << "] (" << key.f() << ", " << key.r() << ")\n";
                    cerr << "score = " << pScore << ", count = " << pCount << ", tprob = " << tprob << "\n";
                    exit(1);
                }
                #endif
                */
            }
        }

        var i = 0
        
        countDict(rootInd).foreach{ sc =>
            println("score class "+i+" has "+sc.count+" solutions of score  "+sc.score)
            i += 1
        }

        //bool restrictOutput = (outputKeys.size() != 0);
        var restrictOutput = false
        val fname = outputName
        val output: Output = Resource.fromFile(fname)
        // This next example is the pattern most developers will likely be most comfortable with:
        
        for{
          // create a processor (signalling the start of a batch process)
          processor <- output.outputProcessor
          // create an output object from it
          out = processor.asOutput
        }{
            
          (0 until H.order).foreach { vid =>
          // all writes to out will be on the same open output stream/channel
            val key = H.vertex(vid)

            if ( H.incident(vid).size == 0 && vid != rootIdFlip && vid != rootIdNoFlip ) {
                if ( (outProbMap contains vid) && (probMap contains vid) ) {
                    if ( outProbMap(vid) != probMap(vid) && outProbMap(vid) > probMap(vid) ) {
                        println("inProbMap has "+probMap(vid)+", outProbMap has "+ outProbMap(vid))
                    }
                    outProbMap(vid) = probMap(vid)
                }
            }

            val approxInProb = probMap.getOrElse(vid, 0.0);
            val approxProb = outProbMap.getOrElse(vid, 0.0);

            // ====================
            /*
            auto vert = H->vertex(*vit);
            std::string from, to;
            cout << "v = [" << t->getNodeName(vert.u()) << ", " << t->getNodeName(vert.v()) << "] (" << vert.f() << ", " << vert.r() << "), inProb = " << approxInProb << "\n";
            if ( !t->isLeaf(vert.u()) ) {
                auto rnode = vert.u();
                int LRN = t->getSonsId(rnode)[0]; int RRN = t->getSonsId(rnode)[1];
                cout << "children of " << t->getNodeName(vert.u()) << " are " << t->getNodeName(LRN) << " and " << t->getNodeName(RRN) << "\n";
            }

            if ( !t->isLeaf(vert.v()) ) {
                auto rnode = vert.v();
                int LRN = t->getSonsId(rnode)[0]; int RRN = t->getSonsId(rnode)[1];
                cout << "children of " << t->getNodeName(vert.v()) << " are " << t->getNodeName(LRN) << " and " << t->getNodeName(RRN) << "\n";

            }

            cout << "actions\n";
            auto parentProb = probMap[*vit];

            size_t tne = 0;
            // The total # of derivations of this vertex (over all
            // considered score classes)
            cl_I total(0);
            for ( size_t i = 0; i < countDict[*vit].size(); ++i ) { total += get<1>(countDict[*vit][i]); }

            // Compute the weights for each of the score classes
            // considered at this node
            auto alphas = computeAlphasDouble( countDict[*vit], k, total );


            // for each top-k score class
            for ( size_t i = 0; i < countDict[*vit].size(); ++i ) {
                double pScore; cl_I pCount;
                // The score and it's count
                tie(pScore, pCount) = countDict[*vit][i];

                cout << "score class " << i << "\n";

                double p = 0.0;
                // for all incoming edges contributing to this score
                for ( const auto& e : usedEdges[*vit][pScore] ) {
                    // The conditional probability of taking edge 'e'
                    // given than we're deriving vertex *vit at score 'pScore'
                    auto condProb = edgeProbMap[e][pScore];
                    auto prob = ( parentProb * (alphas[i] * condProb));
                    auto tail = H->getTail(e);
                    cout << "Action = " << flipType( key, H->vertex(tail[0])) << ", prob = " << prob << "\n";
                    p += prob;
                }
                tne += usedEdges[*vit][pScore].size();
                cout << "score = " << pScore << ", numEdges =  " << usedEdges[*vit][pScore].size() << ", prob = " << p << "\n";
            }

            cout << "total number of incoming edges = " << H->incident(*vit).size() << ", total num used edges = " << tne << ", ";
            cout << "outProb = " << approxProb << "\n\n";
            */
            // ====================
            var writeOut = true
            if ( restrictOutput ) { writeOut = outputKeys contains key }

            if ( approxProb > 0.0 && writeOut ) {
                val fs = flipStrMap( (key.f, key.r) )
                if ( fs != "n" ) {
                    out.write( key.u.getName+"\t"+key.v.getName+"\t"+fs+"\t"+approxProb+"\n" )
                }
            }
        }
        
        println()

    }
}
}

