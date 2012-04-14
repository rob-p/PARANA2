package edu.umd.cbcb.parana

import scala.io._
import scala.collection.mutable._
import scala.collection.JavaConversions._

import java.{ io => jio }
import java.io.{ FileWriter => JFileWriter }

import scopt._

import scalax.collection.Graph
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._
import scalax.collection.edge.{WDiEdge, WUnDiEdge}

import net.robpatro.utils.time.Timer
import net.robpatro.utils.console.ProgressBar

import edu.umd.cbcb.parana.io.{ GMLReader }
import edu.umd.cbcb.parana.graphtools.SimpleNode
import edu.umd.cbcb.parana.treetools.Util._
import edu.umd.cbcb.parana.treetools.TreeNode
import edu.umd.cbcb.parana.io._

import org.forester.io.parsers.util.ParserUtils
import org.forester.io.parsers.PhylogenyParser
import org.forester.phylogeny.{ PhylogenyMethods, PhylogenyNode }

import scalaa.datastruct._

object ReconstructHistory {

  class Config {
    var dupHistFileName : String = ""
    var targetFileName : String = ""
    var outputFileName : String = ""
    var undirected = true
  }

  def main(args: Array[String]) {
    var config = new Config

    /**
     * Configuration Options
     */
    val parser = new OptionParser("ReconstructHistory") {
      opt("t", "target", "<file>", "output is a string property", {v: String => config.targetFileName = v})
      opt("d", "duplication", "<file>", "output is a string property", {v: String => config.dupHistFileName = v})
      opt("o", "output", "<file>", "output is a string property", {v: String => config.outputFileName = v})
      booleanOpt("u", "undirected", "<value>", "extant network is undirected", {v: Boolean => config.undirected = v})
    }

    // Create the command line parser
    if ( parser.parse(args) ) {

      // Read the duplication history
      val dupFile = new jio.File(config.dupHistFileName)
      val newickParser = ParserUtils.createParserDependingOnFileType( dupFile, true )
      val dupHists = PhylogenyMethods.readPhylogenies( newickParser, dupFile )
      // We need at least one duplictation history
      assert( dupHists.size > 0 )
      val dupHist = dupHists(0)

      /** UGH: Because our phylogenetic library overrides the equals method
      * to be based on the node name, and our nodes don't have unique names 
      * (e.g. lost nodes), rename any lost nodes by appending their nodeID to
      * their names.
      */
      val nit = dupHist.iteratorPreorder
      while( nit.hasNext ){
        val n = nit.next
        if( n.getName contains "LOST" ) { n.setName( n.getName+n.getId )}
      }
      
      // Compute the auxiliary tree information
      val treeInfo = PhylogenyUtils.augmentTree( dupHist )

      val directed = !config.undirected
      // Read in the extant edges
      val g = config.targetFileName match {
        case fn: String if (fn.endsWith(".adj")) => { AdjListReader.fromFile( fn, directed ) }//new AdjListReader[String, WUnDiEdge[String]]( fn, edgeFact[String] ).parse.get }
        case fn: String if (fn.endsWith(".madj")) => { MultiAdjListReader.fromFile( fn, directed, weighted=true )}
        case fn => { scala.sys.error("Don't recognize type of file ["+fn+"]") }
      }
      println("G.size = "+g.size+", G.order = "+g.order)
      /*print("EDGES = {")
      g.edges.foreach{ case (u,v) => print("("+u+","+v+"):"+g.getWeight(u,v)+", ") }
      print("}")
      */

      val (cc, dc, tpen) = (1.0, 1.0, 1.0)
      def time[T](code : => T) =  {
        val t0 = System.nanoTime : Double
        val res = code
        val t1 = System.nanoTime : Double
        println("Elapsed time " + (t1 - t0) / 1000000.0 + " msecs")
        res
      }
      val H = time(SolutionEnsemble.buildSolutionSpaceGraph( dupHist, treeInfo, cc, dc, tpen, directed ))


      val order = SolutionEnsemble.topologicalOrder( H )
      
      val slnDict = new SolutionEnsemble.SlnDictT
      SolutionEnsemble.leafCostDict( H, dupHist, g, directed, cc, dc, slnDict)

      val rootKey = FlipKey( dupHist.getRoot, dupHist.getRoot, false, false )
      val rootInd = H.index(rootKey)

      // Count the # of opt slns.
      val countDict = new SolutionEnsemble.CountDictT
      val k = 10      
      val keyList = ArrayBuffer.empty[FlipKey[PhylogenyNode]]
      SolutionEnsemble.upDown(H, dupHist, treeInfo, tpen, order, slnDict, countDict, k, config.outputFileName, keyList)

      /*
      //slnDict.set_empty_key( std::numeric_limits<size_t>::max() );
      if ( undirected ) {
          MultiOpt::leafCostDict( H, tree, get<undirectedGraphT>(G), directed, creationCost, deletionCost, slnDict);
      } else {
          MultiOpt::leafCostDict( H, tree, get<directedGraphT>(G), directed, creationCost, deletionCost, slnDict);
      }
      */
      /*println("EDGES: "+g.edges.mkString(", "))*/
      /*
      // Factories for creating nodes as well as directed and undirected edges
      def treeNodeFactory( n : String, i : Int ) : TreeNode =  new TreeNode(n,i)
      def simpleNodeFactory( n : String, i : Int ) : SimpleNode = new SimpleNode(n,i)
      def unDirEdgeFactory( s : SimpleNode, t : SimpleNode )  = { s ~ t }
      def dirEdgeFactory( s : SimpleNode, t : SimpleNode )  = { s ~> t }

      // The type of edge factory we need is dependent on the undirected input flag
      def edgeFact( s:SimpleNode, t: SimpleNode ) =
        config.undirected match {
          case true => unDirEdgeFactory(s,t)
          case false => dirEdgeFactory(s,t)
        }

      // read the duplication history
      val dupFile = new File(config.dupHistFileName)
      val newickParser = ParserUtils.createParserDependingOnFileType( dupFile, true )
      val dupHists = PhylogenyMethods.readPhylogenies( newickParser, dupFile )
      assert( dupHist.size > 0 )
      val dupHist = dupHists.get(0)
      
      // create a map from the name of a node to the node
      val nameNodeMap = dupHist.nodes.map{ n => (n.name,n) }.toMap

      // Read in the extant graphs from the adjacency list
      type TreeType = Graph[TreeNode, WDiEdge]
      val g = new ADJListReader2(config.targetFileName, simpleNodeFactory, edgeFact ).parse.get
      val rv = getRoot[TreeNode, TreeType](dupHist)
      val lostNodes = dupHist.nodes.filter{ n => n.name contains "LOST" }
      val extantNodes = dupHist.nodes.filter{ n => n.diSuccessors.size == 0 }


      extantNodes.foreach{ n => println(n) }

      val isolatedNodes = ( extantNodes.map{ n => n.name } &~ g.nodes.map{ n => n.name } ).map{ n => simpleNodeFactory(n,0) }
      isolatedNodes.foreach{ n => g += n }

      /*
      # Add back any isolated extant nodes (nodes which have no interactions) to the extant network
      isolatedNodes = extantNodes - set(G.nodes())
      if len(isolatedNodes) > 0:
        logging.info( "Adding isolated extant nodes {0} to the extant network".format( isolatedNodes ) )
        G.add_nodes_from( isolatedNodes )
      */

      augmentTreeNodes[ TreeNode, TreeType, collection.Set[dupHist.NodeT] ](dupHist, rv, lostNodes)

      dupHist.nodes.foreach{ n => println("Leaves below "+n.name+" = "+n.leaves.map{ sn => sn.name } ) }
      dupHist.nodes.foreach{ n => println("Nodes below "+n.name+" = "+n.subnodes.map{ sn => sn.name } ) }
      println(dupHist.nodes.size)

      println("Extant Nodes : "+extantNodes.map{ v => v.name })

      val extantNames = extantNodes.map{ x => x.name }.toSet

      println(dupHist.nodes)
      println(g.nodes.map{ n => n.name})
      println("Root of the tree is "+rv)
      val dstr = g.edges.head.undirected match {
        case true => "undirected"
        case false => "directed"
        case _ => "ERROR"
      }
      println("The network is "+dstr)
      */
      System.exit(0)
    }

  }
}
