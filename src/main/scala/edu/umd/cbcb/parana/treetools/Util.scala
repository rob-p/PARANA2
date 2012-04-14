package edu.umd.cbcb.parana.treetools


import edu.umd.cbcb.parana.graphtools.SimpleNode
import scala.collection.JavaConversions._

import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WDiEdge

import scala.collection.mutable.{ListBuffer, Set => MSet}

object Util {

  /**
   * Performs a post order traversal of the binary tree, and returns a lazily evaluated stream of the
   * visited nodes
   * @param tree The binary tree to traverse
   * @param node The root node of the the tree
   */
  def postOrderStream[NT, GT <: Graph[NT, WDiEdge] ]( tree: GT, node: NT ): Stream[NT] = {
    val cset = children(node, tree).toList
    cset.foreach{ c => println(c) }
    if (cset.size > 0) {
       postOrderStream(tree, cset(0)) ++ postOrderStream(tree, cset(1)) ++ Stream(node)
    } else {
      Stream(node)
    }
  }


  /**
   * Augments the nodes of the provided tree with extra information.  Specifically, each node is augmented
   * with:
   *   $ 1 The set of leaves that reside beneath it
   *   $ 2 The set of all nodes in its subtree
   *   $ 3 The set of extant networks spanned by its leaves
   */
  def augmentTreeNodes[ NT <: TreeNode, GT <: Graph[NT, WDiEdge], NST ]( graph: GT, root: NT, lostNodes: NST ) = {

    val lost = "LOST"

      /*
       Compute some important sets of nodes from the extant network and the tree
       specifically, the set of lost and extant nodes
      leaves = set( [n for n in T.nodes() if len(T.successors(n)) == 0] )
      lostNodes = set( filter( lambda x: x.find('LOST') != -1, leaves ) )
      extantNodes = leaves - lostNodes
      */
    def extantNetworks( nodes:MSet[TreeNode] ): MSet[String] = {
      val lostNodes = MSet.empty[TreeNode]
      val r = MSet.empty[String]
      for (n <- nodes) {
	if( !(lostNodes contains n) ) {
	  val name = n.name
	  r += name.substring( name.size-2, name.size )
	} else {
	  r += lost
	}
      }

      r
    }

    val pon = postOrderStream(graph, root)
    pon.foreach{ v =>
      val cset = children(v, graph)
       if (cset.size == 0) {
	 v.leaves.add(v); v.subnodes.add(v); v.enets = extantNetworks( v.leaves )
       } else {
	 v.subnodes = cset.map{ c => c.subnodes }.reduce{ (ls,rs) => ls | rs }
	 v.subnodes += v
	 v.leaves = cset.map{ c => c.leaves }.reduce{ (ls,rs) => ls | rs }
	 v.enets = extantNetworks( v.leaves )
       }

    }
  }

  def children[NT, GT <: Graph[NT,WDiEdge]]( node: NT, g: GT) : scala.collection.Set[NT]= {
    val n = g.get(node)
    println(n)
    n.diSuccessors.map{ v => v.value }
  }

  def getRoot[NT, GT <: Graph[NT, WDiEdge] ]( tree : GT ) : NT = {
    var svert = tree.nodes.head
    var parents = svert.diPredecessors
    while ( parents.size > 0 ) {
      svert = parents.head // tree.getEdgeSource(inEdges.iterator.next)
      parents = svert.diPredecessors // tree.incomingEdgesOf(svert)
    }
    svert
  }

  //Are the descendants of u and v strictly in different extant networks?
  def differentExtantNetworks[ NT <: TreeNode ]( u: NT, v: NT ) = { (u != v) && ( ( u.enets & v.enets ).size == 0 ) }

}
