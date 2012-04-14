package edu.umd.cbcb.parana

import scala.collection.mutable.{ OpenHashMap => MHashMap }
import scala.collection.immutable.{ HashSet }
import scala.collection.JavaConversions._

import org.forester.phylogeny.{ Phylogeny, PhylogenyMethods, PhylogenyNode }

object PhylogenyUtils {

	object TreeInfo {
		def empty() = { new TreeInfo }
	}

	class TreeInfo() {
	 	var enets = MHashMap.empty[PhylogenyNode, HashSet[String]]
	 	var leaves = MHashMap.empty[PhylogenyNode, HashSet[PhylogenyNode]]
	 	var subnodes = MHashMap.empty[PhylogenyNode, HashSet[PhylogenyNode]]
	}	

	def augmentTree( t: Phylogeny ) = {

		def extantNetworks( n: PhylogenyNode ) = {
			val nodeName = n.getName
			// If this node was lost
			if ( nodeName contains "LOST" ) {  
				HashSet("LOST")	
			} else {
				HashSet( PhylogenyMethods.getSpecies(n) )
			}
		}



		val treeInfo = TreeInfo.empty
		val treeIt = t.iteratorPostorder
		while ( treeIt.hasNext ) {
			val n = treeIt.next
			n.isInternal match {
				// Internal node
				case true => {
					val children = n.getDescendants
					// The subnodes of this internal node is the union of the child subnodes
					treeInfo.subnodes(n) = HashSet(n)
					treeInfo.leaves(n) = HashSet.empty[PhylogenyNode]
					treeInfo.enets(n) = HashSet.empty[String]
					children.foreach{ c => 
						treeInfo.subnodes(n) ++= treeInfo.subnodes(c)
						treeInfo.leaves(n) ++= treeInfo.leaves(c)
						treeInfo.enets(n) ++= treeInfo.enets(c) 
					}
				}
				// Leaf node
				case false => {
					treeInfo.enets(n) = extantNetworks(n)
					treeInfo.leaves(n) = HashSet(n)
					treeInfo.subnodes(n) = HashSet(n)
				}
			}
		}
		treeInfo.enets.foreach{ case (n,en) => println("N: "+n.getName+" enets: "+ en.mkString(", "))}
		treeInfo
	}




}