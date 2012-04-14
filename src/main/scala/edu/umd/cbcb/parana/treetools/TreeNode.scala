package edu.umd.cbcb.parana.treetools

import scala.collection.mutable.{ Set => MSet }
import edu.umd.cbcb.parana.graphtools.{ SimpleNode }

class TreeNode( _name: String, _id: Int ) extends SimpleNode( _name, _id) {
  var leaves = MSet[TreeNode]()
  var subnodes = MSet[TreeNode]()
  var enets = MSet[String]()

  override def hashCode = { name.hashCode + id }

  // Nodes are equal iff their names and id's are equal
  override def equals( other: Any ) = {
    other match {
      case o : TreeNode => o.name == name && o.id == id
      case _ => false
    }
  }

  override def toString : String = {"TreeNode: "+name+", "+id }
}
