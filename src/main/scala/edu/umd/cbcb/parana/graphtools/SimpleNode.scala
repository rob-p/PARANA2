package edu.umd.cbcb.parana.graphtools

class SimpleNode( var name: String, var id: Int ){

  override def hashCode = { name.hashCode + id }

  // Nodes are equal iff their names and id's are equal
  override def equals( other: Any ) = {
    other match {
      case o : SimpleNode => o.name == name && o.id == id
      case _ => false
    }
  }

}
