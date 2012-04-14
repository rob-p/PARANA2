package edu.umd.cbcb.parana

import edu.umd.cbcb.parana.graphtools.SimpleNode

class EdgeKey[NT <: SimpleNode]( u: NT, v: NT, f: Int, r: Int ) { 
  val edge = if (u.name < v.name) { (u,v) } else { (v,u) }
  var fw = f
  var rev = r
  
  override def hashCode() = (edge._1.name, edge._2.name).hashCode + (fw, rev).hashCode

  override def equals(other:Any):Boolean = other match { 
    case that : EdgeKey[NT] => { 
      isComparable(that) && 
      ( (this.edge._1.name == that.edge._1.name) && (this.edge._2.name == that.edge._2.name) ) &&
      ( this.fw == that.fw && this.rev == that.rev) 
    }
    case _ => false
  }

  def isComparable(that:Any) = that.isInstanceOf[EdgeKey[NT]]
}
