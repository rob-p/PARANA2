package edu.umd.cbcb.parana

import scala.collection.mutable.{ArrayBuffer, HashSet}

import com.google.common.collect.HashBiMap

case class ForwardHyperedge ( val tail: Array[Int], val head: Int, val weight: Double ) {
  override def toString() = {
    "[Head : "+head+" Tail : {"+ tail.mkString(",")+"}";
  }
}

class ForwardHypergraph[N <: Comparable[N]]{
  type EdgeSet = HashSet[Int]
  val biMap: HashBiMap[Int, FlipKey[N]] = HashBiMap.create()
  val edgeList = ArrayBuffer.empty[ ForwardHyperedge ]
  val vertices = ArrayBuffer.empty[ EdgeSet ]

  def addVertex( k: FlipKey[N] ): Boolean = {
    if (!biMap.containsValue(k)) {
      val vertId = vertices.size
      biMap.put(vertId, k)
      vertices += new EdgeSet
      true
    } else {
      false
    }
  }

  def addEdge( tail: Array[FlipKey[N]], head: FlipKey[N], weight: Double ): Boolean =  {
    val headIdx = biMap.inverse.get(head)
    val tailIdxs = tail.map{ t => biMap.inverse.get(t) }
    /*ArrayBuffer.empty[Int]//.fill(tail.size)(-1)
    tail.foreach{ t => //zipWithIndex.view.foreach{ case (t,i) =>
      val v = biMap.inverse.get(t)
      tailIdxs += v //(i) = v
    }*/

    val edge = ForwardHyperedge( tailIdxs, headIdx, weight )
    val edgeIdx = edgeList.size
    edgeList += edge
    vertices(headIdx) += edgeIdx
    true
  }

  def getHead( eIdx: Int ) = { edgeList(eIdx).head }
  def getTail( eIdx: Int ) = { edgeList(eIdx).tail }
  def vertex( vIdx: Int ) = { biMap.get(vIdx) }
  def edge( eIdx: Int ) = { edgeList(eIdx) }
  def index( k: FlipKey[N] ) = { biMap.inverse().get(k) }
  def order() = { biMap.size }
  def size() =  { edgeList.size }

  def incident( hIdx: Int ) = {
    assert( hIdx < vertices.size )
    vertices(hIdx)
  }
  //def incident( k: FlipKey[N] ): EdgeSet = { incident( index(k) ) }
}
