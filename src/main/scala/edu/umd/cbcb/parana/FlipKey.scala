package edu.umd.cbcb.parana

object FlipKey{

  def flipBoth[N <: Comparable[N]]( k: FlipKey[N] ) = { FlipKey(k.u, k.v, !k.f, !k.r) }
  def flipForward[N <: Comparable[N]]( k: FlipKey[N] ) = { FlipKey(k.u, k.v, !k.f, k.r) }
  def flipReverse[N <: Comparable[N]]( k: FlipKey[N] ) = { FlipKey(k.u, k.v, k.f, !k.r) }
}

case class FlipKey[N <: Comparable[N]]( var u: N, var v: N, var f: Boolean, var r: Boolean ) {
  // See if there is a better way to achieve what we want aside from 
  // making the member mutable and putting this logic here
  if ( u.compareTo(v) > 0 ) { val tmp = v; v = u; u = tmp }

  def arity() = if( u == v ) { 1 } else { 2 } 
}
