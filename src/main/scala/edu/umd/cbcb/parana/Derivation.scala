package edu.umd.cbcb.parana

import scala.collection.mutable.{ ArrayBuffer, HashSet }
import org.forester.phylogeny.{ Phylogeny, PhylogenyNode }

case class FlipT( u: PhylogenyNode, v: PhylogenyNode, t: String) 

case class Derivation(
    cost: Double,
    target: Int,
    bp: ArrayBuffer[Int],
    flips: HashSet[FlipT]
) 