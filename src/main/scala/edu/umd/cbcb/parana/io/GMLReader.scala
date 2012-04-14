package edu.umd.cbcb.parana.io

import scala.util.parsing.combinator._
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import java.io.{ FileReader => JFileReader }

import edu.umd.cbcb.parana.graphtools.SimpleNode

// Holy crap!  Parser combinators are awesome!
class GMLParser[NodeType] extends JavaTokenParsers {
  val number : Parser[String]  = """(?:[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)|[+-]?\d+""".r

  def graph : Parser[ ( List[(Int,String)], List[(Int, Int)] ) ] = "graph ["~"directed 1"~rep(node)~rep(edge)~"]"  ^^
  {
    case "graph [" ~ "directed 1" ~ nodes ~ edges ~ "]" => (nodes, edges)
  }
  def node : Parser[(Int,String)] = "node ["~id~opt(label)~"]" ^^ { case ns ~ id ~ ol ~ ne => (id,ol.getOrElse("NoName"))}
  def id : Parser[Int] = "id "~wholeNumber ^^ { case "id " ~ num => num.toInt }
  def label : Parser[String] = "label "~stringLiteral ^^ { case "label " ~ lab => lab.slice(1,lab.size-1) }
  def edge : Parser[(Int, Int)] = "edge ["~source~target~opt(weight)~"]" ^^ { case "edge [" ~ s ~ t ~ w ~ "]" => (s,t) }
  def weight : Parser[Double] = "weight"~number ^^ { case "weight"~x => x.toDouble }
  def source : Parser[Int] = "source "~wholeNumber ^^ { case "source " ~ sn => sn.toInt }
  def target : Parser[Int] = "target "~wholeNumber ^^ { case "target " ~ tn => tn.toInt }
}

class GMLReader[NodeType]( filename:String, nodeFact : (String, Int) => NodeType ) {
  //type SDGraph = DefaultDirectedGraph[NodeType, DefaultEdge]
  //val G = new SDGraph( classOf[DefaultEdge] )

  def parse : MGraph[NodeType, DiEdge] = {
    val gparser = new GMLParser
    val res = gparser.parseAll(gparser.graph, new JFileReader(filename))
    val (nlist, elist) = res.get
    val nmap = nlist.map{ kv => (kv._1, nodeFact(kv._2, kv._1) ) }.toMap
    val graph = MGraph.from( nmap.values, elist.map{ e => nmap(e._1)~>nmap(e._2) } )
    graph
  }

}
