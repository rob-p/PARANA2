package edu.umd.cbcb.parana.io

import scala.util.parsing.combinator._

import java.io.{ FileReader => JFileReader }

import scala.collection.JavaConversions._

import scalax.collection.Graph
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._

import edu.umd.cbcb.parana.treetools.TreeNode
import edu.umd.cbcb.parana.treetools.Util._

trait GraphTypes {
  type SDGraph // = Graph[TreeNode, WDiEdge]
  // type SDGraph = DefaultDirectedGraph[TreeNode, DefaultEdge]
}

object ParseNode {

  def buildGraph(n: ParseNode, g: MGraph[TreeNode, WDiEdge]) {
    val tn = new TreeNode( n._name, g.order )
    g += tn
    buildGraphHelper(n, tn, g)
  }

  def buildGraphHelper(n:ParseNode, pn: TreeNode, g: MGraph[TreeNode, WDiEdge]) {
    implicit val f = WDiEdge
    if (n._children.isDefined){
      n._children.get.foreach{
	c => {
	  val cn = new TreeNode( c._name, g.order )
	  g += cn // add the child node
          g += (pn~%>cn)(1) // add the parent -> child edge
	  buildGraphHelper(c, cn, g) // call recursively on the child
        }
      }
    }
  }

}

class ParseNode(
  name:Option[String]=Option.empty[String],
  dist:Option[Double]=Option.empty[Double],
  children:Option[List[ParseNode]] ) {

  var _name = if(name isDefined){ name.get } else { "" }
  var _dist = if(dist isDefined){ dist.get } else { 1.0 }
  var _children = children
}

class NewickParser extends JavaTokenParsers {

  val whitespace : Parser[String] = """\s+""".r
  val cchar : Parser[String]  = """[\S\s&&[^\[\]]]""".r
  val qchar : Parser[String]  = """[\S\s&&[^']]""".r
  val uqchar : Parser[String]  = """[\S&&[^\[\]\(\)',:;]]""".r
  val number : Parser[String]  = """(?:[-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)|[+-]?\d+""".r

  // A forest is one or more trees
  def forest : Parser[List[ParseNode]] = rep(tree) ^^ { case x => x }
  // A tree is defined by it's root node
  def tree : Parser[ParseNode] = node~";" ^^ { case n~";" => n }
  // Nodes have children, labels and distances (some of which are optional)
  def node : Parser[ParseNode] = (
    children~opt(label)~opt(distance) ^^ { case clist~lab~dist => new ParseNode(lab, dist, Some(clist))}
  | opt(children)~label~opt(distance) ^^ { case clist~lab~dist => new ParseNode(Some(lab), dist, clist)}
  )
  // The list of children must have *at least one* child
  def children : Parser[List[ParseNode]] =
    (
      "("~node~rep(","~node)~")" ^^ {
	case "("~x~y~")" => List(x) ++ y.map { sn => sn match { case ","~n => n} }
      }
    )
  // The label can be a quoted or unquoted string
  def label : Parser[String] = (
     quotedList ^^ { case x => "'"+x+"'"}
    |unquotedList ^^ { case x => x }
  )
  // The distance of this branch
  def distance : Parser[Double] = ":"~number~opt(comment) ^^ { case ":"~x~c => x.toDouble }
  // Quoted string
  def quotedList : Parser[String] = "'"~rep(qchar|escapedQuote)~"'" ^^ { case "'"~x~"'" => x.mkString }
  // Escaped Quote in a quoted string
  def escapedQuote : Parser[String] = "''" ^^ { case "''" => "'" }
  // Unquoted string
  def unquotedList : Parser[String] = rep(uqchar|comment) ^^ { case x => x.mkString }
  // Comments
  def comment : Parser[String] = "["~rep(cchar|comment)~"]" ^^ { case x => "" }
}

class NewickReader( filename:String ) {

  def parse : Graph[TreeNode, WDiEdge]= {
    val gparser = new NewickParser
    val res = gparser.parseAll(gparser.forest, new JFileReader(filename))
    val g = MGraph.empty[TreeNode, WDiEdge]
    ParseNode.buildGraph( res.get(0), g )

    // Uniqify names -- Relabel the nodes that have colliding names
    val nameCtr = collection.mutable.Map.empty[String,Int]
    g.nodes.foreach{
      n =>
      val c = nameCtr.getOrElse(n.name, 0)
      nameCtr(n.name) = c+1
      if (c > 0){
        val newName = "%s.%d".format(n.name, c+1)
        val newNode = new TreeNode(newName, n.id)
        val pred = n.diPredecessors
        val succ = n.diSuccessors
        g -= n
        g += newNode
        pred.foreach{ pn => g += (pn.value~%>newNode)(1) }
        succ.foreach{ cn => g += (newNode~%>cn.value)(1) }
      }
    }

    g.nodes.foreach{
      n =>
        println("VALUE : "+n.value)
        println("SUCCESSORS :"+n.diSuccessors)
        val c = children(n.value, g)
        println(c)
    }

    g
  }
}
