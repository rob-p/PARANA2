import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen._
import edu.umd.cbcb.parana.io._

class NewickParserSpec extends FeatureSpec {

  feature("The parser handles simple trees") {
    scenario("Single node tree") {
      
      given("""A tree with a single node 'node;'""")
      val s = """node;"""
      
      when("The parser is invoked")
      val p = new NewickParser
      val r = p.parseAll( p.forest, s )
      
      then("The result should be a single node named \'node\'")
      assert( r.get(0)._name === "node" )

      and("it should have no children")
      assert( r.get(0)._children.isEmpty )
    }
    
    scenario("1 level binary tree") {

      given("""A single level binary tree '(a,b)root;'""")
      val s = """(a,b)root;"""

      when("The parser is invoiked")
      val p = new NewickParser
      val r = p.parseAll( p.forest, s )

      then("""The result should be a tree with root 'root'""")
      assert( r.get(0)._name === "root" )

      and("""and children 'a' and 'b'""")
      assert( r.get(0)._children.get(0)._name === "a")
      assert( r.get(0)._children.get(1)._name === "b")
      
    }
  }


}
