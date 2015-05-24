package logicTests
import org.scalatest._
import reasoning.Structures._

class LogicWithFunSuite extends FunSuite {

   
   
   test("Variabilization of a mode declaration should replace all placemarkers with fresh variables") {
      val modehtest1 = "modeh(initiatedAt(moving(+person,+person),+time))"
      val modebtest1 = "modeb(happensAt(walking(+person),+time))"
      val x = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val y = "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"
      val t = new ModesParserTester()
      assert(t.testModes(modehtest1, t.mhParser, t.varbedStr) == "initiatedAt(moving(X0,X1),X2)")
   }

   test("Invoking head on an empty Set should produce NoSuchElementException") {
      intercept[NoSuchElementException] {
         Set.empty.head
      }
   }

}


class ModesParserTester extends parsing.LogicParser.ModesParser {

      val mhParser = modeh
      val mbParser = modeb
      val tostr = (x: ModeAtom) => x.tostring
      val varbed = (x: ModeAtom) => x.varbed
      val varbedStr = (x: ModeAtom) => x.varbed.tostring
      val varr = (x: ModeAtom) => x.varbed

      val x = getParseResult(parseModes(mhParser, "modeh(initiatedAt(moving(+person,+person),+time))"))

      //println(x.varbed().tostring)
      //println(x.varbed().typePreds)

      val y = getParseResult(parseModes(mhParser, "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)),w(q(+a,-b))),+time))"))
      //val y = getParseResult(parseModes(mhParser, "modeh(test(p(+x,+y,q(-x,#z,r(+o,-o)))))"))
      //println(y.tostring)
      //println(y)
      //println(y.varbed().tostring)
      //println(y.varbed().typePreds)      
      /*
       * To use this, simply write similar functions and local values for the parsers you need and pass them.
       * 
       */

      def testModes(s: String, parser: Parser[ModeAtom], f: ModeAtom => Any): Any = {
         val x = getParseResult(parseModes(parser, s))
         f(x)
      }

   }