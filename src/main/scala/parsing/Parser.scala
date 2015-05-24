/**
 * @author Nikos Katzouris
 *
 */

package parsing

import scala.util.parsing.combinator.JavaTokenParsers

import reasoning.Exceptions.MyParsingException
import reasoning.Structures._


object LogicParser {

   /**
    * ------------------------------------
    * Parses expressions in clausal logic
    * ------------------------------------
    */

   class ClausalLogicParser extends JavaTokenParsers {
      def lowerCaseIdent: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r;
      def upperCaseIdent: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r;
      def anyWord: Parser[String] = """[A-Za-z0-9_]*""".r ^^ { x => x }
      def naf: Parser[String] = "not " ~ rep("\\s+") ^^ { _ => "not" };
      def iff: Parser[String] = rep("\\s+") ~ ":-" ~ rep("\\s+") ^^ { _ => ":-" }
      def number: Parser[String] = floatingPointNumber;
      def variable: Parser[Expression] = upperCaseIdent ^^ { x => Variable(x) };
      def constant: Parser[Expression] = lowerCaseIdent ^^ { x => Constant(x) } | number ^^ { x => Constant(x) };
      def term: Parser[Expression] = (literal | variable | constant);
      def innerTerms: Parser[List[Expression]] = "(" ~> repsep(term, ",") <~ ")";
      def literal: Parser[Literal] = (
         naf ~ lowerCaseIdent ~ innerTerms ^^ { case naf ~ functor ~ inner => Literal(functor, inner, true) }
         | lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(functor, inner, false) });
      def atom: Parser[PosLiteral] = lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => PosLiteral(functor, inner) }
      def clauseHead: Parser[PosLiteral] = atom
      def clauseBody: Parser[List[Literal]] = repsep(literal, ",")
      def clause: Parser[Clause] = clauseHead ~ iff ~ clauseBody ^^ { case head ~ iff ~ body => Clause(head, body) }


      
      
      def parseOutput(parser: Parser[Expression], expression: String): Expression = {
         getParseResult(parse(parser,expression))
      }
      
      def parse(parser: Parser[Expression], expression: String): Option[Expression] = {
         parseAll(parser, expression) match {
            case Success(result, _) => Some(result)
            case f => None
         }
      }

      def getParseResult(x: Option[Expression]): Expression = x match {
         case Some(y) => y
         case _ => throw new MyParsingException
      }

   }

   /**
    * -------------------------
    * Parses mode declarations
    * -------------------------
    */

   class ModesParser extends ClausalLogicParser {
      def mh: Parser[String] = "modeh" ^^ { x => x }
      def mb: Parser[String] = "modeb" ^^ { x => x }
      def ep: Parser[String] = "examplePattern" ^^ { x => x }
      def posplmrk: Parser[PosPlmrk] = "+"~lowerCaseIdent ^^ {case "+"~x => PosPlmrk(x)}
      def negplmrk: Parser[NegPlmrk] = "-"~lowerCaseIdent ^^ {case "-"~x => NegPlmrk(x)}
      def constplmrk: Parser[ConstPlmrk] = "#"~lowerCaseIdent ^^ {case "#"~x => ConstPlmrk(x) }
      def placemarker: Parser[Expression] = (posplmrk | negplmrk | constplmrk) ^^ {x => x}
      def inner: Parser[List[Expression]] = "(" ~> repsep((modeAtom | placemarker), ",") <~ ")";
      def modeAtom: Parser[ModeAtom] = lowerCaseIdent ~ inner ^^ { case x ~ y => new ModeAtom(x.toString, y) }
      def modeh: Parser[ModeAtom] = mh ~ "(" ~ modeAtom ~ ")" ^^ { case mh ~ "(" ~ m ~ ")" => m }
      def modeb: Parser[ModeAtom] = mb ~ "(" ~ modeAtom ~ ")" ^^ { case mb ~ "(" ~ m ~ ")" => m }
      def mode: Parser[ModeAtom] = (modeh | modeb)
      def exmplPattern: Parser[ModeAtom] = ep ~ "(" ~ modeAtom ~ ")" ^^ { case ep ~ "(" ~ m ~ ")" => m }

      def parseModes(parser: Parser[ModeAtom], expression: String): Option[ModeAtom] = {
         parseAll(parser, expression) match {
            case Success(x, _) => Some(x)
            case Failure(msg, _) =>
               println("FAILURE: " + msg); None
            case Error(msg, _) => println("ERROR: " + msg); None
            //case _ => None
         }
      }

      def getParseResult(x: Option[ModeAtom]): ModeAtom = x match {
         case Some(y) => y
         case _ => throw new MyParsingException
      }
   }

   /**
    * ----------------------------------------------------------------
    * Parses models (solutions) from the ASP solver (Clingo or Clasp)
    * ----------------------------------------------------------------
    */

   class ASPResultsParser extends ClausalLogicParser {
      //def result: Parser[Any] = rep((literal | "\\s+" | ".")) ^^ { case x => x }

      def aspResult: Parser[List[String]] = repsep(literal, "") ^^ { case x => for (y <- x) yield y.tostring }

      def parseASP(parser: Parser[Any], expression: String): Option[Any] = {
         parseAll(parser, expression) match {
            case Success(result, _) => Some(result)
            case Failure(msg, _) =>
               println("FAILURE: " + msg); None
            case Error(msg, _) => println("ERROR: " + msg); None
         }
      }

      def parsed(x: Option[Any]): Boolean = x match {
         case Some(y) => true
         case _ => false
      }

      def getResult(x: Option[Any]): Any = x match {
         case Some(y) => y
         case _ => false
      }

   }

   /**
    * --------------------------------------------------------------------------------------------------------
    * Parser for CAVIAR data. This is not used in the project. Caviar data are handled via regular expressions
    * --------------------------------------------------------------------------------------------------------
    */

   class CaviarDataParser extends JavaTokenParsers {

      def strLiteral: Parser[String] = """[a-zA-Z0-9_]*""".r;
      def person: Parser[String] = """id[0-9]""".r
      def num: Parser[String] = """[0-9]*""".r;

      /*Parses an HLE event. Format:
   * happensAt( moving( grp_ID0, [ id4, id5 ]), 2520)
   * 
   * Turn this into:
   * holdsAt(moving(id4,id5),2520)
   */
      def hle: Parser[String] = "happensAt" ~ "(" ~ strLiteral ~ "(" ~ strLiteral ~ "," ~ "[" ~ person ~ "," ~ person ~ "]" ~ ")" ~ "," ~ num ~ ")" ^^
         { case "happensAt" ~ "(" ~ hlevent ~ "(" ~ _ ~ "," ~ "[" ~ p1 ~ "," ~ p2 ~ "]" ~ ")" ~ "," ~ time ~ ")" => s"holdsAt($hlevent($p1,$p2),$time)" }

      /*
   * Parses coordinates atoms. Format:
   * holdsAt( coord( id0 )=( 262, 285 ), 680 )
   * 
   * Turn this into:
   * coords(id0,262,285,680)
   * 
   */
      def coords: Parser[String] = "holdsAt" ~ "(" ~ "coord" ~ "(" ~ person ~ ")" ~ "=" ~ "(" ~ num ~ "," ~ num ~ ")" ~ "," ~ num ~ ")" ^^
         { case "holdsAt" ~ "(" ~ "coord" ~ "(" ~ p ~ ")" ~ "=" ~ "(" ~ x ~ "," ~ y ~ ")" ~ "," ~ time ~ ")" => s"coords($p,$x,$y,$time)" }

      /*
   * Parses LLEs. Format:
   * happensAt( walking( id0 ), 680 )
   */

      def lle: Parser[String] = "happensAt" ~ "(" ~ strLiteral ~ "(" ~ person ~ ")" ~ "," ~ num ~ ")" ^^ { case "happensAt" ~ "(" ~ lle ~ "(" ~ p ~ ")" ~ "," ~ time ~ ")" => s"happensAt($lle($p),$time)" }

      /*
     * Parses appearance attoms. Format:
     * holdsAt( appearance( id0 )=appear,  680 )
     * Turn that into:
     * if "appear" then "happensAt(enters(id0),680)" else if "visible" then "holdsAt(visible(id0),680)" else (if disappear) "happensAt(exits(id0),680)" 
     */

      def visibility: Parser[String] = "holdsAt" ~ "(" ~ "appearance" ~ "(" ~ person ~ ")" ~ "=" ~ strLiteral ~ "," ~ num ~ ")" ^^
         {
            case "holdsAt" ~ "(" ~ "appearance" ~ "(" ~ p ~ ")" ~ "=" ~ what ~ "," ~ time ~ ")" => what match {
               case "appear" => s"happensAt(enters($p),$time)"
               case "disappear" => s"happensAt(exits($p),$time)"
               case "visible" => s"holdsAt(visible($p),$time)"
            }
         }

      /*
     * Parses orientation atoms. Format:
     * holdsAt( orientation( id0 )=19, 7160  )
     * Turn that into:
     * orientation(id0,19,7160)
     */
      def orientation: Parser[String] = "holdsAt" ~ "(" ~ "orientation" ~ "(" ~ person ~ ")" ~ "=" ~ num ~ "," ~ num ~ ")" ^^
         { case "holdsAt" ~ "(" ~ "orientation" ~ "(" ~ p ~ ")" ~ "=" ~ o ~ "," ~ time ~ ")" => s"orientation($p,$o,$time)" }

   }
}


