/**
 * @author Nikos Katzouris
 *
 */

package parsing

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator._
import reasoning.Structures._
import reasoning.Exceptions._

object LogicParser {

   class ClausalLogicParser extends JavaTokenParsers {
      def lowerCaseIdent: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r;
      def upperCaseIdent: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r;
      def anyWord: Parser[String] = """[A-Za-z0-9_]*""".r ^^ { x => x }
      def naf: Parser[String] = "not " ~ rep("\\s+") ^^ { _ => "not" };
      def iff: Parser[String] = rep("\\s+") ~ ":-" ~ rep("\\s+") ^^ { _ => ":-" }
      def number: Parser[String] = floatingPointNumber;
      def variable: Parser[Term] = upperCaseIdent ^^ { x => Variable(x) };
      def constant: Parser[Term] = lowerCaseIdent ^^ { x => Constant(x) } | number ^^ { x => Constant(x) };
      def term: Parser[Term] = (literal | variable | constant);
      def innerTerms: Parser[List[Term]] = "(" ~> repsep(term, ",") <~ ")";
      def literal: Parser[Literal] = (
         naf ~ lowerCaseIdent ~ innerTerms ^^ { case naf ~ functor ~ inner => Literal(functor, inner, true) }
         | lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(functor, inner, false) });
      def atom: Parser[PosLiteral] = lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => PosLiteral(functor, inner) }
      def clauseHead: Parser[PosLiteral] = atom
      def clauseBody: Parser[List[Literal]] = repsep(literal, ",")
      def clause: Parser[Clause] = clauseHead ~ iff ~ clauseBody ^^ { case head ~ iff ~ body => Clause(head, body) }

      def parse(parser: Parser[Term], expression: String): Option[Term] = {
         parseAll(parser, expression) match {
            case Success(result, _) => Some(result)
            case f => None
         }
      }

      def getParseResult(x: Option[Term]): Term = x match {
         case Some(y) => y
         case _ => throw new MyParsingException
      }

   }

   class ModesParser extends ClausalLogicParser {


      /* That's clumsy, I need to see if I need a parser for modes and if yes do it right,
     * (return pattern matchable objects etc...) */

      def mh: Parser[String] = "modeh" ^^ { x => x }
      def mb: Parser[String] = "modeb" ^^ { x => x }
      def ep: Parser[String] = "examplePattern" ^^ { x => x }
      def placemarker: Parser[ModeAtom] = ("+" | "-" | "#") ~ anyWord ^^ { 
         case "+"~x => new ModeAtom("+"+x, List())
         case "-"~x => new ModeAtom("-"+x, List())
         case "#"~x => new ModeAtom("#"+x, List())
         }
      def inner: Parser[List[ModeAtom]] = "(" ~> repsep((modeAtom | placemarker), ",") <~ ")";
      def modeAtom: Parser[ModeAtom] = lowerCaseIdent ~ inner ^^ { case x ~ y => new ModeAtom(x.toString, y) }
      def modeh: Parser[ModeAtom] = mh ~ "(" ~ modeAtom ~ ")" ^^ { case mh ~ "(" ~ m ~ ")" => m }
      def modeb: Parser[ModeAtom] = mb ~ "(" ~ modeAtom ~ ")" ^^ { case mb ~ "(" ~ m ~ ")" => m }
      def exmplPattern: Parser[ModeAtom] = ep ~ "(" ~ modeAtom ~ ")" ^^ { case ep ~ "(" ~ m ~ ")" => m }

      
      
      def parseModes(parser: Parser[ModeAtom], expression: String): Option[ModeAtom] = {
         parseAll(parser, expression) match {
            case Success(x, _) => Some(x)
            case _ => None
         }
      }
      def getParseResult(x: Option[ModeAtom]): ModeAtom = x match {
         case Some(y) => y
         case _ => throw new MyParsingException
      }
   }

}


