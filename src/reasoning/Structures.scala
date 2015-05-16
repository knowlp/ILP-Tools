/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.collection.mutable.ListBuffer

object Structures {

   sealed trait Term

   /*
 * A variable is any identifier consisting of letters, digits and underscore, starting with an upper-case letter
 */
   case class Variable(name: String) extends Term

   /*
 * A variable is any identifier consisting of letters, digits and underscore, starting with an lower-case letter
 */
   case class Constant(name: String) extends Term

   /*/
 * An positive literal is a non-negated compound term
 */
   case class PosLiteral(functor: String, terms: List[Term], isNAF: Boolean = false) extends Term {
      val arity = terms.length
   }

   /*
 * A literal is either positive or negative
 */
   case class Literal(functor: String, terms: List[Term], isNAF: Boolean) extends Term {
      val arity = terms.length
   }

   /*
 * A Clause is an expression of the form a :- X, where a is an atom and X is a
 * comma-separated (denoting logical disjunction) of literals
 */
   case class Clause(head: PosLiteral, body: List[Literal]) extends Term
   
   
   

   case class ModeAtom(functor: String, args: List[ModeAtom]) extends Term {
      val tostring: String = args match {
         case List() => functor
         case _ => functor + "(" + (for (a <- args) yield a.tostring).mkString(",") + ")"
      }
      val kind: String = functor.toSeq match {
         case Seq('+', _@ _*) => "+".mkString
         case Seq('-', _@ _*) => "-".mkString
         case Seq('#', _@ _*) => "#".mkString
         case _ => "compound"
      }
      val ttype: String = functor.toSeq match {
         case Seq('+', x @ _*) => x.mkString
         case Seq('-', x @ _*) => x.mkString
         case Seq('#', x @ _*) => x.mkString
         case _ => ""
      }
      val isPosPlmrk: Boolean = if (kind == "+") true else false
      val isNegPlmrk: Boolean = if (kind == "-") true else false
      val isConstPlmrk: Boolean = if (kind == "#") true else false
      val isPlmrk = isPosPlmrk | isConstPlmrk | isNegPlmrk

      def varbed(): Tuple2[String,List[String]] = {
         val p = """(\+|\-|\#)([A-Za-z0-9_]*)""".r
         val plmrks = p.findAllIn(this.tostring)
         var i = 0
         var typePreds = new ListBuffer[String]()
         var temp = this.tostring
         for (x <- plmrks) {
            val ttype = x.toSeq match {
               case Seq('+', t @ _*) => t.mkString
               case Seq('-', t @ _*) => t.mkString
               case Seq('#', t @ _*) => t.mkString
            }
            val xx = "X" + i
            val typePred = s"$ttype($xx)"
            temp = temp.replaceFirst("\\+" + ttype, xx)
            typePreds += s"$ttype($xx)"
            i = i + 1
         }
         (temp,typePreds.toList)
      }
      
      /*
       * TODO: I need to implement the recursive version at some point
       * 
      def variabilize(currentMAtom: ModeAtom, varCounter: Int): Any = {
         
         for (x <- args){
            if (x.isPlmrk){
               val newMAtom = new ModeAtom(currentMAtom.functor,currentMAtom.args :+ new ModeAtom("X"+varCounter,List()))
               variabilize(newMAtom,varCounter)
            }else{ // We have a compound inner term
               variabilize(x,varCounter+1)
            }
         }
         currentMAtom
         //val varbed: List[Any] = (for (x <- args) yield { 
         //   if (x.isPlmrk) new ModeAtom("X"+varCounter, List()) ;  else x.variabilize(varCounter+1) })
         //val v = new ModeAtom(functor,varbed.asInstanceOf[List[ModeAtom]])
         //v.tostring
            
      }
      */
      
   }

}


