/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.collection.mutable.ListBuffer
import reasoning.Exceptions._
import scala.util.control.Breaks
import scala.util.control._
import com.mongodb.casbah.Imports._

/**
 *
 * Pattern matching with case classes.
 *
 * /* Pattern match on the class */
 * def test(x: Expression) = {
 * x match {
 * case l: Literal => l.skolemize(_, _)
 * }
 * }
 * /* Pattern match on the class and its fields */
 * def test2(x: Expression) = {
 * x match {
 * case l @ Literal(x,y,z) => l.skolemize(_,_)
 * }
 * }
 */

object Structures {

   sealed trait Expression {
      def tostring: String = ""
      def tostringQuote: String = ""
      def _type: String = ""
   }

   /**
    * A variable is any term that starts with an upper-case letter
    *
    * @param name @tparam String the variable symbol
    * @param inOrOutVar @tparam String either '+' or '-' indicating input or output variable, as indicated by mode declarations.
    * The default parameter is an empty string to allow the constructor to work in cases where mode declarations are not present.
    * @param _type @tparam String the (optional) type of the variable
    * @overrides val _type from Expression
    */

   case class Variable(name: String, inOrOutVar: String = "", override val _type: String = "") extends Expression {
      override def tostring = name
      override def tostringQuote = if (inOrOutVar == "-" || inOrOutVar == "#") "\"" + name + "\"" else name
      def asLiteral = Literal(name, List(), false)
   }

   /**
    * A constant is any flat term that starts with a lower-case letter.
    *
    * @param name the constant symbol
    * @param _type the (optional) type of the constant
    * @param plmrk "+", "-", "#" indicating whether this constant corresponds to a input output or ground placemarker.
    * @overrides val _type from Expression
    */

   case class Constant(name: String, plmrk: String = "", override val _type: String = "") extends Expression {
      override def tostring = name
      override def tostringQuote = if (plmrk == "-" || plmrk == "#") "\"" + name + "\"" else name
      def asLiteral = Literal(name, List(), false)
   }

   /**
    * This is a helper class for the representation of non-negated literals.
    */

   case class PosLiteral(functor: String, terms: List[Expression] = Nil, isNAF: Boolean = false,
      modeAtom: ModeAtom = ModeAtom("", Nil), typePreds: List[String] = Nil) extends Expression {
      def arity = terms.length
      def asLiteral = Literal(functor, terms, false, modeAtom, typePreds)
      override def tostring = this.asLiteral.tostring
      override def tostringQuote = this.asLiteral.tostringQuote
   }

   /**
    * A literal is a compound term of the form p(x1,...xn), possibly preceded with 'not' ( 'not p(x1,...xn)' ),
    * in which case it is a negated literal. 'p' is the functor of the literal and xi's are each terms. Each xi
    *  is either a variable, a constant or a non-negated literal.
    *
    * @param functor the predicate/function symbol of the literal.
    * @param terms the inner terms of the literal. This is a var so that it can be updated, by populating the term objects
    * by indicators on whether they correspond to input-output vars or constants, a process that takes place during the
    * construction of the Literal object, by extracting relevant information from the accompanying modeAtom (if one is present
    * with the input). I don't know if this is the best way to do it (having vars), but its seems messy to create a companion object
    * for a case class (as this one).
    * @param isNAF true or false depending on whether the literal is negated or not.
    * @param an (optional) mode declaration pattern. This is pattern according to which the literal has been generated
    * (during bottom clause construction). The mode declaration is used  to annotate the variables and constants of the
    * literal with additional information (types/sorts of constants/variables, input or output variables), which is used in the
    * process of variabilizing the clause in which this literal belongs.
    * @param typePreds an (optional) list of typing predicates, extracted from a matching mode declaration,
    *  for the literal's variables and constants
    *
    */

   case class Literal(functor: String, terms: List[Expression] = Nil, isNAF: Boolean = false,
      modeAtom: ModeAtom = ModeAtom("", Nil), typePreds: List[String] = Nil) extends Expression {

      val arity = terms.length

      /*
       * Helper method for converting a non-negated literal to a PosLiteral object. 
       */
      def asPosLiteral = if (!this.isNAF)
         PosLiteral(this.functor, this.terms, this.isNAF, this.modeAtom, this.typePreds)
      else
         throw new LogicException("Found negated literal casted as postive literal.")

      /**
       * @returns a string representation of the literal (for print-out).
       * @verrides tostring method from Expression Trait.
       */

      override def tostring: String = terms match {
         case List() => functor
         case _ =>
            val prefix = if (isNAF) s"not $functor" else functor;
            prefix + "(" + (for (
               a <- terms; val x = a match {
                  case x: Constant => x
                  case x: Variable => x
                  case x: Literal => x
                  case x: PosLiteral => x
                  case _ => throw new LogicException("Unxpected type of inner term while parsing Literal.")
               }
            ) yield x.tostring).mkString(",") + ")"
      }

      override def tostringQuote: String = terms match {
         case List() => functor
         case _ =>
            val prefix = if (isNAF) s"not $functor" else functor;
            prefix + "(" + (for (
               a <- terms; val x = a match {
                  case x: Constant => x
                  case x: Variable => x
                  case x: Literal => x
                  case x: PosLiteral => x
                  case _ => throw new LogicException("Unxpected type of inner term while parsing Literal.")
               }
            ) yield x.tostringQuote).mkString(",") + ")"
      }

      /**
       * @return a mode declarartion atom that matches this literal.
       * If none is found, returns the empty mode atom ( ModeAtom("",List() )
       */

      def getMatchingMode: ModeAtom = {
         var out: ModeAtom = ModeAtom("", List())
         this.modeAtom match {
            case ModeAtom("", Nil) =>

               val loop = new Breaks;
               loop.breakable {
                  for (x <- Core.modehs ::: Core.modebs) {
                     val test = if (this.functor != x.functor || this.arity != x.arity) false
                     else matchesMode(this.terms zip x.args)
                     //println(x.tostring,test)           
                     if (test) {
                        out = x
                        loop.break()
                     }
                  }
               }
            case _ => this.modeAtom

         }
         out
      }

      def matchesMode(remaining: List[(Expression, Expression)]): Boolean = {
         remaining match {
            case head :: tail => head match {
               case (n: Constant, m: PosPlmrk) => matchesMode(tail)
               case (n: Constant, m: NegPlmrk) => matchesMode(tail)
               case (n: Constant, m: ConstPlmrk) => matchesMode(tail)
               case (n: Variable, m: PosPlmrk) => matchesMode(tail)
               case (n: Variable, m: NegPlmrk) => matchesMode(tail)
               case (n: Variable, m: ConstPlmrk) =>
                  throw new LogicException("Found a variabilized term that corresponds to a grplmrk.")
               case (n: Literal, m: ModeAtom) =>
                  if (n.functor != m.functor || n.arity != m.arity) false else matchesMode(n.terms zip m.args)
               case _ => throw new LogicException("Getting matching mode: Found unexpected term pairing.")
            }
            case Nil => true
         }
      }

      /**
       * Variabilizes a literal. If a matching mode declaration atom is passed with the input, then the literal is variabilzed according
       * to the directives provided by that atom. Else (if no mode atom is present), each constant of the literal is replaced by a new
       * variable (TODO: this is not implemented yet, see comments below). The variabilization of a literal is part of the process of
       * the variabilization of a clause. In this process, constants of the literal that are present in other literals of the clause,
       * which have already been variabilized, should be replaced by the same variable that has already been used for these constants.
       *
       *
       * @param previousMap @tparam scala.collection.mutable.Map[Expression, Expression] a map containing previous bindings of constants to variables.
       * @param accum @tparam List[Literal] an accumulator that collects competed (variabilized) compound sub-terms.
       * @param remaining @tparam List[(Expression, Expression)] a list containing all sub-terms remaining to be variabilized.
       * @param ttypes @tparam List[String] a list collecting typing predicates for the generated variables,
       *  e.g. person(X1), time(X100) etc.
       * @param counter @tparam Int a counter that is incremented by 1 each time a new variable is generated.
       * The name a new variable is simply "X"+currentCounterValue.
       * @Param runningMode @tparam String a flag indicating a "mode" (purpose) for which this method is called. Default is
       * "", in which case the literal is simply variabilized. If mode = "extract-mode-terms", then this method
       * is called on a ground literal and it processes the corresponding mode declaration, extracting a tuple
       * (in,out,grnd) representing the terms of the ground atom that correspond to input, output or ground
       * placemarkers respectively
       */

      def varbed = {
         this.modeAtom match {
            /*
             * 
             * TODO: This should work in any case (exception needed here): Simply, if no mode atom is passed with the input, 
             * replace every constant in the literal. by a fresh variable. So, this should do exactly what modes' variabilizations does.
             * It's easy to do, but I have to think a way to do it without duplicating the code here. One way is to have ModeAtom inherit
             * from Literal and override the variabilization method, but then these classes cannot be case classes and I'll have
             * to write my own apply and unapply methods (I guess thats no big deal, but I don't have the time now).
             *
             */

            case ModeAtom("", Nil) => throw new LogicException("Cannot variabilize a literal without a corresponing mode declaration")
            case _ =>
               val (varbed, ttypes, constVarMap, varCounter) =
                  variabilize(List(Literal(this.functor, List(), this.isNAF)), this.terms zip this.modeAtom.args,
                     scala.collection.mutable.Map[Expression, Expression](), List(), 0)
               val l = Literal(varbed(0).functor, varbed(0).terms,
                  isNAF = false, typePreds = ttypes, modeAtom = this.modeAtom);
               (l, ttypes, constVarMap, varCounter)
         }
      }

      def variabilize(accum: List[Literal], remaining: List[(Expression, Expression)],
         previousMap: scala.collection.mutable.Map[Expression, Expression],
         ttypes: List[String], counter: Int, runningMode: String = ""): (List[Literal], List[String], scala.collection.mutable.Map[Expression, Expression], Int) = {

         // x is a tuple (x1,x2), where x1 is a literal's constant and x2 is it's type as specified by the modeAtom 
         def f(x: (Expression, String), sign: String, tail: List[(Expression, Expression)],
            map: scala.collection.mutable.Map[Expression, Expression]) = {

            val cur = accum match {
               case Nil => Literal(this.functor, List(), this.isNAF)
               case _ => accum.last
            }

            val (litUpdate, typesUpdate, varCountUpdate) = sign match {
               case "#" =>
                  // a term corresponding to constant placemarker remains intact
                  (Literal(cur.functor, cur.terms :+ x._1, cur.isNAF), ttypes, counter)
               case _ =>
                  // if the constant has been variabilized previousely, use the same var.
                  if (map.keySet.contains(x._1)) {
                     (Literal(cur.functor, cur.terms :+ map(x._1), cur.isNAF), ttypes, counter)
                  } else {
                     // else, use a new one
                     val newVar = Variable("X" + counter, "+", x._2)
                     map += (x._1 -> newVar)
                     (Literal(cur.functor, cur.terms :+ newVar, cur.isNAF), ttypes :+ x._2 + "(X" + counter + ")", counter + 1)
                  }
            }
            this.variabilize(accum.tail :+ litUpdate, tail, map, typesUpdate, varCountUpdate)
         }
         remaining match {
            case head :: tail => head match {
               case (x: Constant, y: PosPlmrk) => f((x, y._type), "+", tail, previousMap)
               case (x: Constant, y: NegPlmrk) => f((x, y._type), "-", tail, previousMap)
               case (x: Constant, y: ConstPlmrk) => f((x, y._type), "#", tail, previousMap)
               case (x: Literal, y: ModeAtom) =>
                  val (varbed, newTypes, newMap, newCount) =
                     this.variabilize(List(Literal(x.functor, List(), false)), x.terms zip y.args, previousMap, List(), counter)
                  val pop = accum.last
                  this.variabilize(List(Literal(pop.functor, pop.terms ::: varbed, pop.isNAF)),
                     tail, newMap, ttypes ::: newTypes, newCount)
               case _ => throw new LogicException("Variabilizing Literal " + this.tostring + ": Found unexpected type")
            }
            case Nil =>
               val pop = accum.last
               (accum.tail :+ Literal(pop.functor, pop.terms, pop.isNAF), ttypes, previousMap, counter)
         }
      }

      /**
       * Extracts the terms of the literal marked as input-output or ground terms.
       *
       * @param in @tparam List[Expression] an accumulator for input terms
       * @param out @tparam List[Expression] an accumulator for output terms
       * @param grnd @tparam List[Expression] an accumulator for ground terms
       * @param remaining @tparam List[(Expression, Expression)] the (zipped) terms of the literal and the mode atom
       * that remain to be checked
       *
       * @return a tuple (in,out,ground) carrying the marked terms
       */

      def getPlmrkTerms: (List[Expression], List[Expression], List[Expression]) =
         getPlmrkTerms(List(), List(), List(), this.terms zip this.modeAtom.args)

      def getPlmrkTerms(in: List[Expression], out: List[Expression], grnd: List[Expression],
         remaining: List[(Expression, Expression)]): (List[Expression], List[Expression], List[Expression]) = {
         remaining match {
            case head :: tail => head match {
               case (x: Constant, y: PosPlmrk) =>
                  getPlmrkTerms(in ::: List(Constant(x.name, "+", y._type)), out, grnd, tail)
               case (x: Constant, y: NegPlmrk) =>
                  getPlmrkTerms(in, out ::: List(Constant(x.name, "-", y._type)), grnd, tail)
               case (x: Constant, y: ConstPlmrk) =>
                  getPlmrkTerms(in, out, grnd ::: List(Constant(x.name, "#", y._type)), tail)
               case (x: Variable, y: PosPlmrk) =>
                  getPlmrkTerms(in ::: List(Variable(x.name, "+", y._type)), out, grnd, tail)
               case (x: Variable, y: NegPlmrk) =>
                  getPlmrkTerms(in, out ::: List(Variable(x.name, "-", y._type)), grnd, tail)
               case (x: Variable, y: ConstPlmrk) =>
                  getPlmrkTerms(in, out, grnd ::: List(Variable(x.name, "#", y._type)), tail)
               case (x: Literal, y: ModeAtom) =>
                  val (newin, newout, newconst) =
                     getPlmrkTerms(in, out, grnd, x.terms zip y.args)
                  getPlmrkTerms(newin, newout, newconst, tail)
               case _ => throw new LogicException(this.tostring + ": Unexpected type.")
            }
            case Nil =>
               (in, out, grnd)
         }
      }

      def skolemize(skolems: Map[String, String], accum: ListBuffer[Expression] = ListBuffer[Expression]()): ListBuffer[Expression] = {
         var temp = new ListBuffer[Expression]
         def keyExists = (x: Any) => if (skolems.keySet.exists(_ == x)) true else false
         def append = (x: Expression) => temp += x
         for (x <- this.terms) x match {
            case y: Variable =>
               val name = y.name
               if (keyExists(name))
                  append(Constant(skolems(name)))
               else
                  throw new LogicException("Skolemise: Found a variable without corresponding skolem constant.")
            case y: Constant =>
               val name = y.name
               if (keyExists(name))
                  append(Constant(skolems(name)))
               else
                  throw new LogicException("Skolemise: Found a constant without corresponding skolem constant.")
            case y: Literal =>
               val l = y
               val m = l.skolemize(skolems, temp)
               val toLit = Literal(l.functor, m.toList, l.isNAF)
               temp += toLit
            case _ => throw new LogicException("Skolemise: Unexpected type.")
         }
         temp
      }

      def getSkolemConsts(skolems: ListBuffer[(String, String)], counter: Int): (ListBuffer[(String, String)], Int) = {
         var c = counter; var s = skolems
         def f = (x: String, y: String) => if (!s.contains(x)) s += x -> y else s
         def g = (x: Int) => c += x
         for (x <- this.terms) x match {
            case y: Variable =>
               f(y.name, "skolem" + c); g(1)
            case y: Constant =>
               f(y.name, y.name) // use the constant as a skolem constant
            case y: Literal =>
               val m = y.getSkolemConsts(s, c)
               s = m._1; c = m._2
            case _ => throw new LogicException("Skolemize: Unexpected type of inner term.")
         }
         (s, c)
      }
   }

   case class Clause(head: PosLiteral, body: List[Literal]) extends Expression {

      /**
       * Helper method that converts a clause to a List[Literal] with the head of the clause as the first element.
       */

      def toLiteralList = List(head.asLiteral) ++ (for (x <- body) yield x)

      /**
       * Same as above, but returns a List[String].
       */

      def toStrList: List[String] = List(head.tostring) ++ (for (x <- body) yield x.tostring)

      override def tostringQuote = this.tostring

      /**
       *
       *
       * @returns the string representation of the clause
       * @overrides tostring from Expression.
       *
       */

      override def tostring = this.toStrList match {
         case List() => throw new LogicException("Cannot generate a Clause object for the empty clause")
         case h :: ts =>
            ts.length match {
               case 0 => h + "."
               case 1 => h + " :- \n" + "      " + ts(0) + "."
               case _ => h + " :- \n" + (for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) s"      $x."
               else s"      $x,").mkString("\n")
            }
      }

      def varbed: Clause = {
         var accum = ListBuffer[Literal]()
         var map = scala.collection.mutable.Map[Expression, Expression]()
         var counter = 0
         for (x <- this.toLiteralList) {
            val (a, _, c, d) = x.variabilize(List(Literal(x.functor, List(), x.isNAF)),
               x.terms zip x.modeAtom.args, map, List(), counter)
            val aa = Literal(a(0).functor, a(0).terms, a(0).isNAF, x.modeAtom, a(0).typePreds)
            accum ++= List(aa)
            map ++ c
            counter = d
         }
         val l = accum.toList
         val out = Clause(l.head.asPosLiteral, l.tail)
         out
      }

      /**
       * this theta-subsumes other
       */

      def thetaSubsumes(other: Clause) = {
         val (skolemised, skmap) = other.skolemise
      }

      /**
       * Replaces all variables with a new constant symbol 'skolem0', 'skolem1' etc. Same variables correspond to the
       * same constant symbol. Constants remain intact, i.e. they are used as skolem constants themselves. Example:
       *
       * a(X,Y,Z) :-
       *    p(x,q(Y,const1,2),Z),
       *    not r(A,B,C).
       *
       *    is turned into:
       *
       * a(skolem0,skolem1,skolem2) :-
       *    p(skolem0,q(skolem1,const1,2),skolem2),
       *    not r(skolem3,skolem4,skolem5).
       *
       * Returns the skolemised clause and the 'vars -> skolems' map
       *
       */

      def skolemise: (Clause, Map[String, String]) = {
         val l = this.toLiteralList
         val skmap = this.getSkolemConsts
         var temp = new ListBuffer[Literal]
         for (x <- l) {
            val m = x.skolemize(skmap).toList
            val toLit = Literal(x.functor, m, x.isNAF)
            temp += toLit
         }
         val fl = temp.toList
         val sk = Clause(fl(0).asPosLiteral, (for (x <- fl; if (fl.indexOf(x) != 0)) yield x))
         (sk, skmap)
      }

      /**
       * Generates skolem constants from the variables and the constants of the clause. It returns a map of the form
       * Map('X -> skolem0', 'Y -> skolem1', 'const -> const', .... ) (we use the constants as skolem constants)
       */

      private def getSkolemConsts: Map[String, String] = {
         val l = this.toLiteralList
         print(l)
         var skolems = new ListBuffer[(String, String)]
         var counter = 0
         for (x <- l) {
            val m = x.getSkolemConsts(skolems, counter);
            skolems = m._1; counter = m._2
         }
         println(skolems)
         skolems.toMap
      }

   }

   /**
    * Classes representing a input/output/groung mode declarations placemarkers.
    *
    * @param _type the type/sort of the term, e.g. 'human' in the term '+human'.
    * @overrides val _type from Expression
    */

   case class PosPlmrk(override val _type: String) extends Expression {
      override val tostring = "+" + _type
      override def tostringQuote = this.tostring
   }
   case class NegPlmrk(override val _type: String) extends Expression {
      override val tostring = "-" + _type
      override def tostringQuote = this.tostring
   }
   case class ConstPlmrk(override val _type: String) extends Expression {
      override val tostring = "#" + _type
      override def tostringQuote = this.tostring
   }

   /**
    * A class representing a mode declaration atom.
    * @param functor the outer predicate symbol.
    * @param args a list of the inner terms.
    * @param typePreds an (optional) list of typing predicates for the atom's variables.
    */

   case class ModeAtom(functor: String, args: List[Expression]) extends Expression {

      val arity = this.args.length
      /**
       * @return a string representation of the mode declaration.
       * @overrides tostring from Term trait
       */
      override val tostring: String = args match {
         case List() => functor
         case _ => functor + "(" + (for (a <- args) yield a.tostring).mkString(",") + ")"
      }

      /**
       * @return a string representation of the mode declaration. This method is supposed to be called on a
       * variabilized version of the mode declaration, and it surrounds with double quotes
       * variables that correspond to output and ground placemarkers. For instance, assume the mode atom
       *
       * modeb(p(+type1,-type2,#type3))
       *
       * and its variabilized version
       *
       * p(X,Y,Z)
       *
       * The result of applying this method on the above is
       *
       * p(X,"Y","Z"). These atoms are passed to the ASP solver, in order to generate query atoms, for the
       * construction of the body of a Kernel Set clause. The quoted variables are treated as constants by the
       * solver, which generates instances by grounding only the variables that correspond to input terms.
       * The quotes are removed by post-processing each atom in an answer set, thus obtaining a query atom (i.e.
       * an atom of the form p(2,Y,Z) from the above). This is a query atom, which is subsequently used to
       * generate groundings of the atom that bind only Y,Z vars, keeping the input term intact.
       *
       */

      override def tostringQuote: String = args match {
         case List() => functor
         case _ => functor + "(" + (for (a <- args) yield a.tostringQuote).mkString(",") + ")"
      }

      /**
       * Variabilizes a mode declaration atom, i.e. it replaces all in-out-ground placemarkers with fresh variables.
       * The variabilized mode declarations are used in the construction of bottom clauses, in order to generate ground
       * instances of mode declarations atoms, by replacing variables by constants found in the data.
       *
       * @returns a variabilized Literal. It's variables are annotated as +/-/# and it also carries a List[string] with the
       * typing predicates for it's variables.
       *
       */

      def varbed(): Literal = {
         val (varbed, ttypes, _) = variabilize(List(Literal(this.functor, List(), false)), this.args, List(), 0)
         val l = Literal(varbed(0).functor, varbed(0).terms, isNAF = false, typePreds = ttypes); l

      }

      /**
       *
       * This method does all the work of the variabilation.
       *
       * @param accum an accumulator that collects competed (variabilized) compound sub-terms.
       * @param remaining a list containing all remaining sub-terms that should be variabilized.
       * @param ttypes a list collecting typing predicates for the generated variables, e.g. person(X1), time(X100)
       * @param counter a counter that is incremented by 1 each time a new varaible is generated. The name a new variable is
       * simply "X"+currentCounterValue.
       */

      private def variabilize(accum: List[Literal], remaining: List[Expression],
         ttypes: List[String], counter: Int): (List[Literal], List[String], Int) = {
         def f(x: Expression, sign: String, tail: List[Expression]) = {
            val cur = accum match {
               case Nil => Literal(this.functor, List(), false)
               case _ => accum.last
            }
            // We are variabilizing everything (it's modes variabilization) so replace all with a new Var.
            val update = Literal(cur.functor, cur.terms :+ Variable("X" + counter, sign, x._type), false)
            this.variabilize(accum.tail :+ update, tail, ttypes :+ x._type + "(X" + counter + ")", counter + 1)
         }
         remaining match {
            case head :: tail => head match {
               case x: PosPlmrk => f(x, "+", tail)
               case x: NegPlmrk => f(x, "-", tail)
               case x: ConstPlmrk => f(x, "#", tail)
               case x: ModeAtom =>
                  val (varbed, newTypes, newCount) = this.variabilize(List(Literal(x.functor, List(), false)), x.args, List(), counter)
                  val pop = accum.last
                  this.variabilize(List(Literal(pop.functor, pop.terms ::: varbed, false)), tail, ttypes ::: newTypes, newCount)
               case _ => throw new LogicException("Variabilizing Mode Declaration " + this.tostring + ": Found unexpected type")
            }
            case Nil =>
               val pop = accum.last
               (accum.tail :+ Literal(pop.functor, pop.terms, false), ttypes, counter)
         }
      }

      /**
       * Recieves a tuple of the form ('in','out','ground'), where each coordinate in the tuple is List[Expression]
       * of constants marked as input, output or ground placemarkerks. From this input, this method constructs
       * all istances of the current mode declaration atom 'm', generated as follows:
       * -- Each input placemarker in 'm' is replaced by a term in 'in'
       * -- Each ground placemarker in 'm' is replaced by a variable.
       * -- Each output placemarker in 'm' is replaced by a variable.
       *
       * These constitute the query atoms, used to generate Kernel Set body atoms.
       *
       * @example
       *
       * Assume that the current mode atom is modeb(p(+entity1,-emtity2,#entity3)) and the input is
       * (List("e1","e2"),List("e3"),List("e4","e5")). The result of this method is the list of atoms:
       *
       * List(p(e1,Y,Z),p(e2,Y,Z)).
       *
       * This method is to be called uppon a variabilized version of the mode atom, and replace the variables that
       * correspond to input terms, with terms from 'in'
       *
       *
       */

      def generateQueryAtoms(input: (List[Expression], List[Expression], List[Expression])) = {

      }

   }

   /**
    *
    * A labelled example is generated from a bottom clause C. The head of C has been generated by abduction
    * and its body consists of everything relevant to the terms found in the head, collected in the interpretation
    * that represents the example. The latter is identified by its time key.
    *
    * whatIs is either 'initiatedAt' or 'terminatedAt'.
    *
    * 'botClause' is the bottom clause from which the example results.
    */

   case class LabelledExample(groundKernelClause: Clause, varKernelClause: Clause, commingFromDB: String = "") {

      val headAtomGrnd = groundKernelClause.head
      val headAtomVarbed = varKernelClause.head
      val exampleGrnd = groundKernelClause.body
      val exampleVarbed = varKernelClause.body
      // We assume that time is the last argument in a literal, as is the case with the Event Calculus.
      val timeKey = groundKernelClause.head.terms.last.tostring
      // is an initiatedAt or terminatedAt example?
      val initOrTerm = headAtomGrnd.functor
      // Which HLE is this example for?
      val hle =
         if (groundKernelClause.head.tostring.contains("meeting")) {
            "meeting"
         } else if (groundKernelClause.head.tostring.contains("moving")) {
            "moving"
         } else {
            "fighting"
         }
      def asGrndInterpretation: List[String] = for (x <- groundKernelClause.body) yield x.tostring
      def asVarbedInterpretation: List[String] = for (x <- varKernelClause.body) yield x.tostring

   }

   case class Example(e: DBObject, commingFromDB: String = "") {
      val annotation = e.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map { x => x.toString() }
      // Wraps inside an example/1 predicate for passing to ASP 
      val annotationASP =
         e.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
      val narrative = e.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map { x => x.toString() }
      val narrativeASP = e.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.");
   }

   class ThetaSubsumption {

      def subsumes(c1: Clause, c2: Clause): Boolean = {
         val tolit1 = c1.toLiteralList
         val tolit2 = c2.toLiteralList
         if (c1.toLiteralList.length == c2.toLiteralList.length) {
            subsumesLiteral(tolit1(0).asInstanceOf[Literal], tolit2(0).asInstanceOf[Literal])
         } else {

         }

         def subsumesLiteral(c1: Literal, c2: Literal): Boolean = {
            true
         }

         true
      }

   }

}


