/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.io.Source
import scala.util.matching._
import reasoning.Core._
import scala.util.parsing.combinator._
import parsing.LogicParser._
import scala.collection.mutable.ListBuffer
import reasoning.Exceptions._
import reasoning.Utils._
import scala.sys.process._

object Reasoning extends ASPResultsParser {

  /**
   * Transforms input to an ASP program. The program is written in an output file that is passed to the ASP solver.
   * This file is this only non-optional parameter of the method.
   *
   * @param writeToFile path to file where the ASP program is written.
   * @param program an (optional) set of ground or non-ground rules and/or ground facts.
   * @param generateDirectives an (optional) list containing declarations for atoms to be be generated during the computation
   * of answer sets. Example of such input:
   *
   * List("father(X,Y):person(X):person(Y)","grandfather(X,Y):person(X):person(Y)")
   *
   * Such a list is transformed into the "generate" part of the program:
   *
   * {father(X,Y):person(X):person(Y), grandfather(X,Y):person(X):person(Y)}.
   *
   * @param generateAtLeast an (optional) lower bound for the number of generated atoms to be included in an answer set.
   * @param generateAtMost an (optional) upper bound for the number of atoms to be included in an answer set.
   * @param minimizeStatements an (optional) list of atoms whose instances in an anser set should be minimized. Example of such input:
   *
   * List("father(X,Y)","grandfather(X,Y)"))
   *
   * Such a list is transformed into a minimize statement:
   *
   * #minimize{father(X,Y),grandfather(X,Y)}.
   *
   * @param maximizeStatements similze as above for maximize directives.
   * @param constraints a set of integrity constraints. Example:
   *
   * List(List("father(X,Y)","mother(X,Y)"), List("father(X,Y)","not male(X)"))
   *
   * Such input is transformed to integrity constraints in the ASP program:
   *
   * :- father(X,Y), mother(X,Y).
   * :- father(X,Y), not male(X).
   *
   * @param show a, (optional) list of atoms that are to be displayed. All other atoms in an answer set are hidden.
   * A #hide directive is generated is this list is not empty. Example:
   *
   * List("father(X,Y)","mother(X,Y)") or
   *
   * List("father/2","mother2")
   *
   * Such input is transformed into
   *
   *
   * #hide.
   * #show father(X,Y).
   * #show mother(X,Y)
   */

  def toASPprogram(program: List[String] = Nil,
    generateDirectives: List[String] = Nil,
    generateAtLeast: Int = 1000000000, generateAtMost: Int = 1000000000,
    minimizeStatements: List[String] = Nil, maximizeStatements: List[String] = Nil,
    constraints: List[List[String]] = Nil, show: List[String] = Nil, writeToFile: String): Any = {

    Utils.clearFile(writeToFile) // clear here, append everywhere else.
    Utils.writeToFile(new java.io.File(writeToFile), "append")(p => program foreach (p.println))
    val genStatems = (generateDirectives, generateAtLeast, generateAtMost) match {
      case x @ (Nil, _, _) => List()
      //case x @ (head :: tail, 1000000000,1000000000) => println(x); x._1.map( y => "{" + y + "}.\n")
      case x @ (head :: tail, 1000000000, 1000000000) =>
        println(x); for (e <- x._1) yield "{" + e + "}."
      case x @ (head :: tail, lower, 1000000000) => (head :: tail).map(y => "$lower {" + y + "}.\n")
      case x @ (head :: tail, 1000000000, upper) => (head :: tail).map(y => "0 {" + y + "} $upper.\n")
      case x @ (head :: tail, lower, upper) => (head :: tail).map(y => "$lower {" + y + "} $upper.\n")
    }
    Utils.writeToFile(new java.io.File(writeToFile), "append")(p => genStatems foreach (p.println))
    //genStatems
    val minStatement = minimizeStatements match { // This is a single string
      case Nil => ""
      case _ => "#minimize{ " + minimizeStatements.mkString(",") + "}.\n"
    }
    val maxStatement = maximizeStatements match { // This is a single string
      case Nil => ""
      case _ => "#maximize{ " + maximizeStatements.mkString(",") + "}.\n"
    }
    val constrs = constraints match { // This is a list of strings
      case Nil => List("")
      case _ => for (x <- constraints) yield ":- " + x.mkString(",") + ".\n"
    }
    Utils.writeLine(minStatement, writeToFile, "append")
    Utils.writeLine(maxStatement, writeToFile, "append")
    Utils.writeToFile(new java.io.File(writeToFile), "append")(p => constrs foreach (p.println))
    val (hideDir, showDirs) = show match {
      case Nil => ("", List(""))
      case _ => ("#hide.\n", (for (x <- show) yield "#show " + x + ".\n"))
    }
    Utils.writeLine(hideDir, writeToFile, "append")
    Utils.writeToFile(new java.io.File(writeToFile), "append")(p => showDirs foreach (p.println))
  }



  /**
   * Prepares the ASP input for abduction and calls the ASP solver to get the results.
   *
   * @param abducibles a flag to indicate where to get the abducible predicates from.
   * Currently the only acceptable flag is "modehs", meaning that abducible predicates
   * are the head mode declaration atoms.
   * @param numberOfModels an upper bound to the number of models. Currently this is not
   * used anywhere.
   * @throws AbductionException in case of mistaken or missing abducibles flag.
   *
   * @todo implement the case where abducible predicates are explicitly provided.
   * (see comments in code).  
   */ 

  def abduce(abducibles: Any, numberOfModels: Int = 1000): (Stream[Any], StringBuffer) = {
    def directives = {
      Core.modehs match {
        case Nil => throw new RuntimeException("No Mode Declatations found.")
        case _ =>
          val varbedMHAtoms = for (x <- Core.modehs) yield x.varbed
          val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
          val varbedToStr = varbedMHAtoms.map(x => x.tostring)
          // Generate directives from abducibles
          val generate: List[String] =
            varbedMHAtoms.map(x => x.tostring + ":" + x.typePreds.mkString(":"))

          /*
           * Generate the minimize directive for the abducibles
           *
           * The commented val below is for generating compression-based minimized
           * directives. See the comment below for why we don't use such at abduction
           * mode, and instead we go for hard constraints that guarantee full coverage
           * of the examples.
           *
           * -----------------------------------------------
           * Generate compression-based minimize directive:
           * -----------------------------------------------
           * val minimize = varbedToStr ::: varbedExmplPatterns.map(x => 
           *   List(s"posNotCovered($x)",s"negsCovered($x)")).flatten
           */
          // -------------------------------------------
          // Generate full-coverage minimize directive:
          //--------------------------------------------

          val minimize = varbedToStr

          /*
           * Generate coverage constraints for positive and negative examples:
           *
           * For abduction, hard constraints must be used to guarantee an optimal solution.
           * Note that even if the data is noisy, you can always find an abductive explanation 
           * that accounts for it. So, for abduction, we don't follow the commpression approach
           * (minimize{posNotCovered + negsCovered + hypothesisSize}), but instead we follow the
           * complete search approach (:- posNotCovered, :- negsCovered).
           * The commented val below is for generating compression-based directives to guide the search.
           *
           * ---------------------------------------------------------------
           * Generate compression-related definitions for example coverage:
           -----------------------------------------------------------------
           * val coverageConstr: List[String] =
           *   varbedExmplPatterns.map(x => 
           *   List(s"\nposNotCovered($x) :- example($x), not $x.", s"\nnegsCovered($x):- $x, not example($x).\n")).flatten
          */
          // Generate hard constraints for example coverage:
          val coverageConstr: List[String] = varbedExmplPatterns.map(x =>
            List(s"\n:- example($x), not $x.\n", s"\n:- $x, not example($x).\n")).flatten

          // Generate the "show" directives:
          val showDirs: List[String] = varbedToStr
          toASPprogram(program = coverageConstr, generateDirectives = generate,
            minimizeStatements = minimize, show = showDirs, writeToFile = Core.abdFile)
      }

    }
    abducibles match {
      case "modehs" => directives
      /* This is for the case where abducibles are explicitly given. 
          *
          * @todo: Implement this logic 
          * 
          * */
      case _: List[Any] => throw new AbductionException("This logic has not been implemented yet.")
      case _ => throw new AbductionException("You need to specifiy the abducible predicates.")
    }
    val command = List[String](Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09").mkString(" ")
    println(command)
    /*
      def runAbd(): (Stream[Any], StringBuffer) = {
         val buffer = new StringBuffer()
         val cmd = Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09")
         val allLines = cmd lines_! ProcessLogger(buffer append _)
         val lines = (for (x <- allLines) yield parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
            case Success(result, _) => result
            case f => None
         })
         (lines, buffer)
      }
      */
    //runAbd()

    runASPSolver("abduction")
  }

  

  

  def generateKernel(): Unit = {
    val abdModel = abduce("modehs")._1(0)
    
     def getMatchingMode(abduced: String): Any = {
       val toLit = Utils.objectFactory.getLiteral(abduced)
       for (x <- Core.modehs) {
          matchesMode
       } 
     }
    /* Utility function that checks if an abduced modeh atom matches 
     * a corresponding mode declaration atom. */ 
    def matchesMode(abduced: Literal, modeAtom: ModeAtom): Any = {
       val toLit = Utils.objectFactory.getLiteral(modeHAtom)
    }
  }





  /**
   * Calls the ASP solver and returns the results.
   * 
   * @todo Error handling needs fixing. Rightnow we do not intercept
   * the stderr stream, but we flag an error in case of an empty model.
   * 
   * FIX THIS!!!
  */ 

  def runASPSolver(task: String): (Stream[Any], StringBuffer) = {
    val buffer = new StringBuffer()
    val command = task match {
      case "abduction" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09")
      case "deduction" => Seq() // Not implemented yet
      case "induction" => Seq() // Not implemented yet
    }
    val allLines = command lines_! ProcessLogger(buffer append _)
    val lines = allLines match {
      case Stream() =>
        throw new ASPInputException(buffer.toString())
      case _ =>
        (for (x <- allLines) yield parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
          case Success(result, _) => result
          case f => None
        }).filter(x => x != None)
    }
    println("\n\n--------------")
    println(     "Models found:")
    println(     "--------------")
    println(lines(0))
    (lines, buffer)
  }

}