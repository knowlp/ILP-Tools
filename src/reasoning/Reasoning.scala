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

   def abduce(abducibles: Any, numberOfModels: Int): (Stream[Any], StringBuffer) = {
      abducibles match {
         case "modehs" =>
            Utils.writeToFile(new java.io.File(Core.abdFile), "overwrite")(p => (for (
               x <- Core.modehs; val atom = x.varbed._1;
               val types = x.varbed._2.mkString(":")
            ) yield s"0{$atom:$types}$numberOfModels.") foreach (p.println))
            // Generate the minimize directive for the abducibles:
            Utils.writeToFile(new java.io.File(Core.abdFile), "append")(p => p.println("\n#minimize{" + (for (
               x <- Core.modehs;
               val atom = x.varbed._1; val types = x.varbed._2.mkString(":")
            ) yield s"$atom").mkString(",") + "}.\n\n"))
            // Generate coverage constraints for positive examples:
            Utils.writeToFile(new java.io.File(Core.abdFile), "append")(p => (for (
               x <- Core.examplePatterns; val atom = x.varbed._1;
               val types = x.varbed._2.mkString(":")
            ) yield s":- $atom, not example($atom).") foreach (p.println))
            // Generate coverage constraints for negative examples:
            Utils.writeToFile(new java.io.File(Core.abdFile), "append")(p => (for (
               x <- Core.examplePatterns; val atom = x.varbed._1;
               val types = x.varbed._2.mkString(":")
            ) yield s":- example($atom), not $atom.") foreach (p.println))
            Utils.writeToFile(new java.io.File(Core.abdFile), "append")(p => p.println("\n\n#hide.\n\n"))
            // Generate the "show" directives:
            Utils.writeToFile(new java.io.File(Core.abdFile), "append")(p => (for (x <- Core.modehs; val atom = x.varbed._1)
               yield s"#show $atom.") foreach (p.println))
         /* This is for the case where abducibles are explicitly given. 
          *
          * @TODO: Implement this logic 
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
      Utils.runASPSolver("abduction") 
   }

}