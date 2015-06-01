package reasoning

import scala.sys.process.ProcessIO
import parsing.LogicParser._
import reasoning.Exceptions._
import scala.sys.process._

object Crossvalidation extends ASPResultsParser {

   var fps = 0
   var fns = 0
   var tps = 0

   def run(theoryPath: String = Core.theoryFile, testingDatasetPath: String = Core.exmplsFile,
      crossvalHelperASPath: String): Unit = {

      val command = Seq(Core.aspSolverPath + "/./clingo", Core.bkFile,
         testingDatasetPath, theoryPath, crossvalHelperASPath, "0", "--asp09")
      val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
      val coverageConstr: List[String] = varbedExmplPatterns.map(x =>
         List(s"\nposNotCovered($x) :- example($x), not $x.",
            s"\nnegsCovered($x):- $x, not example($x).\n",
            s"\nposCovered($x):- $x, example($x).\n")).flatten

      // Get the false negatives:     
      reasoning.Reasoning.toASPprogram(program = coverageConstr,
         show = List("posNotCovered/1"),
         writeToFile = crossvalHelperASPath)
      fns = asp(command, "fns")(0).asInstanceOf[List[String]].length  
      //Get the false positives :
      reasoning.Reasoning.toASPprogram(program = coverageConstr,
         show = List("negsCovered/1"),
         writeToFile = crossvalHelperASPath)
      fps = asp(command, "fps")(0).asInstanceOf[List[String]].length 
      //Get the true positives:
      reasoning.Reasoning.toASPprogram(program = coverageConstr,
         show = List("posCovered/1"),
         writeToFile = crossvalHelperASPath)
      tps = asp(command, "tps")(0).asInstanceOf[List[String]].length 

      println("")
      println("FPs: " + fps)
      println("FNs: " + fns)
      println("TPs: " + tps)
      println("Precision: " + tps.toFloat / (tps + fps))
      println("Recall: " + tps.toFloat / (tps + fns))
   }
   /*
   def getResult(x: String, what: String): Int = {
      parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
         case Success(result, _) =>
            result match {
               case List() => what match {
                  case "fps" =>
                     fps = 0; 0
                  case "fns" =>
                     fns = 0; 0
                  case "tps" => tps = 0; 0
               }
               case _ => what match {
                  case "fps" =>
                     fps = result.length; result.length
                  case "fns" =>
                     fns = result.length; result.length
                  case "tps" => tps = result.length; result.length
               }
            }
         case f => throw new LogicException("Parsing error at cross-validation.")
      }
   }
   */
   def asp(command: Seq[String], what: String) = {
      val buffer = new StringBuffer()
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
      //println(lines)
      lines

   }

}