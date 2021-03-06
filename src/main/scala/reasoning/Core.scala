/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.io.Source
import scala.util.matching._
import parsing.LogicParser._

object Core extends ModesParser{
  
  val aspSolverPath = new java.io.File("./asp").getCanonicalPath 
  val modesFile = new java.io.File("./knowledge/modes").getCanonicalPath
  //val modesFile = new java.io.File("./knowledge/ecoli/modes").getCanonicalPath
  val abdFile = new java.io.File("./knowledge/abdin.lp").getCanonicalPath
  val dedFile = new java.io.File("./knowledge/dedin.lp").getCanonicalPath
  val indFile = new java.io.File("./knowledge/indin.lp").getCanonicalPath
  val theoryFile = new java.io.File("./theory").getCanonicalPath
  val theoryCrossvalFile = new java.io.File("./knowledge/theoryCrossval.lp").getCanonicalPath
  val exmplsFile = new java.io.File("./knowledge/examples.lp").getCanonicalPath
  val crossvalFile = new java.io.File("./knowledge/crossval.lp").getCanonicalPath 
  //val exmplsFile = new java.io.File("./knowledge/ecoli/examples.lp").getCanonicalPath
  val bkFile = new java.io.File("./knowledge/bk.lp").getCanonicalPath
  //val bkFile = new java.io.File("./knowledge/ecoli/bk.lp").getCanonicalPath
  val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches
  val modes = Source.fromFile(modesFile).getLines.toList.map(x => x.replaceAll("\\s", "")).filter(line => !matches("""""".r, line))
  val modehs = modes.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).map(x => getParseResult(parseModes(modeh,x)))
  val modebs = modes.filter(m => m.contains("modeb") && !m.startsWith("%")).map(x => getParseResult(parseModes(modeb,x)))
  val x = modes.filter(m => m.contains("examplePattern") && !m.startsWith("%")).map(x => getParseResult(parseModes(exmplPattern,x))) 
  val examplePatterns = x match {
     case List() => modehs // If no example patterns are found, use the head mode declarations for them
     case _ => x
  }
  

  
  

}