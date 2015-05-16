/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.io.Source
import scala.util.matching._
import parsing.LogicParser._

object Core extends ModesParser{
  
  val modesFile = new java.io.File("./knowledge/modes").getCanonicalPath
  val abdFile = new java.io.File("./knowledge/abdin.lp").getCanonicalPath
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