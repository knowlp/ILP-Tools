/**
 * @author Nikos Katzouris
 *
 */

package parsing

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.JavaConversions._
import scala.util.matching.Regex._
import scala.util.matching.Regex
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ListBuffer


object ProcessCaviar {

  class CaviarDataParser extends JavaTokenParsers {

    def strLiteral: Parser[String] = """[a-zA-Z0-9_]*""".r;
    def person: Parser[String] = """id[0-9]""".r
    def num: Parser[String] = """[0-9]*""".r;

    def parseCaviar(dirName: String): Unit = {
      def getFiles(dirName: String): List[java.io.File] = {
        val nameTest = (x: java.io.File) => x.getName.contains("Appearence") |
          x.getName.contains("Movement") |
          x.getName.contains("SituationGrp")
        val fs = new java.io.File(dirName).listFiles.filter(nameTest)
        fs.toList
      }
      def parseHLEs(pattern: Regex, matched: MatchIterator): Map[Int, String] = {
        val x = for (m <- matched; val pattern(hle, _, p1, p2, time) = m) yield hle match {
          /*Map HLEs to their time stamps, in order to use the times as keys to retrieve other relevant data*/
          case "moving" => time.toInt -> s"holdsAt(moving($p1,$p2),$time)"
          case "interacting" => time.toInt -> s"holdsAt(meeting($p1,$p2),$time)"
          case "fighting" => time.toInt -> s"holdsAt(fighting($p1,$p2),$time)"
        }
        x.toMap
      }
      def parseLLEs(pattern: Regex, matched: MatchIterator): Map[Int, String] = {
        val x = for (m <- matched; val pattern(_, _, time) = m) yield time.toInt -> m
        x.toMap
      }
      def parseCoords(pattern: Regex, matched: MatchIterator): Map[Int, String] = {
        val x = for (m <- matched; val pattern(p, x, y, time) = m) yield time.toInt -> s"coords($p,$x,$y,$time)"
        x.toMap
      }
      def parseAppear(pattern: Regex, matched: MatchIterator): Map[Int, String] = {
        val x = for (m <- matched; val pattern(p, appear, time) = m) yield appear match {
          case "appear" => time.toInt -> s"happensAt(enters($p),$time)"
          case "disappear" => time.toInt -> s"happensAt(exits($p),$time)"
          case "visible" => time.toInt -> s"holdsAt(visible($p),$time)"
        }
        x.toMap
      }
      def parseDirsction(pattern: Regex, matched: MatchIterator): Map[Int, String] = {
        val x = for (m <- matched; val pattern(p, direction, time) = m) yield time.toInt -> s"orientation($p,$direction,$time)"
        x.toMap
      }

      val patterns = Map[String, Tuple2[Regex, (Regex, MatchIterator) => Map[Int, String]]](
        "hles" -> ("""happensAt\((moving|interacting|fighting)\(([a-zA-Z0-9_]*),\[([a-z0-9]*),([a-z0-9]*)\]\),([0-9]*)\)""".r, parseHLEs),
        "lles" -> ("""happensAt\(([a-zA-A0-9_]*)\(([a-zA-A0-9_]*)\),([0-9]*)\)""".r, parseLLEs),
        "coords" -> ("""holdsAt\(coord\(([a-z0-9]*)\)=\(([0-9]*),([0-9]*)\),([0-9]*)\)""".r, parseCoords),
        "appear" -> ("""holdsAt\(appearance\(([a-z0-9]*)\)=([a-z0-9A-Z]*),([0-9]*)\)""".r, parseAppear),
        "orient" -> ("""holdsAt\(orientation\(([a-z0-9]*)\)=([0-9]*),([0-9]*)\)""".r, parseDirsction))

      def process(patternName: String, data: String): Seq[(Int, String)] = {
        val matchesFound = patterns(patternName)._1.findAllIn(data)
        val f = patterns(patternName)._2
        val results = f(patterns(patternName)._1, matchesFound)
        results.toSeq.sortBy(_._1)
        //results.foreach(println)
      }

      def formExample(timeKey: Int, others: List[Seq[(Int,String)]]): List[String] = {
        val x = for(s <- others; y <- s if(y._1 == timeKey)) yield y._2 ; x
      }
      
      def toMongo(example: Tuple2[Int,List[String]], col: MongoCollection): Unit = {
        val annotPattern = """holdsAt\((meeting|moving|fighting)\(([a-z0-9]*),([a-z0-9]*)\),([0-9]*)\)""".r
        val matches = (p:Regex, str: String) => p.pattern.matcher(str).matches
        var annotation = new ListBuffer[String]() // mutable
        var narrative = new ListBuffer[String]() // mutable
        for(atom <- example._2){
          if(matches(annotPattern,atom)){
            val annotPattern(hle, p1, p2, time) = atom
            val dual = s"holdsAt($hle($p2,$p1),$time)"
            annotation += atom
            annotation += dual
          }else{
            narrative += atom
          }
        }
        val entry = MongoDBObject("time" -> example._1) ++ ("annotation" -> annotation) ++ 
                                                           ("narrative" -> narrative)
        col.insert(entry)                                                   
      }
      
      var all = Map[String,Seq[(Int,String)]]()
      val files = getFiles(dirName)
      for (f <- files) {
        val data = scala.io.Source.fromFile(dirName.concat("/").concat(f.getName())).mkString.replaceAll("\\s", "")
        for(key <- patterns.keySet){
          val matched = process(key, data)
          if(matched != Seq()) all += (key -> matched)
        } 
      }
      val allButLLEs = for((k,v) <- all; if k != "lles") yield (k,v)
      val examples = for(x <- all("lles");
                  val t = for(t <- allButLLEs.keySet) yield allButLLEs(t);
                  val y = formExample(x._1,t.toList)) 
                  yield (x._1,x._2 :: y)
      val collection = MongoClient()("CAVIAR")("examples")
      // Store in Mongo
      examples.foreach(toMongo(_,collection))
      //for( e <- examples) toMongo(e,collection)
      
      
    }
    
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
    
    def lle: Parser[String] = "happensAt"~"("~strLiteral~"("~person~")"~","~num~")" ^^ {case "happensAt"~"("~lle~"("~p~")"~","~time~")" => s"happensAt($lle($p),$time)"}
    
    /*
     * Parses appearance attoms. Format:
     * holdsAt( appearance( id0 )=appear,  680 )
     * Turn that into:
     * if "appear" then "happensAt(enters(id0),680)" else if "visible" then "holdsAt(visible(id0),680)" else (if disappear) "happensAt(exits(id0),680)" 
     */

    def visibility: Parser[String] = "holdsAt"~"("~"appearance"~"("~person~")"~"="~strLiteral~","~num~")" ^^
    {case "holdsAt"~"("~"appearance"~"("~p~")"~"="~what~","~time~")" =>  what match {
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
     def orientation: Parser[String] = "holdsAt"~"("~"orientation"~"("~person~")"~"="~num~","~num~")" ^^ 
      {case "holdsAt"~"("~"orientation"~"("~p~")"~"="~o~","~time~")" => s"orientation($p,$o,$time)"}
  }
 

}