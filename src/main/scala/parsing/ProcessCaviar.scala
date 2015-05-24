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
import reasoning.Utils._

object ProcessCaviar {
  
   class CaviarDataParser extends JavaTokenParsers {

       
      val test = false
     
      def strLiteral: Parser[String] = """[a-zA-Z0-9_]*""".r;
      def person: Parser[String] = """id[0-9]""".r
      def num: Parser[String] = """[0-9]*""".r;

      def parseCaviar(dirName: String): Unit = {

         var storeMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[String, ListBuffer[String]]]()

         /*
          * 'where' in the args is either 'narrative' or 'annotation'
          */
         val updateStoreMap = (timeKey: String, newAtom: String, where: String) =>
            if (storeMap.keySet.exists(_ == timeKey.toInt)) {
               if (storeMap(timeKey.toInt).keySet.exists(_ == where)) {
                  storeMap(timeKey.toInt)(where) += newAtom
               } else {
                  storeMap(timeKey.toInt) += where -> ListBuffer(newAtom)
               }
            } else {
               storeMap += timeKey.toInt -> scala.collection.mutable.Map(where -> ListBuffer(newAtom))
            }
        
         def getFiles(dirName: String): List[java.io.File] = {
            val nameTest = (x: java.io.File) => x.getName.contains("Appearence") |
               x.getName.contains("Movement") |
               x.getName.contains("SituationGrp")
            val fs = new java.io.File(dirName).listFiles.filter(nameTest)
            fs.toList
         }

         def parseHLEs(pattern: Regex, matched: MatchIterator): Unit = {
            for (m <- matched; val pattern(hle, _, p1, p2, time) = m) hle match {
               /*Map HLEs to their time stamps, in order to use the times as keys to retrieve other relevant data*/
               case "moving" => updateStoreMap(time, s"holdsAt(moving($p1,$p2),$time)","annotation");
                                updateStoreMap(time, s"holdsAt(moving($p2,$p1),$time)","annotation")
               case "interacting" => updateStoreMap(time, s"holdsAt(meeting($p1,$p2),$time)","annotation");
                                     updateStoreMap(time, s"holdsAt(meeting($p2,$p1),$time)","annotation") 
               case "fighting" => updateStoreMap(time, s"holdsAt(fighting($p1,$p2),$time)","annotation");
                                  updateStoreMap(time, s"holdsAt(fighting($p1,$p2),$time)","annotation")
            }
         }

         def parseLLEs(pattern: Regex, matched: MatchIterator): Unit = {
            for (m <- matched; val pattern(_, _, time) = m)
               updateStoreMap(time, m, "narrative")
         }

         def parseCoords(pattern: Regex, matched: MatchIterator): Unit = {
            //val x = for (m <- matched; val pattern(p, x, y, time) = m) yield time.toInt -> s"coords($p,$x,$y,$time)"
            for (m <- matched; val pattern(p, x, y, time) = m) updateStoreMap(time, s"coords($p,$x,$y,$time)","narrative")
         }

         def parseAppear(pattern: Regex, matched: MatchIterator): Unit = {
            for (m <- matched; val pattern(p, appear, time) = m) appear match {
               case "appear" => updateStoreMap(time, s"happensAt(enters($p),$time)","narrative")
               case "disappear" => updateStoreMap(time, s"happensAt(exits($p),$time)","narrative")
               case "visible" => updateStoreMap(time, s"holdsAt(visible($p),$time)","narrative")
            }
         }
         def parseDirection(pattern: Regex, matched: MatchIterator): Unit = {
            for (m <- matched; val pattern(p, direction, time) = m)
               updateStoreMap(time, s"orientation($p,$direction,$time)","narrative")
         }

         val patterns = Map[String, Tuple2[Regex, (Regex, MatchIterator) => Unit]](
            "hles" -> ("""happensAt\((moving|interacting|fighting)\(([a-zA-Z0-9_]*),\[([a-z0-9]*),([a-z0-9]*)\]\),([0-9]*)\)""".r, parseHLEs),
            "lles" -> ("""happensAt\(([a-zA-A0-9_]*)\(([a-zA-A0-9_]*)\),([0-9]*)\)""".r, parseLLEs),
            "coords" -> ("""holdsAt\(coord\(([a-z0-9]*)\)=\(([0-9]*),([0-9]*)\),([0-9]*)\)""".r, parseCoords),
            "appear" -> ("""holdsAt\(appearance\(([a-z0-9]*)\)=([a-z0-9A-Z]*),([0-9]*)\)""".r, parseAppear),
            "orient" -> ("""holdsAt\(orientation\(([a-z0-9]*)\)=([0-9]*),([0-9]*)\)""".r, parseDirection))

         def process(patternName: String, data: String): Unit = {
            val matchesFound = patterns(patternName)._1.findAllIn(data)
            val f = patterns(patternName)._2
            f(patterns(patternName)._1, matchesFound)
         }


         def toMongo(example: (Int, scala.collection.mutable.Map[String, ListBuffer[String]]), col: MongoCollection): Unit = {
            val anot = if( example._2.keySet.exists(_ == "annotation") ) example._2("annotation") else ListBuffer[String]()
            val entry = MongoDBObject("time" -> example._1) ++ ("annotation" -> anot) ++
               ("narrative" -> example._2("narrative"))
            println(entry)   
            col.insert(entry)
         }

         
         val files = getFiles(dirName)
         for (f <- files) {
            val data = scala.io.Source.fromFile(dirName.concat("/").concat(f.getName())).mkString.replaceAll("\\s", "")
            for (key <- patterns.keySet) {
               process(key, data) // this stores the data in storeMap
            }
         }
  
         val dbName = "CAVIAR-" + dirName.split("/").last //the db is created at this point           
         val collection = MongoClient()(dbName)("examples")
         collection.drop // clear it in any case
         storeMap.toSeq.sortBy(_._1).foreach(toMongo(_, collection))

      }

  
    
     
   }

}