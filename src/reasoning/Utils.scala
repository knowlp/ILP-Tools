package reasoning

import java.io.FileWriter
import scala.sys.process._
import parsing.LogicParser._
import reasoning.Exceptions._
import com.mongodb.casbah.Imports._
//import reasoning.Utils
//import reasoning.Core
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.JavaConversions._
import scala.util.matching.Regex._
import scala.util.matching.Regex
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ListBuffer

object Utils extends ASPResultsParser {

   /* 
    * Write an iterable to file
    * 
    * Usage:
    * 
    * writeToFile(new File("example.txt")) { p => data.foreach(p.println) }
    * 
    */

   def writeToFile(f: java.io.File, howTowrite: String)(op: java.io.PrintWriter => Unit) {
      val p = howTowrite match {
         case "append" => new java.io.PrintWriter(new FileWriter(f, true))
         case "overwrite" => new java.io.PrintWriter(new FileWriter(f, false))
         case _ => new java.io.PrintWriter(new FileWriter(f, false)) // default is overwrite
      }
      try { op(p) } finally { p.close() }
   }

   def runASPSolver(task: String): (Stream[Any], StringBuffer) = {
      val buffer = new StringBuffer()
      val command = task match {
         case "abduction" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09")
         case "deduction" => Seq() // Not implemented yet
         case "induction" => Seq() // Not implemented yet
      }

      /*
      val logger = ProcessLogger(
         //(o: String) => println("out " + o),
         (o: String) => buffer append o,
          (e: String) => throw new ASPInputException(e) )
          * 
          */
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
      //lines.filter(x => x != None)
      (lines, buffer)
   }

   /*
    * -------------------------------------------------------------------------------------------
    * Utilities for connecting to mongo and getting examples. Fixes for locking below.
    * -------------------------------------------------------------------------------------------
    * 
    * Fix mongo connectivity issues: 
    *                                                              
    * -- Manually remove the lockfile: sudo rm /var/lib/mongodb/mongod.lock  
    *                      
    * -- Run the repair script: sudo -u mongodb mongod -f /etc/mongodb.conf --repair 
    *              
    * -- Start your MongoDB server with sudo start mongodb and verify it is running with sudo 
    *     
    *    status mongodb and by trying to connect to it with mongo test.
    * 
    * --------------------------------------------------------------------------------------------
    */

   def addToBD(dbName: String, colName: String): Unit = {

   }
   /**
    * Get all examples from a DB and write them to ASP input files
    */
   def getAllExamples(db: String, collection: String): MongoCollection = {
      val col = MongoClient()(db)(collection)
      examplestoASP("all", "all", col)
      col
   }
   /*
    * Get one example
    */
   def getOneExample(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      examplestoASP(field, fieldValue, collection)
   }

   def examplestoASP(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      var annotation = new ListBuffer[String]()
      var narrative = new ListBuffer[String]()
      field match {
         case "all" =>
            for (x <- collection.find()) {
               annotation = annotation ++ x.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
               narrative = narrative ++ x.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.");
            }
            write((annotation, narrative))
         case "\\s" => throw new RuntimeException("Which example do you want?")
         case _ => fieldValue match {
            case "\\s" => throw new RuntimeException("Which example do you want?")
            case "all" => throw new RuntimeException("Excecution should not have reached this code")
            case _ =>
               val query = MongoDBObject(field -> fieldValue)
               try {
                  val target = collection.findOne(query).get
                  annotation = annotation ++ target.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
                  narrative = narrative ++ target.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.");
                  write((annotation, narrative))
               } catch {
                  case e: NoSuchElementException =>
                     println(s"The example with \'field -> value\' : \'$field -> $fieldValue\' does not exit");
                     System.exit(-1)
               }

         }
      }
      def write(ins: (ListBuffer[String], ListBuffer[String])): Unit = {
         Utils.writeToFile(new java.io.File(Core.exmplsFile), "overwrite")(p => ins._1 foreach (p.println))
         Utils.writeToFile(new java.io.File(Core.exmplsFile), "append")(p => ins._2 foreach (p.println))
      }
   }

   def computeDistancesMany(supervision: List[List[String]], collection: MongoCollection): Unit = {
      collection.find().foreach(row =>
         try {
            for (x <- supervision) print(reasoning.Logic.exmplHausdrfDist(x, f(row)) + " ")
            println("\n")
         } catch {
            case e: MyParsingException => f(row).foreach(println); println(e); throw new RuntimeException
         })
   }

   def computeDistances(realExmpl: List[String], collection: MongoCollection): Unit = {
      collection.find().foreach(row =>
         try {
            println(reasoning.Logic.exmplHausdrfDist(realExmpl, f(row)))
         } catch {
            case e: MyParsingException => f(row).foreach(println); println(e); throw new RuntimeException
         })
   }

   /**
    * Helper method
    */
   def f(row: Any): List[String] = {
      val x = row.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList
      val y = row.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList
      val example = x ++ y map { x => x.asInstanceOf[String] }
      example
   }
   /**
    * Get an example as an interpretation in a list
    */
   def getExample(field: String, fieldValue: Any, collection: MongoCollection): List[String] = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query) match {
         case Some(x) => x
         case _ => List()
      }
      val narrative = target.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList
      val annotation = target.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList
      val oneExample = narrative ++ annotation map { x => x.asInstanceOf[String] }
      oneExample
   }

   /* Get a simple string as result, from the field of interest */
   def getStringByField(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query)
      val result = target match {
         case Some(x) => target.get(field)
         case _ => None
      }
      result
   }

   /* Returns a query whose result is an array */
   def getArrayByField(field: String, fieldValue: Any, collection: MongoCollection): Any = {
      val query = MongoDBObject(field -> fieldValue)
      val target = collection.findOne(query).get
      //val result = target match {
      //  case Some(x) => target.getAs[MongoDBList](field).get
      //}
      //result
   }

   /* Returns the first entry. Works for data stored as arrays*/
   def getOneArray(collection: MongoCollection): List[String] = {
      val target = collection.findOne().asInstanceOf[BasicDBList]

      val result = Some(List[String]() ++ target map { x => x.asInstanceOf[String] }) match {
         case Some(z) => z
         case _ => List[String]()
      }
      result

   }

   /**
    * ---------------------------------------------------------------------------------------------------------
    *
    * Utilities for parsing CAVIAR data and storing each example in mongo. Top-level method is parseCaviar
    * which needs the path to the folder with the CAVIAR data.
    *
    * ---------------------------------------------------------------------------------------------------------
    */

   

   def parseCaviar(dirName: String): Unit = {
      println(s"Parsing $dirName")
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

      def formExample(timeKey: Int, others: List[Seq[(Int, String)]]): List[String] = {
         val x = for (s <- others; y <- s if (y._1 == timeKey)) yield y._2; x
      }

      def toMongo(example: Tuple2[Int, List[String]], col: MongoCollection): Unit = {
         val annotPattern = """holdsAt\((meeting|moving|fighting)\(([a-z0-9]*),([a-z0-9]*)\),([0-9]*)\)""".r
         val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches
         var annotation = new ListBuffer[String]() // mutable
         var narrative = new ListBuffer[String]() // mutable
         for (atom <- example._2) {
            if (matches(annotPattern, atom)) {
               val annotPattern(hle, p1, p2, time) = atom
               val dual = s"holdsAt($hle($p2,$p1),$time)"
               annotation += atom
               annotation += dual
            } else {
               narrative += atom
            }
         }
         val entry = MongoDBObject("time" -> example._1) ++ ("annotation" -> annotation) ++
            ("narrative" -> narrative)
         col.insert(entry)
      }

      var all = Map[String, Seq[(Int, String)]]()
      val files = getFiles(dirName)
      for (f <- files) {
         val data = scala.io.Source.fromFile(dirName.concat("/").concat(f.getName())).mkString.replaceAll("\\s", "")
         for (key <- patterns.keySet) {
            val matched = process(key, data)
            if (matched != Seq()) all += (key -> matched)
         }
      }
      //val allButLLEs = for((k,v) <- all; if k != "lles") yield (k,v)
      val allButLLEs = for ((k, v) <- all; if true) yield (k, v) // We need to search in the LLEs too... I should fix this at some time, there in no point in doing this here
      val examples = for (
         x <- all("lles");
         val t = for (t <- allButLLEs.keySet) yield allButLLEs(t);
         val y = formExample(x._1, t.toList)
      ) yield (x._1, x._2 :: y)

      val dbName = "CAVIAR-" + dirName.split("/").last //the db is created at this point           
      val collection = MongoClient()(dbName)("examples")
      collection.drop // clear it in any case
      // Store in Mongo
      examples.foreach(toMongo(_, collection))

   }

}