package reasoning

import java.io.FileWriter
import scala.sys.process._
import parsing.LogicParser._
import reasoning.Exceptions._
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.JavaConversions._
import scala.util.matching.Regex._
import scala.util.matching.Regex
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ListBuffer
import java.io.BufferedWriter
import scala.io.Source
import reasoning.Structures._

object Utils extends ASPResultsParser {

  /**
   * This is a helper object for easily parsing strings and creating objects from them.
   * This is basically for testing and debugging.
   *
   */

  object objectFactory extends ModesParser {

    def getLiteral(lit: String, mode: String = ""): Literal = {
      val out = mode match {
        case "" => getParseResult(parse(literal, lit)).asInstanceOf[Literal]
        case _ =>
          val l = getParseResult(parse(literal, lit)).asInstanceOf[Literal]
          val m = getModeAtom(mode)
          Literal(l.functor, l.terms, l.isNAF, modeAtom = m)
      }
      out
    }

    def getLiteralNoParse1(lit: String, mode: ModeAtom = ModeAtom("", List())): Literal = {
      val l = getParseResult(parse(literal, lit)).asInstanceOf[Literal]
      val out = Literal(l.functor, l.terms, l.isNAF, modeAtom = mode)
      out
    }

    def getLiteralNoParse(lit: Literal, mode: ModeAtom = ModeAtom("", List())): Literal = {
      val out = mode match {
        case ModeAtom("", List()) => lit
        case _ => Literal(lit.functor, lit.terms, lit.isNAF, modeAtom = mode)
      }
      out
    }

    def getModeHAtom(atom: String): ModeAtom = {
      getParseResult(parseModes(modeh, atom))
    }

    def getModeAtom(atom: String): ModeAtom = {
      getParseResult(parseModes(mode, atom))
    }

    def getModeBAtom(atom: String): ModeAtom = {
      getParseResult(parseModes(modeb, atom))
    }
  }

  /**
   * A function that replaces every 'token' \in 'tokens' found in 'word' with 'replWith'. 'word' is the 'initial' value
   * passed to the foldLeft method and 'currentWord' is the accumulator
   */
  val replInWord = (tokens: List[String], word: String, replWith: String) =>
    tokens.foldLeft(word)((currentWord, token) => currentWord.replaceAll(token, replWith))

  def clearFile(file: String): Unit = {
    val writer = new java.io.PrintWriter(new FileWriter(new java.io.File(file), false))
    writer.write("")
    writer.close()
  }

  /* 
	 * Write an iterable to file. Usage:
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

  def readFiletoList(file: String): List[String] = {
    Source.fromFile(file).getLines.toList
  }

  def readFileToString(file: String) = {
    Source.fromFile(file).mkString.replaceAll("\\s", "")
  }

  def write(ins: (ListBuffer[String], ListBuffer[String])): Unit = {
    Utils.writeToFile(new java.io.File(Core.exmplsFile), "overwrite")(p => ins._1 foreach (p.println))
    Utils.writeToFile(new java.io.File(Core.exmplsFile), "append")(p => ins._2 foreach (p.println))
  }

  def writeLine(in: String, file: String, append: String): Unit = {
    val w = append match {
      case "append" => new BufferedWriter(new FileWriter(file, true))
      case "overwrite" => new BufferedWriter(new FileWriter(file, false))
      case _ => throw new RuntimeException("Specify append or overwrite")
    }
    w.write(in)
    w.close()
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

  /**
   * Parses all CAVIAR dataset to mongo
   */

  def caviartoMongo(path: String): Unit = {
    val splitPath = path.split("/").filter(x => x != "\\s" | x != "") // handle '/' in the path input
    val dirs = for (file <- new java.io.File(path).listFiles.sorted) yield file.getName()
    dirs.foreach(x =>
      try {
        Utils.parseCaviar(splitPath.mkString("/") + "/" + x)
      } catch {
        case e: scala.MatchError => println(e.getMessage)
      })
  }

  /**
   * Returns the names of all existing DBs. Identifier (if passed) is used to filter the names.
   *
   */

  def getAllDBs(identifier: String = ""): List[String] = {
    val mongoClient = MongoClient()
    val all = mongoClient.databaseNames.filter(x => x.contains("CAVIAR-")).toList
    all.foreach(x => mongoClient(x)("examples"))
    mongoClient.close()
    all
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
        case "moving" =>
          updateStoreMap(time, s"holdsAt(moving($p1,$p2),$time)", "annotation");
          updateStoreMap(time, s"holdsAt(moving($p2,$p1),$time)", "annotation")
        case "interacting" =>
          updateStoreMap(time, s"holdsAt(meeting($p1,$p2),$time)", "annotation");
          updateStoreMap(time, s"holdsAt(meeting($p2,$p1),$time)", "annotation")
        case "fighting" =>
          updateStoreMap(time, s"holdsAt(fighting($p1,$p2),$time)", "annotation");
          updateStoreMap(time, s"holdsAt(fighting($p1,$p2),$time)", "annotation")
      }
    }

    def parseLLEs(pattern: Regex, matched: MatchIterator): Unit = {
      for (m <- matched; val pattern(_, _, time) = m)
        updateStoreMap(time, m, "narrative")
    }

    def parseCoords(pattern: Regex, matched: MatchIterator): Unit = {
      //val x = for (m <- matched; val pattern(p, x, y, time) = m) yield time.toInt -> s"coords($p,$x,$y,$time)"
      for (m <- matched; val pattern(p, x, y, time) = m) updateStoreMap(time, s"coords($p,$x,$y,$time)", "narrative")
    }

    def parseAppear(pattern: Regex, matched: MatchIterator): Unit = {
      for (m <- matched; val pattern(p, appear, time) = m) appear match {
        case "appear" => updateStoreMap(time, s"happensAt(enters($p),$time)", "narrative")
        case "disappear" => updateStoreMap(time, s"happensAt(exits($p),$time)", "narrative")
        case "visible" => updateStoreMap(time, s"holdsAt(visible($p),$time)", "narrative")
      }
    }
    def parseDirection(pattern: Regex, matched: MatchIterator): Unit = {
      for (m <- matched; val pattern(p, direction, time) = m)
        updateStoreMap(time, s"orientation($p,$direction,$time)", "narrative")
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
      val anot = if (example._2.keySet.exists(_ == "annotation")) example._2("annotation") else ListBuffer[String]()
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