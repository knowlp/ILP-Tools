package reasoning

import scala.io.Source
import scala.util.matching._
import reasoning.Core._
import parsing.LogicParser._
import reasoning.Reasoning._
import reasoning.Structures._
import reasoning.Exceptions._
import scala.collection.mutable.ListBuffer
//import parsing.LogicParser.ClausalLogicParser
import com.mongodb.casbah.Imports._
import reasoning.Utils._
import scala.sys.process._

object KnnClassifier extends parsing.LogicParser.ClausalLogicParser {

  def getUnlabelledExmpls(db: String, collection: String): Unit = {
    val (supervision, dbcol) = this.getSupervision(db, collection)
    Utils.computeDistancesMany(supervision, dbcol)
  }

  def getSupervision(db: String, collection: String): (List[List[String]], MongoCollection) = {
    val dbConnection = Utils.getAllExamples(db, collection) // fetches the examples and returns a collection instance to use later. 
    val (aspResult, buffer) = abduce("modehs", 10)
    /* We may have a number of solutions (in optimization tasks), fro which we need the last one */
    val abduced = if (aspResult.length == 1) aspResult(0) else aspResult.last
    var seenTimes = new ListBuffer[Int]()
    var supervision = new ListBuffer[List[String]]()
    abduced match {
      case _: List[_] => proceed(abduced.asInstanceOf[List[String]])
      case _ => throw new AbductionException("Problem with the output of the ASP solver.")
    }
    def proceed(atoms: List[String]): Unit = {
      for (atom <- atoms) {
        val time = getParseResult(parse(literal, atom)).asInstanceOf[Literal].terms.last.asInstanceOf[Constant].name.toInt
        if (!seenTimes.contains(time)) supervision = supervision :+ Utils.getExample("time", time, dbConnection)
        seenTimes = if (seenTimes.contains(time)) seenTimes else seenTimes :+ time
      }
      //supervision.foreach(println)
    }
    (supervision.toList, dbConnection)
  }

  /**
   * Experimental. Get consecutive pairs of examples. Keep the annotation only for the second example, so that we can
   * construct a Kernel Set from the first example.
   */

  /*
   def main(args: Array[String]){
      //val col = MongoClient()("CAVIAR-01-Walk1")("examples")
      //getPairs(col)
      
      val alldbs = Utils.getAllDBs("CAVIAR-")
      val accum = new ListBuffer[LabelledExample]
      for (x <- alldbs) {
        println(x) 
        Utils.getAllExamples(x,"examples")
        val here = new java.io.File( "." ).getCanonicalPath
        val cmd = "python "+here+"/py-ILED/src/main.py mode=batch kernel-set-only=true"
        //println(cmd)
        cmd.!
        kernelToExamples.foreach(x => accum += x)
      }
      accum.foreach(x => println(x.timeKey,x.botClause))
   }
   */

  def kernelToExamples: List[LabelledExample] = {
    val kernelFile = "/home/nkatz/dev/ILP-Tools/py-ILED/runtime/var-kernel-set.lp"
    // We read all variabilized kernel set clauses, since we are interested in the general 'pattern/structure'
    // that a clause represents, and not in its specific instantiations found in the data. in this spirit, each 
    // variable term should be compared with another for similary only in terms of the types that the two variables
    // represent, something that has already been taken care of via the mode declarations. In other words, when 
    // comparing 'happensAt(walking(X2),X3)' with 'happensAt(walking(A),B)' the variable names do not matter. So we
    // replace all variables with a common term 'X' as a dummy placeholder.
    val vars = (word: String) =>
      """[A-Za-z0-9_]*""".r.findAllIn(word).toList.filter(x => "[A-Z][A-Za-z0-9_]*".r.pattern.matcher(x).matches())
    val clsList = Utils.readFileToString(kernelFile).split("\\.").toList.filter(x => x != "").map(x => (x, Utils.replInWord(vars(x), x, "X")))

    val out = for ((z, x) <- clsList) yield parseOutput(clause, x) match {
      case y @ Clause(head, body) => LabelledExample(head.asLiteral, body.map(z => z.tostring), z)
      case _ => throw new MyParsingException("Should have been parsed into a Clause.")
    }
    out
  }

  def getPairs(col: MongoCollection) {
    val inittime = col.findOne.get("time")
    val all = col.find()
    var current: Int = inittime.asInstanceOf[Int]; var next = inittime.asInstanceOf[Int] + 40
    var annotation = new ListBuffer[String]()
    var narrative = new ListBuffer[String]()
    try {
      while (all.hasNext) {
        println(current, next)
        if (current == 2480) {
          val stop = "stop"
        }
        val query1 = MongoDBObject("time" -> current)
        val query2 = MongoDBObject("time" -> next)
        val first = col.findOne(query1).get
        val second = col.findOne(query2).get
        narrative = narrative ++ first.asInstanceOf[BasicDBObject].get("narrative").
          asInstanceOf[BasicDBList].toList.map(x => s"$x.");
        narrative = narrative ++ second.asInstanceOf[BasicDBObject].get("narrative").
          asInstanceOf[BasicDBList].toList.map(x => s"$x.");
        annotation = annotation ++ second.asInstanceOf[BasicDBObject].get("annotation").
          asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
        Utils.write((annotation, narrative))
        current = next; next = next + 40
        annotation = new ListBuffer[String]()
        narrative = new ListBuffer[String]()

      }
    } catch {
      case s: NoSuchElementException => System.exit(0)
    }

  }

}