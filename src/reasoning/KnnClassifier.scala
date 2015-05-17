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

object KnnClassifier extends parsing.LogicParser.ClausalLogicParser{

   def getUnlabelledExmpls(db: String, collection: String): Unit = {
      val (supervision,dbcol) = this.getSupervision(db, collection)
      Utils.computeDistancesMany(supervision, dbcol)
   }
   
   def getSupervision(db: String, collection: String): (List[List[String]],MongoCollection) = {
      val dbConnection = Utils.getAllExamples(db,collection) // fetches the examples and returns a collection instance to use later. 
      val (aspResult,buffer) = abduce("modehs",10)
      //println(buffer)
      /* We may have a number of solutions (in optimization tasks), fro which we need the last one */
      val abduced = if(aspResult.length == 1) aspResult(0) else aspResult.last 
      var seenTimes = new ListBuffer[Int]()
      var supervision = new ListBuffer[List[String]]()
      abduced match {
         case _ : List[_] => proceed(abduced.asInstanceOf[List[String]])
         case _ => throw new AbductionException("Problem with the output of the ASP solver.")
      }
      def proceed(atoms: List[String]): Unit = {
         for(atom <- atoms){
            val time = getParseResult(parse(literal,atom)).asInstanceOf[Literal].terms.last.asInstanceOf[Constant].name.toInt
            if(!seenTimes.contains(time)) supervision = supervision :+ Utils.getExample("time", time, dbConnection) 
            seenTimes = if(seenTimes.contains(time)) seenTimes else seenTimes :+ time
         }
         //supervision.foreach(println)
      }
      (supervision.toList,dbConnection)
   }
   
}