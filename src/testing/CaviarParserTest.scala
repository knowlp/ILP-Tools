package testing

import com.mongodb.casbah.Imports.BasicDBList
import com.mongodb.casbah.Imports._
import parsing.MongoDB._
import parsing.ProcessCaviar._
import reasoning.Exceptions._

object CaviarParserTest extends CaviarDataParser {

  def main(args: Array[String]) {
    /*
    parseCaviar("/home/nkatz/dev/CAVIAR-abrupt/01-Walk1")
    val collection = MongoClient()("CAVIAR")("examples")
    collection.find().foreach(row => println(row))
    */
    val collection = MongoClient()("CAVIAR")("examples")

    /*
     * This (getExample) should be turned into a generic method. The fields that 
     * should be retrieved should be given as arguments (and based on that, 
     * create the lists merged or not). 
     */
    
    def getExample(field: String, fieldValue: Any): List[String] = {
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

    
    def f(row: collection.T): List[String] ={
      val x = row.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList
      val y = row.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList
      val example = x ++ y map {x => x.asInstanceOf[String]}
      example
    }
    
    val e = getExample("time",22400)
    println(e)
    collection.find().foreach(row => 
      try{
        println(reasoning.Logic.exmplHausdrfDist(e, f(row)))
      }catch{
        case e: MyParsingException => f(row).foreach(println); println(e) ; throw new RuntimeException
      }
    )
    

    }

}