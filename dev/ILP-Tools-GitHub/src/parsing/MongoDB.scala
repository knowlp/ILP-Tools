/**
 * @author Nikos Katzouris
 *
 */

package parsing

import com.mongodb.casbah.Imports._

object MongoDB {

  def addToBD(dbName: String, colName: String): Unit = {

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
    
     //val result = List[String]() ++ target map { x => x.asInstanceOf[String] }
    //result
   // target
  }
  

}