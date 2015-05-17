package main

  /*
    *| --------------------------------------------------------------------------------------------|
    *| Fix mongo connectivity issues:                                                              |
    *| -- Manually remove the lockfile: sudo rm /var/lib/mongodb/mongod.lock                       |
    *| -- Run the repair script: sudo -u mongodb mongod -f /etc/mongodb.conf --repair              |
    *| -- Start your MongoDB server with sudo start mongodb and verify it is running with sudo     |
    *|    status mongodb and by trying to connect to it with mongo test.                           |
    *| --------------------------------------------------------------------------------------------|
    */




import scala.io.Source
import scala.util.matching._
import reasoning._
import com.mongodb.casbah.Imports._
import scala.sys.process._
import java.io.File
import scala.util.matching._
import parsing.ProcessCaviar.CaviarDataParser
import reasoning.Utils._

/**
 * This is the entry point. It should be runnable as a script, by passing cmd args.
 * Some tasks:
 * -- generate a new db, store a dataset it it
 * -- Store CAVIAR ( parseCaviar("/home/nkatz/dev/CAVIAR-abrupt/01-Walk1") )
 * -- get all examples from a db
 * -- get one/some example/examples from a db
 * -- obtain supervision (and show it somewhere to inspect it)
 * -- Learn with ALEPH
 * -- Learn with XHAIL
 * -- ......
 */



object Main extends App {
 
   //val dir = "ls /home/nkatz/dev/CAVIAR-abrupt/"
   //val buffer = new StringBuffer()
   //val allLines = dir lines_! ProcessLogger(buffer append _)
   //println(allLines,buffer)
   
   
   
   caviartoMongo("/home/nkatz/dev/CAVIAR-abrupt") 
   
   /**
    * This does not work yet. I get a 
    * 
    * scala.MatchError at video 06-Browse3, with message "occluded (of class java.lang.String)".
    * Also I get a
    * 
    *  com.mongodb.WriteConcernException: { "serverUsed" : "127.0.0.1:27017" , "ok" : 1 , "n" : 0 , "err" : "can't map file memory - mongo requires 64 bit build for larger datasets" , "code" : 10084}
    * 
    * error at 11-Rest_SlumpOnFloor
    */
   
   //caviartoMongo("/home/nkatz/dev/CAVIAR-abrupt") 
   
   def caviartoMongo(path: String): Unit = {
      val splitPath = path.split("/").filter(x => x != "\\s" | x != "") // handle '/' in the path input
      val dirs = for (file <- new File(path).listFiles.sorted) yield file.getName()
      dirs.foreach( x =>  
         try {
            Utils.parseCaviar(splitPath.mkString("/")+"/"+x) 
         } catch {
            case e : scala.MatchError => println(e.getMessage)
         } )
   }
  
   //KnnClassifier.getUnlabelledExmpls("CAVIAR-01-Walk1", "examples")
   
}