package reasoning

import java.io.FileWriter

object Utils {

   /* Usage:
    * 
    * writeToFile(new File("example.txt")) { p => data.foreach(p.println) }
    * 
    */
   
   def writeToFile(f: java.io.File, howTowrite: String)(op: java.io.PrintWriter => Unit) {
      val p = howTowrite match{
         case "append" => new java.io.PrintWriter( new FileWriter(f, true) )
         case "overwrite" => new java.io.PrintWriter( new FileWriter(f, false) ) 
         case _ => new java.io.PrintWriter( new FileWriter(f, false) ) // default is overwrite
      } 
      try { op(p) } finally { p.close() }
   }

}