/**
 * @author Nikos Katzouris
 *
 */

package reasoning

import scala.io.Source
import scala.util.matching._
import reasoning.Core._
import scala.util.parsing.combinator._
import parsing.LogicParser._
import scala.collection.mutable.ListBuffer
import reasoning.Exceptions._
import reasoning.Utils._

object Reasoning {

   
   
   def abduce(abducibles: Any, numberOfModels: Int): Unit = {
      abducibles match {
         case "modehs" => 
            Utils.writeToFile(new java.io.File(Core.abdFile),"overwrite")(p => 
               (for (x <- Core.modehs; val atom = x.varbed._1; val types = x.varbed._2.mkString(":")) 
                  yield s"0{$atom:$types}$numberOfModels.") foreach(p.println) )
            // Generate the example constraints:
            Utils.writeToFile(new java.io.File(Core.abdFile),"append")(p => 
               (for (x <- Core.examplePatterns; val atom = x.varbed._1; val types = x.varbed._2.mkString(":")) 
                  yield s"0{$atom:$types}$numberOfModels.") foreach(p.println) )      
            
         /* This is for the case where abducibles are explicitly given. 
          *
          * @TODO: Implement this logic 
          * 
          * */   
         case _ : List[Any] => throw new AbductionException("This logic has not been implemented yet.")    
         case _ => throw new AbductionException("You need to specifiy the abducible predicates.")   
      }
   }

}