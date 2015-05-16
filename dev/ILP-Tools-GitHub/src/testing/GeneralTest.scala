package testing

import scala.io.Source
import scala.util.matching._
import reasoning.Core._
import parsing.LogicParser._
import reasoning.Reasoning._
import reasoning.Structures._
import reasoning.Core

object GeneralTest extends ModesParser{
  
  def main(args: Array[String]){


    
    //Abduction().toAsp()

    //val x =  getParseResult(parseModes(modeAtom,"initiatedAt(fighting(+person,+person),+time)"))
    
    //println(x.tostring)
    
    //println(x.varbed)
     
     //Core.modebs.foreach((x:ModeAtom) => println(x.tostring))
     //Core.modebs.foreach((x:ModeAtom) => println(x.varbed))
    
     Core.examplePatterns.foreach((x:ModeAtom) => println(x.varbed))
     
     //Core.modehs.foreach(x => println(x.varbed))
     
     abduce("modehs",3)

    
  }
  
}