/**
 *
 */
package testing

/**
 * @author nkatz
 *
 */

import reasoning.Structures._
import reasoning.Utils._
import reasoning.Utils
import reasoning.Reasoning._
import reasoning.Core
import scala.util.matching.Regex._
import scala.util.matching.Regex
import parsing.LogicParser._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props


object GeneralTest extends ModesParser{

  def main(args: Array[String]) {
    /*
    val m = reasoning.Utils.objectFactory.getModeHAtom("modeh(initiatedAt(moving(+person,+person,-person),+time))")
    println(m)
    val l = reasoning.Utils.objectFactory.getLiteral("initiatedAt(moving(id1,id2,id1),10)")
    println(l.tostring)
    val ll = Literal(l.functor, l.terms, l.isNAF, modeAtom = m)
    println(ll)
    println(ll.varbed._1.tostring)
    */
    
    /* 
    // Test Clause variabilization.

    val head = reasoning.Utils.objectFactory.getLiteral("initiatedAt(moving(id1,id2,id1),10)",
      "modeh(initiatedAt(moving(+person,+person),+time))")
    val body1 = reasoning.Utils.objectFactory.getLiteral("happensAt(walking(id1),10)",
      "modeb(happensAt(walking(-person),+time))")
    val body2 = reasoning.Utils.objectFactory.getLiteral("happensAt(running(id12),10)",
      "modeb(happensAt(running(+person),+time))")

    val clause = Clause(head.asPosLiteral, List(body1, body2))

    println(clause.varbed.tostring)
    clause.toLiteralList.foreach(x => println(x.modeAtom))
    */

    
    // Parse all caviar in DB
    //Utils.caviartoMongo("/home/nkatz/dev/CAVIAR-abrupt") 
    

    //val m = reasoning.Utils.objectFactory.getModeHAtom("modeh(initiatedAt(moving(+person,+person,test(#ss,#ss),-person),+time))")
    //println(m)
    //val z = m.varbed
    //println(z)
    //println(z.tostringQuote)
    //println(m.varbed.tostring)
    //println(m.varbed.tostringQuote)

    //val l = reasoning.Utils.objectFactory.getLiteral("initiatedAt(moving(id1,id2,test(x,y),id1),10)")
    //println(l.tostring)
    //val ll = Literal(l.functor, l.terms, l.isNAF, modeAtom = m)
    //println(ll)
    //println(ll.getPlmrkTerms)


    /* Test Abduction */ 
    //abduce("modehs")
    /* Test Kernel Set generation */ 
    //generateKernel
    //val z = Core.modehs.map(x => x.varbed)
    //matchModesProgram(z)
    
     
     //xhail
     
     getAllExamples("CAVIAR-01-Walk1", "examples")
     
     //val l = reasoning.Utils.objectFactory.getLiteral("happensAt(walking(X),Test)")
     //println(l.getMatchingMode.tostring)    
    
     //parseOutput(literal,"holdsAt(available(\"X0\"),1)")

    
    //Utils.getAllExamples("CAVIAR-01-Walk1", "examples")


  }

}