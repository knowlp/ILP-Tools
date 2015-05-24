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
import reasoning.Reasoning._

object GeneralTest {

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

    /* Test Abduction */ 
    //abduce("modehs")
    /* Test Kernel Set generation */ 
    generateKernel

  }

}