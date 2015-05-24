package testing

import reasoning.Structures._
import parsing.LogicParser.ClausalLogicParser

object ParserTesting extends ClausalLogicParser {
/*
   def main(args: Array[String]) {
      /*
    println(parseAll(variable,"Y"))
    println(parseAll(variable,"Time"))
    println(parseAll(variable,"Time_donT_43_matTEr_no_More"))
    println(parseAll(predicate,"a(x,y,9,a(t))"))
    println(parseAll(clauseBody,"holdsAt(punctuality(X,Y,punctual,f(f(f(g(XXX))))),T)"))
    println(parseAll(iff," :-"))
    println(parseAll(clause,"a(X) :- b(Z),c(K,n,b(X,Y,Z)),rZ_rt3547ndbXDRT_(t,e,r_t)"))
    */

      //println(parse(literal, "not initiatedAt(fighting(id1,id2),15)"))

      //val x = "initiatedAt(fighting(id1,id2),15) :- not test(X,Z),test(Y)"
      val y = "initiatedAt(fighting(X,Y),Z):-test(A,B,test(1,p(R,R)),C),q(R,E,K)"
         
         

      //val c = getParseResult(parse(clause, x))
      val c2 = getParseResult(parse(clause, y))

      
      //c.asInstanceOf[Clause].skolemize
      println(c2.asInstanceOf[Clause].skolemise._1.tostring)
      //parse(literal,"")
   }
*/
}


