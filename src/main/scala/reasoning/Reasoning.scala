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
import scala.sys.process._
import reasoning.Structures._
import scala.util.control.Breaks
import scala.util.matching.Regex._
import scala.util.matching.Regex
import scala.sys.process.ProcessIO

object Reasoning extends ASPResultsParser {

   def xhail = {
      val abdModel: List[String] = abduce("modehs")(0).asInstanceOf[List[String]]
      val (kernel, varKernel) = generateKernel(abdModel)
      findHypothesis(varKernel)
   }

   def kernelSetOnly: (List[Clause], List[Clause]) = {
      val abdModel: List[String] = abduce("modehs")(0).asInstanceOf[List[String]]
      val (kernel, varKernel) = generateKernel(abdModel)
      (kernel, varKernel)
   }

   /**
    * Prepares the ASP input for abduction and calls the ASP solver to get the results.
    *
    * @param abducibles a flag to indicate where to get the abducible predicates from.
    * Currently the only acceptable flag is "modehs", meaning that abducible predicates
    * are the head mode declaration atoms.
    * @param numberOfModels an upper bound to the number of models. Currently this is not
    * used anywhere.
    * @param useMatchModesProgram @tparam Boolean is true, then this methods creates an additional program that allows
    * to pair an abduced atom with its matching mode atom, on ASP side.
    *
    * @throws AbductionException in case of mistaken or missing abducibles flag.
    *
    * @todo implement the case where abducible predicates are explicitly provided.
    * (see comments in code).
    */

   def abduce(abducibles: Any, numberOfModels: Int = 1000, useMatchModesProgram: Boolean = true): Stream[Any] = {
      def directives = {
         Core.modehs match {
            case Nil => throw new RuntimeException("No Mode Declatations found.")
            case _ =>
               val varbedMHAtoms = for (x <- Core.modehs) yield x.varbed
               val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
               val varbedToStr = varbedMHAtoms.map(x => x.tostring)
               // Generate directives from abducibles
               val generate: List[String] =
                  varbedMHAtoms.map(x => x.tostring + ":" + x.typePreds.mkString(":"))
               val minimize = varbedToStr
               // For abduction we use hard example coverage constraints, instead of constraints
               // based on compression (you can always find an abductiove explanation, despite of the noise).
               val coverageConstr: List[String] = varbedExmplPatterns.map(x =>
                  List(s":- example($x), not $x.", s":- $x, not example($x).")).flatten

               val modeMatchingProgram = if (useMatchModesProgram) matchModesProgram(Core.modehs.map(x => x.varbed)) else List()
               toASPprogram(program = coverageConstr, generateDirectives = generate,
                  minimizeStatements = minimize, extra = modeMatchingProgram, writeToFile = Core.abdFile)
         }
      }
      abducibles match {
         case "modehs" => directives
         /* This is for the case where abducibles are explicitly given. 
          *
          * @todo: Implement this logic 
          * 
          * */
         case _: List[Any] => throw new AbductionException("This logic has not been implemented yet.")
         case _ => throw new AbductionException("You need to specifiy the abducible predicates.")
      }
      val command = List[String](Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09").mkString(" ")
      //println(command)
      runASPSolver("abduction")
   }

   /**
    * Generates a Kernel Set.
    *
    * @param abdModel @tparam List[Literal] the list of atoms previousely abduced.
    * @return the ground and variabilized Kernel Set in a tuple
    *
    * @todo Need to fix the body generation loop: Each constant that corresponds to an output placemarker
    * must be added to the initial (but growing) set of input terms, used to generate instances of body atoms.
    */

   def generateKernel(abdModel: List[String]): (List[Clause], List[Clause]) = {
      def generateQueries(abduced: Literal, interms: List[Expression]): List[(Any, ModeAtom)] = {
         val filterout = (x: String, y: Regex, z: List[String]) =>
            z.filter(e => !y.findAllIn(x).toList.map(q => q.replaceAll("\"", "")).exists(e.contains(_)))
         val p: List[String] = for (x <- interms) yield x.asInstanceOf[Constant]._type + "(" + x.asInstanceOf[Constant].name + ")."
         var accum = new ListBuffer[(Any, ModeAtom)]()
         for (x <- Core.modebs) {
            val varb = x.varbed
            val str = varb.tostringQuote + " :- " +
               filterout(varb.tostringQuote, "\"([A-Za-z0-9_])*\"".r, varb.typePreds).mkString(",") + "."
            toASPprogram(program = p ::: List(str), show = List(varb.tostring), writeToFile = Core.dedFile)
            val q = runASPSolver("getQueries")(0)
            accum += ((q, x))
         }
         accum.toList
      }
      val abducedAtoms: List[Literal] = for (
         x <- abdModel;
         val tolit = Utils.objectFactory.getLiteral(x);
         val (atom, modeAtom) = try {
            (tolit.terms(1), Core.modehs(tolit.terms(0).asInstanceOf[Constant].name.toInt - 1))
         } catch {
            case e: java.lang.ClassCastException => (tolit, tolit.getMatchingMode)
         }
      ) yield Utils.objectFactory.getLiteralNoParse(atom.asInstanceOf[Literal], modeAtom.asInstanceOf[ModeAtom])
      var kernelSet = new ListBuffer[Clause]()
      for (x <- abducedAtoms) {
         var body = new ListBuffer[Literal]()
         val (interms, _, _) = x.getPlmrkTerms
         val queries = generateQueries(x, interms)
         for ((queryList, mAtom) <- queries) {
            val program = abducedAtoms.map(x => x.tostring + ".")
            val show = queryList.asInstanceOf[List[String]].map(x => x.replaceAll("\"", ""))
            toASPprogram(program = program, show = show, writeToFile = Core.dedFile)
            val q = runASPSolver("deduction")(0)
            val b = q.asInstanceOf[List[String]].map(x =>
               Utils.objectFactory.getLiteralNoParse1(x.asInstanceOf[String], mAtom))
            body ++= b
         }
         val kernelClause = Clause(x.asPosLiteral, body.toList)
         kernelSet += kernelClause
      }
      val varKernel = kernelSet.map(x => x.varbed)
      println("\n-----------------------------------")
      println("Kernel Set (Ground---Variabilized):")
      println("------------------------------------")

      kernelSet.foreach(x => println(x.tostring))
      println("")
      varKernel.foreach(x => println(x.tostring))
      (kernelSet.toList, varKernel.toList)
   }

   def findHypothesis(varKernel: List[Clause]): Unit = {
      val get: (List[Clause], collection.mutable.Map[String, Literal]) = useTrySplit(varKernel)
      val defeasibleKS = get._1; val useAtomsMap = get._2
      //println(useAtomsMap)
      //Generate compression-related definitions for example coverage:
      val varbedExmplPatterns = for (x <- Core.examplePatterns) yield x.varbed.tostring
      val coverageConstr: List[String] =
         varbedExmplPatterns.map(x =>
            List(s"\nposNotCovered($x) :- example($x), not $x.", s"\nnegsCovered($x):- $x, not example($x).\n")).flatten
      val program = defeasibleKS.map(x => x.tostring) ::: coverageConstr
      //Create generate directives for use/2 atoms:
      val generateUse =
         for ((cl, clCount) <- varKernel zip List.range(1, varKernel.length + 1)) yield "use(" + clCount + ",0.." + cl.body.length + ")"
      //Generate compression-based minimize statement:
      val minimize = List("use(I,J)") ::: varbedExmplPatterns.map(x => List(s"posNotCovered($x)", s"negsCovered($x)")).flatten
      val constraints = List(List("use(I,J)", "not use(I,0)"))
      val show = List("use(I,J)")
      toASPprogram(program = program, generateDirectives = generateUse,
         minimizeStatements = minimize, constraints = constraints, show = show, writeToFile = Core.indFile)
      println("\n------------------------------------------")
      println("Searching the Kernel Set for a hypothesis:")
      println("--------------------------------------------")
      runASPSolver("induction", useAtomsMap)
   }

   /*-----------------------------------------*
   *-----------------------------------------*
   * Only helper methods from here below. 
   * (related to the generation of ASP input)
   * and calling the ASP solver.
   *-----------------------------------------*
   *-----------------------------------------*/

   /**
    * Turns a Kernel Set into a defeasible program using use/2 and try/3 predicates.
    *
    * @param varKernel @tparam List[Clause] the variabilized Kernel Set
    * @return @tparam (List[Clause],collection.mutable.Map[String,String])
    * where the List[Clause] is the defeasible program as a list of clauses and the map is a map of use/2 atoms to
    * actual literals of the Kernel Set.
    *
    */

   def useTrySplit(varKernel: List[Clause]): (List[Clause], collection.mutable.Map[String, Literal]) = {
      val f = (x: Expression) => x.asInstanceOf[Variable]
      def processClause(clauseBody: List[Literal], clCount: Int): List[(Literal, Clause, Clause, (String, Literal))] = {
         val out = for (
            (lit, litCount) <- clauseBody zip List.range(1, clauseBody.length + 1);
            val (in, out, _) = lit.getPlmrkTerms;
            val vars = in ::: out;
            val _try = Literal("try", List(Constant(clCount.toString), Constant(litCount.toString),
               Literal("vars", (for (x <- vars) yield Variable(f(x).name, _type = f(x)._type)))),
               typePreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")");
            val tryLit = litCount match {
               case 0 => Literal("use", List(Constant(clCount.toString), Constant("0")))
               case _ => _try
            };
            val tryClause1 = Clause(_try.asPosLiteral,
               List(Literal("use", List(Constant(clCount.toString), Constant(litCount.toString))), lit));
            val tryClause2 = Clause(_try.asPosLiteral,
               List(Literal("use", List(Constant(clCount.toString),
                  Constant(litCount.toString)), isNAF = true)) :::
                  (for (x <- _try.typePreds) yield Utils.objectFactory.getLiteral(x)));
            val useMap = ("use(" + clCount + "," + litCount + ")" ->
               Literal(lit.functor, lit.terms, lit.isNAF, typePreds = _try.typePreds, modeAtom = lit.modeAtom))
         ) yield (tryLit, tryClause1, tryClause2, useMap)
         out
      }

      var defeasibleKS = new ListBuffer[Clause]()
      val useAtomsMap = collection.mutable.Map[String, Literal]()
      for ((cl, clCount) <- varKernel zip List.range(1, varKernel.length + 1)) {
         val clauseAnalyzed = processClause(cl.body, clCount)
         val headClauseBody = for (x <- clauseAnalyzed) yield x._1
         val tryClauses1 = for (x <- clauseAnalyzed) yield x._2
         val tryClauses2 = for (x <- clauseAnalyzed) yield x._3

         //======================================================================================================================
         // Set the type predicates for the cl.head atom. They are not set here, I should dela with this
         // generically, have a factory constructor that populates all fields. That's the purpose of object
         // oriented programming: Avoid 9 hours of debugging to get this work, only to result in messy and unmaintainable code!!!
         //=======================================================================================================================   

         val (in, out, _) = cl.head.asLiteral.getPlmrkTerms;
         val vars = in ::: out;
         val typePreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")"
         val _head = Literal(cl.head.functor, cl.head.terms, cl.head.isNAF, modeAtom = cl.head.modeAtom, typePreds = typePreds)

         //val _one = (for (x <- clauseAnalyzed) yield x._4).toMap
         val useMap = collection.mutable.Map((for (x <- clauseAnalyzed) yield x._4).toMap.toSeq: _*) +=
            ("use(" + clCount + "," + "0" + ")" -> _head)
         useAtomsMap ++= useMap

         val useHeadAtom = Literal("use", List(Constant(clCount.toString), Constant("0")))
         val headClauseCarTypes =
            (for (x <- clauseAnalyzed) yield x._1.typePreds).flatten.distinct.map(x => Utils.objectFactory.getLiteral(x))

         val headClause = Clause(_head.asPosLiteral, List(useHeadAtom) ::: headClauseBody ::: headClauseCarTypes)
         defeasibleKS += headClause

         defeasibleKS ++= tryClauses1 ++= tryClauses2
      }
      println("\n-------------------------")
      println("Defeasible Kernel Set :")
      println("--------------------------- ")
      defeasibleKS.foreach(x => println(x.tostring))
      (defeasibleKS.toList, useAtomsMap)
   }

   /**
    * Transforms input to an ASP program. The program is written in an output file that is passed to the ASP solver.
    * the writeTo file is the only non-optional parameter of the method.
    *
    * @param writeToFile @tparam String path to file where the ASP program is written.
    * @param program @tparam List[String] an (optional) set of ground or non-ground rules and/or ground facts.
    * @param generateDirectives @tparam List[String] an (optional) list containing declarations for atoms to be be generated during the computation
    * of answer sets.
    * @example of such input:
    *
    * List("father(X,Y):person(X):person(Y)","grandfather(X,Y):person(X):person(Y)")
    *
    * Such a list is transformed into the "generate" part of the program:
    *
    * {father(X,Y):person(X):person(Y), grandfather(X,Y):person(X):person(Y)}.
    *
    * @param generateAtLeast @tparam Int an (optional) lower bound for the number of generated atoms to be included in an answer set.
    * @param generateAtMost @tparam Int an (optional) upper bound for the number of generated atoms to be included in an answer set.
    * @param minimizeStatements @tparam List[String] an (optional) list of atoms whose instances in an anser set should be minimized.
    * @example of such input:
    *
    * List("father(X,Y)","grandfather(X,Y)"))
    *
    * Such a list is transformed into a minimize statement:
    *
    * #minimize{father(X,Y),grandfather(X,Y)}.
    *
    * @param maximizeStatements @tparam List[String] similar as above for maximize directives.
    * @param constraints @tparam List[List[String]] a set of integrity constraints. Example:
    *
    * List(List("father(X,Y)","mother(X,Y)"), List("father(X,Y)","not male(X)"))
    *
    * Such input is transformed to integrity constraints in the ASP program:
    *
    * :- father(X,Y), mother(X,Y).
    * :- father(X,Y), not male(X).
    *
    * @param show @tparam List[String] an (optional) list of atoms that are to be displayed. All other atoms in an answer set are hidden.
    * A #hide directive is generated is this list is not empty.
    *
    * @example of such input:
    *
    * List("father(X,Y)","mother(X,Y)") or
    *
    * List("father/2","mother2")
    *
    * Such input is transformed into
    *
    *
    * #hide.
    * #show father(X,Y).
    * #show mother(X,Y)
    * @param extra @tparam List[String] any extra knowledge, that is simply printed in the ASP input file
    */

   def toASPprogram(program: List[String] = Nil, generateDirectives: List[String] = Nil,
      generateAtLeast: Int = 1000000000, generateAtMost: Int = 1000000000,
      minimizeStatements: List[String] = Nil, maximizeStatements: List[String] = Nil,
      constraints: List[List[String]] = Nil, show: List[String] = Nil, extra: List[String] = Nil,
      writeToFile: String): Any = {

      Utils.clearFile(writeToFile) // clear here, append everywhere else.
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => program foreach (p.println))
      val genStatems = (generateDirectives, generateAtLeast, generateAtMost) match {
         case x @ (Nil, _, _) => List()
         //case x @ (head :: tail, 1000000000,1000000000) => println(x); x._1.map( y => "{" + y + "}.\n")
         case x @ (head :: tail, 1000000000, 1000000000) =>
            println(x); for (e <- x._1) yield "{" + e + "}."
         case x @ (head :: tail, lower, 1000000000) => (head :: tail).map(y => "$lower {" + y + "}.\n")
         case x @ (head :: tail, 1000000000, upper) => (head :: tail).map(y => "0 {" + y + "} $upper.\n")
         case x @ (head :: tail, lower, upper) => (head :: tail).map(y => "$lower {" + y + "} $upper.\n")
      }
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => genStatems foreach (p.println))
      //genStatems
      val minStatement = minimizeStatements match { // This is a single string
         case Nil => ""
         case _ => "#minimize{ " + minimizeStatements.mkString(",") + "}.\n"
      }
      val maxStatement = maximizeStatements match { // This is a single string
         case Nil => ""
         case _ => "#maximize{ " + maximizeStatements.mkString(",") + "}.\n"
      }
      val constrs = constraints match { // This is a list of strings
         case Nil => List("")
         case _ => for (x <- constraints) yield ":- " + x.mkString(",") + ".\n"
      }
      Utils.writeLine(minStatement, writeToFile, "append")
      Utils.writeLine(maxStatement, writeToFile, "append")
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => constrs foreach (p.println))
      val (hideDir, showDirs) = show match {
         case Nil => ("", List(""))
         case _ => ("#hide.\n", (for (x <- show) yield "#show " + x + "."))
      }
      Utils.writeLine(hideDir, writeToFile, "append")
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => extra foreach (p.println))
      Utils.writeToFile(new java.io.File(writeToFile), "append")(p => showDirs foreach (p.println))
   }

   /**
    * This generates a helper ASP program to extract the mode declaration atoms (if any) that match
    * each atom in an answer set returned by the solver. This helps to process the atoms and populate
    * the objects the are constructed from them as their internal representations. In practice this
    * program computes theta-subsumption between literals.
    *
    * @example This is a (slightly adapted) example from the E.coli case study from:
    *
    * Ray, O. (2009). Nonmonotonic abductive inductive learning. Journal of Applied Logic, 7(3), 329-340.
    *
    * %% Given Mode declarations:
    * -----------------------------------------------
    * modeh(happens(use(#sugar),+time)).
    * modeh(happens(add(#sugar),+time)).
    * modeb(holdsAt(available(#sugar),+time)).
    * modeb(not_holdsAt(available(#sugar),+time)).
    * -----------------------------------------------
    * %% Generate the following program:
    * ----------------------------------------------------------------------------------------------------------
    * mode(1,happens(use(X),Y)) :- sugar(X),time(Y). %% one atom for each mode, counting them with the 1st arg.
    * mode(2,happens(add(X),Y)) :- sugar(X),time(Y).
    * mode(3,holdsAt(available(X),Y)) :- sugar(X),time(Y).
    * mode(4,not_holdsAt(available(X),Y)) :- sugar(X),time(Y).
    *
    * modeCounter(1..4).
    *
    * matchesMode(ModeCounter,Atom,Mode) :-
    *     mode(ModeCounter,Atom), mode(ModeCounter,Mode), true(Atom), Atom = Mode.
    *
    * %% Add one such rule for each predicate (mode atom) you want to query. The purpose is to
    * %% is to generate matchesMode/3 instances only for atoms that are included in an
    * %% answer set (i.e. true atoms), in order to avoid huge amounts of irrelevant info.
    *
    * true(happens(use(X),Y)) :- happens(use(X),Y).
    * true(happens(add(X),Y)) :- happens(add(X),Y).
    * true(holdsAt(available(X),Y)) :- holdsAt(available(X),Y).
    * true(holdsAt(not_available(X),Y)) :- holdsAt(not_available(X),Y).
    *
    * #hide.
    * #show matchesMode/3.
    * ---------------------------------------------------------------------------------------------------------
    *
    * An atom 'matchesMode(m,atom,_)'' in an answer set of this program is interpreted as a true atom
    * that matches with mode atom 'm'.
    *
    * @param queryPreds @tparam List[Literal] the predicates we want to query. The helper
    * program will be generated only for these predicates, to avoid unnecessary information.
    * The input muct be in the FORM OF A LIST OF **VARIABILIZED** literals (thus carrying their typing info).
    */
   def matchModesProgram(queryModePreds: List[Literal]): List[String] = {
      val modeDecl: List[String] = for (
         x <- queryModePreds;
         y <- List.range(1, queryModePreds.length + 1) zip queryModePreds
      ) yield "mode(" + y._1 + "," + x.tostring + "," + y._2.tostring + ") :- " + x.typePreds.mkString(",") + "."
      val modeCount: String = "modeCounter(1.." + queryModePreds.length + ")."
      val clause = """matchesMode(ModeCounter,Atom,Mode) :- 
       mode(ModeCounter,Atom, Mode), true(Atom), Atom = Mode."""
      val trues: List[String] = for (x <- queryModePreds) yield "true(" + x.tostring + ")" + " :- " + x.tostring + "."
      val program = modeDecl ::: List(modeCount) ::: List(clause) ::: trues ::: List("#hide.", "#show matchesMode/3.")
      //program.foreach(println)
      program
   }

   /**
    * Calls the ASP solver and returns the results.
    *
    * @param task @tparam String an indicator for what we want to do with the solver, See the code for details. New task may be
    * added easily by providing the proper input to the ASP solver.
    * @todo Error handling needs fixing. Rightnow we do not intercept
    * the stderr stream, but we flag an error in case of an empty model.
    *
    * FIX THIS!!!
    */

   def runASPSolver(task: String, useAtomsMap: collection.mutable.Map[String, Literal] = collection.mutable.Map()): Stream[Any] = {
      val buffer = new StringBuffer()
      val command = task match {
         case "abduction" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.abdFile, "0", "--asp09")
         case "getQueries" => Seq(Core.aspSolverPath + "/./clingo", Core.dedFile, "0", "--asp09")
         case "getGroundings" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.dedFile, "0", "--asp09")
         case "deduction" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.dedFile, "0", "--asp09")
         case "induction" => Seq(Core.aspSolverPath + "/./clingo", Core.bkFile, Core.exmplsFile, Core.indFile, "0", "--asp09")
      }

      /*
       * Utilities for forming a hypothesis from use/2 atoms
       * ---------------------------------------------------------------------------------------------------------
       * ---------------------------------------------------------------------------------------------------------
       */

      val tolit = (atom: String) => Utils.objectFactory.getLiteral(atom)
      val getClauseIndex = (atom: String) => tolit(atom).terms(0).asInstanceOf[Constant].name
      val getLiteralIndex = (atom: String) => tolit(atom).terms(1).asInstanceOf[Constant].name

      def formHypothesis(answerSet: List[String], hypCount: Int): (List[Clause], List[Clause]) = {
         val headAtoms = answerSet.filter(x => getLiteralIndex(x) == "0").toList
         val out = for (x <- headAtoms) yield formHypothesisClause(x, answerSet)
         val (reasoningClauses, showClauses) = (for (x <- out) yield x._1, for (x <- out) yield x._2)
         Utils.writeToFile(new java.io.File(Core.theoryFile), "overwrite")(p =>
            showClauses foreach (x => p.println(x.asInstanceOf[Clause].tostring)))
         Utils.writeToFile(new java.io.File(Core.theoryCrossvalFile), "overwrite")(p =>
            reasoningClauses foreach (x => p.println(x.asInstanceOf[Clause].tostring)))
         println("\n---------------------------------")
         println("Enumerated hypothesis " + hypCount + ":")
         println("-----------------------------------")
         showClauses.foreach(x => println(x.asInstanceOf[Clause].tostring))
         Crossvalidation.run(Core.theoryCrossvalFile, Core.exmplsFile, Core.crossvalFile)
         println("-----------------------------------")
         (reasoningClauses, showClauses)
      }

      def getHeadAtoms(answerSet: List[String]) = {
         answerSet.filter(x => getLiteralIndex(x) == "0").toList
      }

      def formHypothesisClause(headUseAtom: String, answerSet: List[String]): (Clause, Clause) = {
         val currentClauseIndex = getClauseIndex(headUseAtom)
         val bodyAtoms = answerSet.filter(x => getClauseIndex(x) == currentClauseIndex && getLiteralIndex(x) != "0").toList
         // This is the clause to display
         val clauseShow = Clause(useAtomsMap(headUseAtom).asPosLiteral, for (x <- bodyAtoms) yield useAtomsMap(x))
         // This is the clause to reason with, having also type variables:
         val body = for (x <- bodyAtoms) yield useAtomsMap(x)
         val headTypes = for (x <- useAtomsMap(headUseAtom).typePreds) yield x
         val bodyTypes = for (x <- body; y <- x.typePreds) yield y
         val types = (headTypes ::: bodyTypes).toSet.toList
         val _types = for (x <- types) yield Utils.objectFactory.getLiteral(x)
         val clauseReason = Clause(useAtomsMap(headUseAtom).asPosLiteral, body ::: _types)

         (clauseReason, clauseShow)
      }

      /*
       * -----------------------------------------------------------------------------------------------------------
       * -----------------------------------------------------------------------------------------------------------
       */
      var hypCount: Int = 1
      val processLine = (x: String, y: Int) => parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
         case Success(result, _) => result match {
            case List() =>
               println("\n---------------------------------")
               println("Enumerated hypothesis " + hypCount + ":")
               println("Empty hypothesis")
               println("-----------------------------------"); hypCount += 1
            case _ => formHypothesis(result, y); hypCount += 1

         }
         case f => None
      }
      task match {
         case "induction" =>
            //command.!; Stream[Any]()
            val pio =
               new ProcessIO(_ => (),
                  stdout => scala.io.Source.fromInputStream(stdout).getLines.foreach(x => processLine(x, hypCount)),
                  stderr => scala.io.Source.fromInputStream(stderr).getLines.foreach(println))
            command.run(pio)
            Stream[Any]()
         case _ =>
            val allLines = Process(command).lineStream
            val lines = allLines match {
               case Stream() =>
                  throw new ASPInputException(buffer.toString())
               case _ =>
                  (for (x <- allLines) yield parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
                     case Success(result, _) => result
                     case f => None
                  }).filter(x => x != None)
            }
            println("\n\n--------------------------")
            println(task + "| Models found:")
            println("--------------------------")
            println(lines)
            lines
      }
      /*
      val allLines = Process(command).lineStream
      val lines = allLines match {
         case Stream() =>
            throw new ASPInputException(buffer.toString())
         case _ =>
            (for (x <- allLines) yield parseAll(aspResult, x.replaceAll("." + "\\s", "")) match {
               case Success(result, _) => result
               case f => None
            }).filter(x => x != None)
      }
      println("\n\n--------------------------")
      println(task + "| Models found:")
      println("--------------------------")
      println(lines)
      lines
      */
   }

}