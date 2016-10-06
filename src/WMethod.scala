/**
  * Created by lucatrubbiani on 15/07/2016.
  */
import java.io.{File, PrintWriter}

import fsm._
import wmethod._
import utils._

import scala.collection.mutable.{ListBuffer, _}

/**
  * 1) Estimate the maximum nubmber of state in correct design
  * 2) Construct the characterization W set for the give machine M
  * 3) Construct the testing Tree for M and determine the transition cover set P
  * 4) Construct set Z
  * 5) P*Z is the desired test set
  */
object WMethod {
  type TRow = (String, Set[(String,Any)],Set[(String,(String,Int))])
  type TGroup = ListBuffer[(Int,ListBuffer[TRow])]

  def main(args: Array[String]): Unit = {
    if(args.length > 0){
      if(args(0).equals("--debug")) {
        Options.debug = true
      }
    }
    // Initialize class for report generation
    val report = new Report

    println("Generate the big machines Test!")
    var theBigMachineTest = TheBigMachineTest
    var machines = theBigMachineTest.getMachines

    report.printMachines(ListBuffer(machines._1))
    report.printMachines(machines._2)

    // Generate k equivalence partition table
    var ke:KEquivalence = new KEquivalence(machines._1.oStates)
    var tables = ke.doKEquivalence

    report.printEquivalenceTable(tables)

    //Build the chracterization set for the correct model M
    var characterizationSet = new CharacterizationSet
    var wSet = characterizationSet.doCharacterizationSet(tables)
    if(characterizationSet.distinguishingSequence.size == 0) {
      System.exit(1)
    }
    report.printWSet(wSet)

    // Build transition cover set for the correct model M
    var trasitionConverSet = new TransitionCoverSet
    var pSet = trasitionConverSet.doTransitionTree(tables)

    report.printPSet(pSet)

    //Sequentially run test given the model and the IUTs
    for(iut <- machines._2){

      // Build the Z set
      var zSet = new ZSet
      var z = zSet.doZSet(iut.states.size, machines._1.states.size, characterizationSet.wSet, machines._1.alphabet)

      // Build TSet
      var testSet = new TestSet
      var t = testSet.doZSet(trasitionConverSet.pSet, zSet.zSet)

      // Execute oracle on IUT
      var oracle = new Oracle
      val result = oracle.doOracle(machines._1,iut,testSet.reducedTSet)

      // Store Test results
      report.writeResultInFile(iut,result._2)
      report.printZSet(z)
      report.printTSet(t._2,t._1.size,t._2.size)
    }
    report.closeAndWrite

    println("End procedure ^_^")
    println("Report generated")
    //Not used!
    //var implicationChartMethod = new ImplicationChartMethod
    //implicationChartMethod.minimize(fsm)
  }
}

/**
  * Cose da fare
  *
  * terminare la generazione delle macchine e di alcuni mutanti.
  * Report
  *   argomenti
  *   - introduzione fsm , tipologia e mutanti
  *   - algoritmo
  *   - idee implementative
  *   - alcuni test e risulati magari formattati in maniera decente
  */

object TheBigMachineTest {

  def getMachines : (Fsm, ListBuffer[Fsm]) = {
    // The correct model
    var fsm = Fsm("Correct Model");
    fsm.alphabet = Set("a","b")
    fsm.oAlphabet = Set("0","1")
    fsm.setDebugMode(Options.debug)
    fsm.addState("q0")
    fsm.addState("q1");
    fsm.addTransition(Transition("a","q0","q0",(e) => 1))
    fsm.addTransition(Transition("b","q0","q1",(e) => 1))
    fsm.addTransition(Transition("a","q1","q0",(e) => 1))
    fsm.addTransition(Transition("b","q1","q1",(e) => 0))


    // Operation Error
    var t1 = Fsm("t1");
    t1.alphabet = Set("a","b")
    t1.oAlphabet = Set("0","1")
    t1.setDebugMode(Options.debug)
    t1.addState("q0")
    t1.addState("q1");
    t1.addTransition(Transition("a","q0","q0",(e) => 0))
    t1.addTransition(Transition("b","q0","q1",(e) => 1))
    t1.addTransition(Transition("a","q1","q0",(e) => 1))
    t1.addTransition(Transition("b","q1","q1",(e) => 0))

    // Transfer error
    var t2 = Fsm("t2");
    t2.alphabet = Set("a","b")
    t2.oAlphabet = Set("0","1")
    t2.setDebugMode(Options.debug)
    t2.addState("q0")
    t2.addState("q1");
    t2.addTransition(Transition("a","q0","q0",(e) => 1))
    t2.addTransition(Transition("b","q0","q0",(e) => 1))
    t2.addTransition(Transition("a","q1","q0",(e) => 1))
    t2.addTransition(Transition("b","q1","q1",(e) => 0))


    // Extra state error
    var t3 = Fsm("t3");
    t3.alphabet = Set("a","b")
    t3.oAlphabet = Set("0","1")
    t3.setDebugMode(Options.debug)
    t3.addState("q0")
    t3.addState("q1");
    t3.addState("q2");
    t3.addTransition(Transition("a","q0","q0",(e) => 1))
    t3.addTransition(Transition("b","q0","q2",(e) => 1))
    t3.addTransition(Transition("a","q1","q0",(e) => 1))
    t3.addTransition(Transition("b","q1","q1",(e) => 0))
    t3.addTransition(Transition("a","q2","q1",(e) => 1))
    t3.addTransition(Transition("b","q2","q2",(e) => 0))


    // Miss state
    var t4 = Fsm("t4");
    t4.alphabet = Set("a","b")
    t4.oAlphabet = Set("0","1")
    t4.setDebugMode(Options.debug)
    t4.addState("q0")
    t4.addTransition(Transition("a","q0","q0",(e) => 1))
    t4.addTransition(Transition("b","q0","q0",(e) => 0))


    // Extra state Not recognize ... extra state but same behaviour
    var t5 = Fsm("t5");
    t5.alphabet = Set("a","b")
    t5.oAlphabet = Set("0","1")
    t5.setDebugMode(Options.debug)
    t5.addState("q0")
    t5.addState("q1");
    t5.addState("q2");
    t5.addTransition(Transition("a","q0","q0",(e) => 1))
    t5.addTransition(Transition("b","q0","q1",(e) => 1))
    t5.addTransition(Transition("a","q1","q0",(e) => 1))
    t5.addTransition(Transition("b","q1","q2",(e) => 0))
    t5.addTransition(Transition("a","q2","q0",(e) => 1))
    t5.addTransition(Transition("b","q2","q2",(e) => 0))


    return (fsm, ListBuffer(t1,t2,t3,t4,t5))
  }
}