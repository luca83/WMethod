package test

import fsm._
import wmethod._
import org.junit.runner.RunWith
import utils._

import collection.mutable.Set
import org.scalatest.{BeforeAndAfter, FlatSpec, FunSuite}
import org.scalatest.junit.JUnitRunner
/**
  * Created by lucatrubbiani on 21/07/2016.
  *
  * An FSM should be
  * 1) Completely specified
  * 2) Minimal
  *
  */
@RunWith(classOf[JUnitRunner])
class FsmSpec extends FunSuite with BeforeAndAfter {
  //Options.debug = true
  var fsm = Fsm("FSM");
  var iut = Fsm("IUT");

  before{
    //generate correct design
    fsm.alphabet = Set("a","b")
    fsm.oAlphabet = Set("0","1")
    fsm.setDebugMode(true)
    fsm.addState("q1")
    fsm.addState("q2")
    fsm.addState("q3")
    fsm.addState("q4")
    fsm.addState("q5")
    fsm.addTransition(Transition("a","q1","q1",(e) => 0))
    fsm.addTransition(Transition("b","q1","q4",(e) => 1))
    fsm.addTransition(Transition("a","q2","q1",(e) => 0))
    fsm.addTransition(Transition("b","q2","q5",(e) => 1))
    fsm.addTransition(Transition("a","q3","q5",(e) => 0))
    fsm.addTransition(Transition("b","q3","q1",(e) => 1))
    fsm.addTransition(Transition("a","q4","q3",(e) => 1))
    fsm.addTransition(Transition("b","q4","q4",(e) => 1))
    fsm.addTransition(Transition("a","q5","q2",(e) => 1))
    fsm.addTransition(Transition("b","q5","q5",(e) => 1))


    //generate possibile implementation
    iut.alphabet = Set("a","b")
    iut.oAlphabet = Set("0","1")
    iut.setDebugMode(true)
    iut.addState("q1")
    iut.addState("q2");
    iut.addState("q3");
    iut.addState("q4");
    iut.addState("q5");
    iut.addTransition(Transition("a","q1","q1",(e) => 0))
    iut.addTransition(Transition("b","q1","q4",(e) => 1))
    iut.addTransition(Transition("a","q2","q1",(e) => 0))
    iut.addTransition(Transition("b","q2","q5",(e) => 1))
    iut.addTransition(Transition("a","q3","q5",(e) => 0))
    iut.addTransition(Transition("b","q3","q1",(e) => 1))
    iut.addTransition(Transition("a","q4","q3",(e) => 1))
    iut.addTransition(Transition("b","q4","q4",(e) => 1))
    iut.addTransition(Transition("a","q5","q2",(e) => 1))
    iut.addTransition(Transition("b","q5","q5",(e) => 1))

  }

  test("FSMs should have an input alphabet with at least one symbol"){
    assert(fsm.alphabet.size > 0 )
    assert(iut.alphabet.size > 0 )
  }

  test("FSMs should have an output alphabet with at least one symbol"){
    assert(fsm.oAlphabet.size > 0 )
    assert(iut.oAlphabet.size > 0 )
  }

  test("FSMs should be completely specified"){
    for((n,s) <- fsm.states ){
      fsm.alphabet.foreach( a => assert(s.transitions.contains(a)))
    }
    for((n,s) <- iut.states ){
      iut.alphabet.foreach( a => assert(s.transitions.contains(a)))
    }
  }

  test("FSMs should be deterministic"){
    /*
    * TRUE for the FSM engine implementation
    */
  }

  test("FSMs should be minimal"){
    var implicationChartMethod = new ImplicationChartMethod
    val redStateNumForFSM = implicationChartMethod.minimize(fsm)
    //assert(redStateNumForFSM == fsm.states.size)


    val redStateNumForIUT = implicationChartMethod.minimize(iut)
    //assert(redStateNumForIUT == iut.states.size)


  }

  test("FSMs can be reset accurately to the initial state"){
    fsm.reset
    assert(fsm.currentState == fsm.initialState )
    iut.reset
    assert(iut.currentState == iut.initialState )
  }

  test("FSMs have the same input alphabet"){
    assert(fsm.alphabet.equals(iut.alphabet)  )
  }

  after{
    // Do something
  }
}
