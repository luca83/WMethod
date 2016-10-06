package fsm

import java.util.NoSuchElementException

import scala.collection.mutable.{ListBuffer, Map, Set}

/**
  * Fsm class
 *
  * @param name
  */
case class Fsm(name:String) {

  var currentState:String = ""
  var states:Map[String, State] = Map()
  var oStates:ListBuffer[(String,State)] = ListBuffer()
  var debug:Boolean = false
  var alphabet:Set[String] = Set()
  var oAlphabet:Set[String] = Set()
  var initialState:String = ""

  // Used for get values after execution
  var fsmOutput:String = ""
  var fsmTransitions:ListBuffer[String] = ListBuffer()

  def setDebugMode(debug:Boolean)= this.debug = debug

  def getState: String = currentState

  def addState(state:String) : Unit = {
    val isInitial:Boolean = states.size == 0
    if(!states.contains(state)){
      var s = State(state)
      states+= state -> s
      oStates+= ((state,s))
    }
    if(isInitial){
      setState(state)
      initialState = state
      fsmTransitions+=(state)
    }
  }

  //Do epsilon transition
  def setAutoTransition(startState:String  , endState:String) {
    states(startState).epsilonTransitionState = endState
    addTransition(new Transition("(epsilon)", startState, endState, (e) => "" ))
  }

  def setState(state: String): Unit = {
    currentState = state
  }


  def addTransition(trans:Transition) {
    try {
      val st: State = states(trans.startState)
      st.addTransition(trans)
    }catch{
      case e:NoSuchElementException => {
        e.printStackTrace
        println("State: " +trans.startState+ " not found in FSM -> procedure will stop!");
        System.exit(1)
      }
    }
  }

  def addEvent(evtName:String): Any =  {
    try{
      val state:State = states(currentState)
      val trans:Transition = state.transitions(evtName)
      if (debug) {
        println("FSM", "Event: " + evtName + ", Output: "+trans.output(evtName)+ ", " + trans.startState +
          " --> " + trans.endState );
      }

      val output = trans.output(evtName)
      fsmOutput = fsmOutput + output.toString
      fsmTransitions+=(trans.endState)

      setState(trans.endState)
      if (states(trans.endState).epsilonTransitionState != null) {
        if (debug) {
          println("FSM", "Automatically transitioning from " +
            trans.endState + " to "
            + states(trans.endState).epsilonTransitionState);
        }
        addEvent("(epsilon)")
      }
      // return output
      output
    }catch{
      case e:NoSuchElementException => {
        if(!states.contains(currentState) )
          throw NoStateFound(currentState+" cannot be found in the state list")
        else
          throw NoTransitionFound("No transition ("+evtName+") found in state " +currentState)
      }
    }
  }

  /**
    * Print state transitions
    * @param state
    */
  def printTransitions(state:String): Unit = {
    val s:State = states(state)
    s.transitions.keys.foreach { t =>
      val trans = s.transitions(t)
      if (debug) {
        println("(" + trans.startState + ") --> evt: " + trans.evtName + " / out: " + trans.output(trans.evtName) + "--> (" + trans.endState + ")")
      }
    }
  }

  def reset: Unit ={
    currentState = initialState
    fsmOutput = ""
    fsmTransitions = ListBuffer(initialState)
  }
}
