package fsm

import scala.collection.mutable.Map

/**
  * State object
  */
case class State(name:String) {
  var transitions: Map[String, Transition] = Map()
  var epsilonTransitionState: String = null

  def addTransition(trans: Transition): Unit = {
    transitions += trans.evtName -> trans
  }
}
