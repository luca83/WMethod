package fsm

/**
  *
  * @param evtName
  * @param startState
  * @param endState
  * @param f
  */
case class Transition(evtName: String, startState: String, endState: String, f:(Any) => Any ){

  def output(input:String): Any ={
    f(input)
  }
}
