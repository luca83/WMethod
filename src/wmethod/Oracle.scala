package wmethod

import fsm._
import utils.Options

import scala.collection.mutable.{ListBuffer, Set}
/**
  * Created by lucatrubbiani on 20/07/2016.
  *
  * Fault model
  *
  * 1) Operation error
  * 2) Transfer error
  * 3) Extra state error can be or not an error
  * 4) Miss state error
  *
  */
trait TOracle{
  type TResult = ListBuffer[(String,String,String,String,String,String,String)]
}
class Oracle extends TOracle{

  var testResults:TResult= ListBuffer()
  var mTransitions:ListBuffer[String] = ListBuffer()
  var iutTransitions:ListBuffer[String] = ListBuffer()
  var distinguishable:Boolean = false

  def doOracle( m:Fsm, iut:Fsm ,test:Set[String]): (Boolean,TResult) ={
    for(t <- test){
      runTest(m,iut,t)
      m.reset
      iut.reset
      mTransitions = ListBuffer()
      iutTransitions = ListBuffer()
    }
    if(Options.debug){
      println("-----------------------------------------------------------" )
      println("Input\t\t|\t\tOutput M\t\t|\t\tOutput IUT\t\t Equal?\t\tError type\t\t M Trace path\t\t IUT Trace path")
      for(t <- testResults){
        println(t._1+"\t|\t\t"+t._2 +"\t\t|\t\t"+ t._3+"\t\t|\t\t"+t._4+"\t\t|\t\t"+t._5+"\t\t|\t\t"+t._6+"\t\t|\t\t"+t._7)
      }
    }
    (distinguishable,testResults)
  }

  private def runTest(m:Fsm, iut:Fsm ,input:String):Unit = {
    var errorType:String = "<span style='color:green'>NONE</span>"
    for(i<- input){
      try{
        m.addEvent(i.toString)
        mTransitions+=(m.currentState)
        iut.addEvent(i.toString)
        iutTransitions+=(iut.currentState)
      }catch{
        case e:NoTransitionFound => {
          println(e.message)
          errorType = "MISS TRANSITION"
        }
        case e:NoStateFound => {
          println(e.message)
          errorType = "MISS STATE"
        }
      }
    }
    if(m.fsmOutput.equals(iut.fsmOutput)){
      if(!mTransitions.equals(iutTransitions) ){
        errorType ="<span style='color:orange'>NONE <br /> BUT DIFFERENT PATH</span>"
      }
      testResults+=((input, m.fsmOutput, iut.fsmOutput, "TRUE",errorType, mTransitions.mkString(" -> "),iutTransitions.mkString(" -> ")))
    }else{
      distinguishable = true
      errorType = "<span style='color:red'>OPERATION ERROR</span>"
      if(!mTransitions.equals(iutTransitions) ){
        if(errorType != "MISS STATE" || errorType != "MISS TRANSITION"){
          errorType ="<span style='color:red'>TRANSFER ERROR</span>"
          if(iut.states.size > m.states.size )
            errorType ="<span style='color:red'>EXTRA STATE ERROR</span>"
          else if(iut.states.size < m.states.size)
            errorType ="<span style='color:red'>MISS STATE ERROR</span>"
        }else{
          errorType = "<span style='color:red'>"+errorType+"</span>"
        }

      }
      testResults+=((input, m.fsmOutput, iut.fsmOutput, "FALSE",errorType,mTransitions.mkString(" -> "),iutTransitions.mkString(" -> ")))
    }
  }
}