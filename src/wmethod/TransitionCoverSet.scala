package wmethod

import utils.Options

import scala.collection.mutable.{ListBuffer, Set, Stack}

/**
  * Created by lucatrubbiani on 20/07/2016.
  */

class TransitionCoverSet extends KEType{
  var tables:ListBuffer[TGroup] = ListBuffer()
  var pSet:Set[String] = Set()
  var stack:Stack[String]= Stack()

  def doTransitionTree(t:ListBuffer[TGroup]) : Set[String] = {
    //var tree = Node(1, List(Node(2,List()), Node(3,List()) ))
    tables = t
    buildCoverSet(tables.head , Set.empty, "")
    if(Options.debug){
      println("-------------------------- P set ----------------------------" )
      println(pSet)
    }
    pSet
  }

  def buildCoverSet(table:TGroup, transitions:Set[(String,(String,Int))], p:String): Unit ={
    var ps = p
    for((g,v) <- table){

      if(transitions.size == 0){
        //get First state
        val first = v.head
        stack.push(first._1)  //add first element
        buildCoverSet(table, first._3, p)
      }else{
        // per tutte le transizioni provenienti dal padre
        for((input,(state,_)) <-  transitions){
          // se lo stato e' gia presente nello stack non devo piu espanderlo
          // semplicemente salvo la string nel set P
          if(stack.contains(state)){
            pSet+= (ps+input)
          }else{
            // la salvo comunque ma devo espandere questo stato
            pSet+= (ps+input)
            //get state from transitions and iterate on them
            for((s, o, n) <- v){
              if(state == s) {
                if(stack.contains(s) == false){
                  stack.push(s)
                  buildCoverSet(table, n, ps+input)
                }
              }
            }
          }
        }
      }
    }
  }
}
