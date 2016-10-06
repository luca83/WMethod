package wmethod

import utils.Options

import scala.collection.mutable.{ListBuffer, Set}
/**
  * Created by lucatrubbiani on 20/07/2016.
  */
class TestSet {
  var tSet:ListBuffer[String] = ListBuffer()
  var reducedTSet:Set[String] = Set()

  def doZSet(pSet:Set[String], zSet:Set[String] ):(ListBuffer[String],Set[String]) ={
    for(p <- pSet ){
      for(z <- zSet) yield {
        tSet+=(p+z)
        reducedTSet+=(p+z)
      }
    }
    if(Options.debug){
      println("-------------------------- TEST set ----------------------------" )
      println(tSet)
      println("---------------------- REDUCED TEST set-------------------------" )
      println(reducedTSet)

      println("")
      println("Total number of test : "+tSet.size)
      println("Total number of test reduced : "+reducedTSet.size)
    }
    (tSet,reducedTSet)
  }

}
