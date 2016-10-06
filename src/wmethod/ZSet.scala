package wmethod

/**
  * Created by lucatrubbiani on 20/07/2016.
  */
import scala.collection.mutable.Set
import utils._

class ZSet {

  var zSet:Set[String] = Set()

  def doZSet(numStatesM:Int, numStatesN:Int, wSet:Set[String], x:Set[String] ):Set[String] ={
    // X is the input alphabet
    var X:String = ""
    x.foreach( s => X=X+s )

    if(numStatesM >= numStatesN){
      val p = numStatesM - numStatesN
      if(p == 0)
        zSet = wSet
      else if(p==1){
        zSet = calculateZ(wSet,X)
      }else{
        var a = 0
        zSet = calculateZ(wSet,X)
        for( a <- 1 until p){
          X=X+X
          zSet = zSet ++ calculateZ(wSet,X)
        }
      }
    }
    if(numStatesM < numStatesN){
      zSet = calculateZ(wSet,X)
    }
    if(Options.debug){
      println("--------------------------- Z  set -----------------------------" )
      println(zSet)
    }
    zSet
  }

  def calculateZ(wSet:Set[String], x:String):Set[String] ={
    var z:Set[String] = Set()
    for(c <- x ){
     for(w <- wSet) {
         z+=(c+w)
       }
    }
    z
  }

}
