package wmethod

import fsm.State
import utils.Options

import scala.collection.mutable.{ListBuffer, Map}

/**
  * Returns k-equivalence partitions
 *
  * @param states
  */
class KEquivalence(states:ListBuffer[(String,State)]) extends KEType{


  def doKEquivalence: ListBuffer[TGroup] = {
    var group = Group(states)
    // Build first table
    group.build
    // Make primary groups by output
    group.regroup
    // Reindex next state
    group.reindex
    var keq = false;
    // do loop execution
    do {
      keq = group.regroupWithNext
      if(keq == false) group.reindex
    }
    while( keq == false )

    if(Options.debug) {
      for (t <- group.tableGroups) {
        println("---------------------- Partitions table ----------------------")
        for ((k, v) <- t) {
          println("Group: " + k)
          for (s <- v) {
            s match {
              case (state, output, next) => println(state + " ---> " + output + "  --->" + next)
            }
          }
        }
      }
    }

    return group.tableGroups


  }
}
