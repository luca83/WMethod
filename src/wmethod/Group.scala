package wmethod

import fsm.State
import utils._

import scala.collection.mutable.{ListBuffer, Map, Set}

/**
  * Created by lucatrubbiani on 21/07/2016.
  */
case class Group(states:ListBuffer[(String,State)]) extends KEType{
  //contains the history of all elaborations
  var tableGroups:ListBuffer[TGroup] = ListBuffer()

  // Return the first k-equivalence table
  def build : Unit = {
    //table divided by groups
    var groups:TGroup = ListBuffer()
    var table:ListBuffer[TRow] = ListBuffer()

    //Loop through all states
    for((k,s) <-  states){

      // compute all output and next states
      var output:Set[(String,Any)] = Set()
      var next:Set[(String,(String,Int))] = Set()
      for(t <- s.transitions) {
        //evaluate output functions  -> ( input, output )
        output += ((t._1, t._2.output(t._1)))

        //evaluate next states -> (input, (endstate,group))
        next += ((t._1, (t._2.endState,1)))
      }
      table+=((k,output,next))

    }
    // groups -> (group, state, List(input -> output), List(input -> nextState))
    groups+=(1 -> table)
    tableGroups+=groups
  }

  // Find the group membership
  def findGroupMembership(s:Any): Int = {
    var last:TGroup = tableGroups.last
    var g:Int = 0;
    for(e <- last){
      e match{
        case (group,list) => {
          list.foreach {
            l => l match {
              case (state,_,_) => {
                if (state == s){
                  g = group
                }
              }
            }
          }
        }
        case _ => throw new CustomException("Error during (reindexing) table")
      }
    }
    g
  }

  /**
    * state.group == nextState.group ?
    *   yes -> leave it as it is
    *   no -> change the index group in the next state
    */
  def reindex:Unit = {
    var last:TGroup = tableGroups.last
    for(e <- last){
      var table:ListBuffer[TRow] = ListBuffer()
      e match {
        case (group,list) => {
          list.foreach {
            l => l match {
              case (state,output,next) => {
                var set:Set[(String,(String,Int))] = Set()
                for( n <- next){
                  val g:Int = findGroupMembership(n._2._1)
                  if(g == 0)
                    throw CustomException("Group not found during reindexing table")
                  set += ((n._1,(n._2._1,g)))
                }
                table += ((state, output, set))
                //println("------------------- TABLE REINDEXED --------------");
                //println(table);
              }
            }
          }
          var idx = 0;
          for(tg <- tableGroups.last){
            tg match{
              case (itg,ltg) => {
                if(itg == group){
                  tableGroups.last.update(idx,(group,table))
                }else{
                  idx=idx+1
                }
              }
            }
          }

        }
        case _ => throw CustomException("Error during (reindexing) table")
      }
    }
  }

  def regroupWithNext(): Boolean ={
    //println("--------------- REGROUP WITH NEXT --------------")
    var groups:TGroup = ListBuffer()
    var table:ListBuffer[TRow] = ListBuffer()
    var last:TGroup = getCopy

    // map groups
    var groupMap:ListBuffer[(Int, ListBuffer[TRow])] =ListBuffer()
    var useNextGroup:Map[Int,ListBuffer[Set[(String,Int)]]] = Map()
    var newGroupRef:Map[(Int,Int),ListBuffer[Set[(String,Int)]]] = Map()
    for(e <- last) {
      e match {
        case (group, list) => {
          groupMap+=((group,list ))
        }
      }
    }
    //println("-------------------- MAPPED GROUP --------------")
    //println(groupMap)
    // foreach groups find the elmement that has different next group state. For those element
    // create a new group..all the other groups are incremented by 1
    var idxGroup:Int = 1
    for((g,rows) <- groupMap){
      rows.foreach {
        l => var (state, output, next) = l

          var tSet: Set[(String, Int)] = Set()
          for (nxt <- next) {
            tSet += ((nxt._1, nxt._2._2))
          }
          //println("--------------------T SET --------------")
          //println(state+" -- " +tSet+" -------- TNEXT : "+useNextGroup)
          //println(newGroupRef)

          if(useNextGroup.contains(g)){
            var lb = useNextGroup(g)

            if(element(tSet, lb) == false){
              //allora fara parte di un nuovo gruppo
              lb+= (tSet)
              useNextGroup.update(g,lb)
              newGroupRef+=((g,idxGroup)-> ListBuffer(tSet))
              groups+=((idxGroup,ListBuffer[TRow]((state, output, next))))
              idxGroup = idxGroup+1
            }else{
              //println("----- CIO CHE CERCO STA E CONTENUTO DENTRO TSET ---------")
              //devo cercare il gruppo di appartenenza
              for(ngrf <- newGroupRef){
                ngrf match{
                    // ngrfOIdx -> old group id
                    // ngrfNIdx -> new group id
                  case ((ngrfOIdx,ngrfNIdx),ngrfList) => {
                    //println(ngrfOIdx +" --->" +ngrfNIdx + " --- "+ ngrfList)
                    // se il vecchio gruppo corrisponde al mio
                    if(ngrfOIdx == g){
                      // devo cercare nella lista e controllare se trovo un set contenente che corrisponde al mio set next
                      for(nextSet <- ngrfList){
                        if(tSet == nextSet){
                          //println(state+" -- HO TROVATO IL MIO GRUPPO DI APPARTENENZA : " + ngrfNIdx)
                          var ii =0
                          for(og <- groups){

                            og match {
                              case (ig, lg) => {
                                if (ig == ngrfNIdx) {
                                  var nlg = lg += ((state, output, next))
                                 groups.update(ii, (ig, nlg))
                                }
                              }
                            }
                            ii = ii+1
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }else{
            useNextGroup+=(g-> ListBuffer(tSet))
            newGroupRef+=((g,idxGroup)-> ListBuffer(tSet))
            groups+=((idxGroup,ListBuffer[TRow]((state, output, next))))
            idxGroup = idxGroup+1
          }
      }
    }
    //println("--------------------THE USE NEXT GROUP --------------")
    //println(useNextGroup)
    //println("--------------------NEW REF FOR GROUP --------------")
    //println(newGroupRef)
    //println("--------------------NEW GROUPS --------------")
    //println(groups)
    if(tableGroups.last == groups){
      true
    }else{
      tableGroups+=groups
      false
    }
  }

  /**
    * Grupping function that use the output to generate the group
    */
  def regroup: Unit = {
    //println("--------------- REGROUP --------------")
    var groups:TGroup = ListBuffer()
    var table:ListBuffer[TRow] = ListBuffer()
    var last:TGroup = tableGroups.last

    // current group , first is always 1
    var g:Int = 1

    for(e <- last){
      e match {
        case (group,list) => {
          list.foreach{
            l => var (state, output, next)  = l
              //println(state +" --->" +output + " --- "+ next)

              // check if new groups as at least one element
              if(groups.size> 0){
                var found:Boolean = false;
                for((ng, nl) <- groups) {
                  if (!found){
                      if (nl.head._2 == output) {
                        //ouput from the same group found
                        found = true
                        //add entry row to group
                        nl += ((state, output, next))
                      }
                  }
                }
                if(!found){
                  //println("------ Not found! -------")
                  g+=1
                  //add new group with this entries
                  groups+=((g,ListBuffer[TRow]((state, output, next))))
                }
              }else{
                //first occurrence
                groups+=((g,ListBuffer[TRow]((state, output, next))))
              }
          }
        }
        case _ => throw new CustomException("Error during (regroup) function")
      }
    }
    tableGroups+=groups
  }

  def copy: Unit ={
    var clone = tableGroups.last.clone()
    tableGroups+=clone
  }

  // clone the last copy of the table groups
  def getCopy: TGroup = return tableGroups.last.clone()


  // find element in a list and return true or false
  def element[A]( x:A, lst:ListBuffer[A] ): Boolean = lst match {
    case a if a.size == 0  => false
    case a +: _  if( a == x )  => true
    case _ +: as  => element( x, as )
  }

}
