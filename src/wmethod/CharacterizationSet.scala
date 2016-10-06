package wmethod

import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map, Set}
import scala.util.control.Breaks._
import utils.Options
/**
  * Created by lucatrubbiani on 19/07/2016.
  */

class CharacterizationSet extends KEType{

  var tables:ListBuffer[TGroup] = ListBuffer()
  var wSet:Set[String] = Set()
  var distinguishingSequence:Map[(String,String),(ListBuffer[String],Map[String,(String,Any)])] = Map()
  var cp:ListBuffer[Set[String]] = ListBuffer()


  def doCharacterizationSet(t:ListBuffer[TGroup]) : Set[String] = {
    tables = t
    // first of all build cartesian product of states
    cartesianProduct
    // next for each couple try to find the last table in which they were together
    var tablesFound:Map[Set[String],Option[(TGroup,Int)]] = Map()
    for(cpset <- cp){
      tablesFound = findSameGroupTable(cpset, tables.tail.reverse,tablesFound)
    }
    // for each results I will have Some(Table) or None.

    // if None get the first table and find an input simbol x <- X such that O(qi,x) not equal O(qj,x)
    // add this simbol into the caracterization set
    // else .... exist costruct
    for(t <-tablesFound){
      t match{
        case (tuple, Some((t,drop))) => KDistinguishable(tuple, drop)
        case (tuple, None ) => K1Distinguishable(tuple)
      }
    }
    if(distinguishingSequence.size == 0){
      println("-----------------------------------------------------------" )
      println("Sates are k-equivalent -> SKIP PROCEDURE")
    }else{
      if(Options.debug){
        println("------------------- Distinguishing Sequence -----------------" )
        distinguishingSequence.foreach(s => println(s))
        println("-------------------------- W set ----------------------------" )
        println(wSet)
      }
    }
    wSet

  }

  def cartesianProduct: Unit ={
    val table = tables.head

    var pc:ArrayBuffer[String] = ArrayBuffer()
    for((group,rows) <- table){
      for(s <- rows){
        pc+=(s._1)
      }
    }
    val pcc = pc.clone()

    pc.foreach( (s:String) => {
      pcc.foreach( (ss:String) => {
        if(cp.contains(Set(s,ss)) == false && s != ss) cp +=(Set(s,ss))
      })
    })
    //println("------------------- Cartesian product-------------" )
    //println(cp)
  }

  def findSameGroupTable(cpset:Set[String],tables:ListBuffer[TGroup],
                         tFound:Map[Set[String],Option[(TGroup,Int)]]): Map[Set[String],Option[(TGroup,Int)]] = {
    var tablesFound:Map[Set[String],Option[(TGroup,Int)]] = tFound
    var idDrop:Int = 1;
    breakable{
      for(table <- tables){
        val found = checkTableRows(cpset,table)
        if(found){
          tablesFound(cpset) = Some((table,idDrop))
          break
        }else{
          tablesFound(cpset) = None
        }
        idDrop=idDrop+1
      }
    }
    return tablesFound
  }

  def checkTableRows(cpset:Set[String], table:TGroup) : Boolean = {
    //println("---------------------------- START CHECK Row")
    var allIn:Boolean = false
    breakable {
      for ((g, rows) <- table) {
        if(allIn == true) break
        //println(rows)
        for (row <- rows) {
          //check wheather first element of cpset exist
          if (row._1 == cpset.head) {
            //println("------------- found FIRST IN row "+cpset)
            //println(rows)
            // if exist try to find from this group if all other element in set are presents
            allIn = checkSiblings(cpset.tail, rows)
            //if all element in a table group -> Ok table found
            if(allIn == true) break
          }
        }
      }
    }
    //println("ALL IN TABLE ROWS" + allIn)
    return allIn
  }

  def checkSiblings(cpset:Set[String], rows:ListBuffer[TRow]):Boolean ={
    //println("---------------------------- START CHECK SIBLINGS")
    var allIn:Boolean = true
    var checked:Map[String,Boolean] = Map()
    for(ee <- cpset) checked += ee->false
    if(cpset.size > 0){
      breakable{
        for(e <- cpset){
          for(row <- rows){
            //check wheather first element of cpset exist
            if (row._1 != e){
              //println("One element is not on the list " +e  )
            }else{
              //println("Element is on list " + e + " /// " + row  )
              checked(e) = true
            }
          }
        }
        for((e,s) <-checked) {
          //println(e+" -------  "+s)
          if(s == false){
            allIn = false
            break
          }
        }
      }
    }
    //println(checked)
    //println("ALL IN "+ allIn)
    return allIn
  }

  def K1Distinguishable(states:Set[String]) :Unit ={
   // println("new calll k1 dist")
    var table:TGroup = tables.head

    //Collect all couple state -> ouput values
    var next:Map[String,Set[(String,Any)]] = Map()

    //first of all find the n rows
    for((g,rows) <- table){
      for(row <- rows){
        for(s <- states){
          if(row._1 == s){
            next+= s-> row._2
          }
        }
      }
    }
    // Store informations on the wSet && into distinguishible table
    var distFound:Boolean = false;
    for((s1, o1) <- next){
      if(!distFound)
      for((s2, o2) <- next){
        if(!distFound)
          o2.foreach( s => {
            if(o1.contains(s) == false && s1 != s2){
              distFound = true
              var o = for(d <- o1; if d._1 == s._1) yield { (d._1,d._2) }
              //println(s1 + " & "+ s2 + " ====> " +s +", "+o)
              distinguishingSequence+=(s1,s2) -> (ListBuffer(s._1), Map(s1 ->(s._1, s._2), s2->(o.head._1 , o.head._2)))
              wSet+=((s._1))
            }
          })
      }
    }
  }

  def KDistinguishable(states:Set[String], d:Int) : Unit ={
    //rename s1 -> p1 , s2 -> p2

    val po1 = states.head
    val po2 = states.last // before was tail.head
    var p1:String = states.head
    var p2:String = states.last // before was tail.head
    var distFound:Boolean = false;
    var rStates:Set[String] = states
    var originalOutput:Map[String,(String,Any)] = Map()
    var first:Boolean = true
    var z:String = ""
    //Collect all couple state -> ouput values
    var group:Map[String,Set[(String,(String,Int))]] = Map()
    //Collect all couple state -> ouput values
    var next:Map[String,Set[(String,Any)]] = Map()


    var tbls = tables.tail.reverse.drop(d-1)
    for(table <- tbls){

      //first of all find the n rows
      for((g,rows) <- table){
        for(row <- rows){
          for(s <- rStates){
            if(row._1 == s){
              group+= s-> row._3

             // println("Groups found")
            }
          }
        }
      }

      //find an input x that bring the two state in different group, if more than one choose one
      var group1 = group(p1)
      var group2 = group(p2)

      for((o1, g1) <- group1){
        if(!distFound)
          for((o2, g2) <- group2){
            if(!distFound){
              if(o1 == o2 && g1._2 != g2._2){
                //println(p1 + " - "+ o1+ " ----> g:"+g1._2)
                //println(p2 + " - "+ o2+ " ----> g:"+g2._2)
                distFound = true

                // This will be part fo the test string
                z=z+o1
                // rename state p1 and p2 emulating transtion
                p1 = g1._1
                p2 = g2._1
              }
            }
          }
      }

      //println("Z : "+z)
      distFound = false
      group.clear()
      rStates.clear()
      rStates+=(p1,p2)
    }

    //Now focus on the original state transition to find last occurrence
    //first of all find the n rows
    next.clear()
    for((g,rows) <- tables.head){
      for(row <- rows){
        for(s <- rStates){
          if(row._1 == s){
            next+= s-> row._2
          }
        }
      }
    }
    // Store informations on the wSet && into distinguishible table
    for((s1, o1) <- next){
      if(!distFound)
        for((s2, o2) <- next){
          if(!distFound)
            o2.foreach( s => {
              if(o1.contains(s) == false && s1 != s2){
                distFound = true
                var o = for(d <- o1; if d._1 == s._1) yield { (d._1,d._2) }

                distinguishingSequence+=(po1, po2) -> (ListBuffer(z+s._1), Map(s1 ->(s._1, s._2), s2->(o.head._1 , o.head._2)))
                wSet+=((z+s._1))
              }
            })
        }
    }
  }
}
