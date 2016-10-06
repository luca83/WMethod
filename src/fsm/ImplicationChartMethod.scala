package fsm
import scala.collection.mutable.{ListBuffer, Map, Set}

trait ImplicationType{
  type Tuple = ListBuffer[Map[String,State]]
  type FsmMap = Map[(String,String), ( Map[String,String], Map[String,String] )]
  type Chart = Map[(String,String),Option[Tuple]]
}
/**
  * Created by lucatrubbiani on 21/07/2016.
  */
class ImplicationChartMethod extends  ImplicationType{
  var chart:Chart = Map()
  var minimizedFsm:FsmMap = Map()
  var set:ListBuffer[(State,State)] = ListBuffer()
  var sTablePair:ListBuffer[(String,String)] = ListBuffer()

  def minimize(fsm:Fsm) : FsmMap = {

    minimizedFsm=Map()
    chart = Map()
    set = ListBuffer()
    sTablePair = ListBuffer()

    var sstate1 = fsm.oStates.tail
    var sstate2 = fsm.oStates.reverse.tail.reverse

    set = buildList(sstate2,sstate1,set)


    var b:Boolean=false;
    for(ls <- set){

      var t1:Map[String,State] = Map()
      var t2:Map[String,State] = Map()
      var s1 = ls._1
      var s2 = ls._2

      //for the set tuple get all transitions
      for(a <- fsm.alphabet ){
        t1+= a-> fsm.states(s1.transitions(a).endState)
        t2+= a-> fsm.states(s2.transitions(a).endState)

      }


      /**
        * Ex: state q1 goes to q1 with symbol 0 and q4 with symbol 1
        *     state q1 goes to q1 with symbol 0 and q5 with symbol 1
        *    ----------
        * q2 | q1 | q1 -> generate the same output for all symbol in the alphabet ?
        *    | q4 | q5 -> generate the same output for all symbol in the alphabet ?
        *    ----------
        *         q1
        * if q2 output == q1 ouput  -> store next state square
        */
      var equal:Boolean = true
      for(a <- fsm.alphabet ) {
        if(s1.transitions(a).output(a) != s2.transitions(a).output(a)){
          equal = false
        }
      }

      var spair:(String,String) = (s1.name,s2.name)
      sTablePair+=(spair)
      if(equal) {
        var tmp = ListBuffer(t1,t2)
        chart += spair -> Some(tmp)
      }
      else
        chart+=spair -> None
    }


    /*println("-------------------------  BEFORE ----------------")
    for(pair <- sTablePair ){
      chart(pair) match {
        case Some(lst) =>println(pair + " ----> "+ lst)
        case None => println(pair + " ----> "+ None)
      }
    }
    println("------------------------- BEFORE ENDCHART----------------")*/

    // Second step check for each square if next transition hold
    var cl:Boolean =false
    do{
      /*println("-------------------------  CLEAR ITERATION ----------------")
      cl = cleanChart(fsm)
      println("------------------------- END CLEAR ITERATION ----------------")*/

    }while(cl == true)

    //Compose the new minimized transition table
    /*for(pair <- sTablePair ){
      println(pair -> chart(pair))
    }*/
    return minimizedFsm.clone()
  }


  def cleanChart(fsm:Fsm) : Boolean = {
    var cleaned:Boolean = false
    for(pair <- sTablePair ){
      var lst = chart(pair) match{
        case Some(lst) => lst
        case None => List()
      }
      if(lst.size > 0){
        for(a <- fsm.alphabet ){
          var f1 = lst.head(a).name
          var f2 = lst.last(a).name
          if(chart.contains((f1,f2))){
            var cp = chart((f1,f2))
            if(cp.equals(None)){
              cleaned = true
              chart(pair) = None
            }
          }
        }
      }
    }
  cleaned
  }

  def buildList(states1:ListBuffer[(String,State)],
                states2:ListBuffer[(String,State)],
                set:ListBuffer[(State,State)] ): ListBuffer[(State,State)] ={
    if(states2.size > 0){
      for((ss,sstate) <- states2 ){
        set+=((states1.head._2,sstate))
      }
      buildList(states1.tail,states2.tail,set)
    }
    set
  }
}
