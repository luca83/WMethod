package utils

import java.io.{File, PrintWriter}

import fsm.Fsm
import wmethod.TOracle
import wmethod.KEType

import scala.collection.mutable.{ListBuffer, Set}

/**
  * Created by lucatrubbiani on 23/07/2016.
  */
class Report extends TOracle with KEType{
  var xml="<html><head><title>FMS WMethod Report</title><style>.machines{display:inline-block;margin:20px;}table td{text-align:center;}</style></head><body>"

  def writeResultInFile(iut:Fsm,testResults:TResult): Unit = {

    xml = xml+"<hr/><h1>Result test from IUT ("+iut.name+")</h1>"
    xml = xml+"<table border='1'>"
    xml = xml+"<tr><th>Input</th><th>Outoput M</th><th>Output IUT</th><th>Equal?</th><th>ErrorType</th><th>M Trace path</th><th>IUT Trace path</th></tr>"
    for(t <- testResults){
      xml = xml+"<tr><td>"+t._1+"</td><td>"+t._2+"</td><td>"+t._3+"</td><td>"+t._4+"</td><td>"+t._5+"</td><td>"+t._6+"</td><td>"+t._7+"</td></tr>"
    }
    xml = xml+"</table>"

  }
  def closeAndWrite: Unit = {
    xml= xml+"</body></html>"

    val pw = new PrintWriter(new File("reports/report.html" ))
    pw.write(xml)
    pw.close
  }

  def printMachines(fsms:ListBuffer[Fsm]) : Unit ={
    for(m <- fsms){
      xml+="<div class='machines'><h3>Machine : "+ m.name +"</h3>"
      xml = xml+"<table border='1'>"
      xml = xml+"<tr><td>State</td><td>Transitions</td><td>Output</td></tr>"
      for(s <- m.states){
        val trans = s._2.transitions
        var strans:String = ""
        var out:String =""
        for(t <- trans){
          strans = strans+t._1+" -> "+ t._2.endState+"</br>"
          out = out+t._1+" -> "+ t._2.output(t._1)+"</br>"
        }
        xml = xml+"<tr><td>"+s._1+"</td><td>"+strans+"</td><td>"+out+"</td></tr>"

      }
      xml = xml+"</table></div>"
    }
  }

  def printEquivalenceTable(tableGroups:ListBuffer[TGroup]): Unit ={
    xml+="<h3>Equivalences partitions table generated</h3>"
    var idx:Int = 0
    for (t <- tableGroups) {
      xml = xml+"<div><b>Tabella : "+idx+"</b></div><table border='1'>"
      xml = xml+"<tr><td>Group</td><td>State</td><td>Transitions</td><td>Output</td></tr>"
      for ((k, v) <- t) {
        for (s <- v) {
          s match {
            case (state, output, next) => {
              var outs = ""
              var strans=""
              for((s,o) <- output){
                outs = outs+s+" -> "+ o+"</br>"
              }
              for((s,o) <- next){
                strans = strans+s+" -> "+ o._1+", group:"+o._2+"</br>"
              }
              xml = xml+"<tr><td>Group:"+k+"</td><td>"+state+"</td><td>"+strans+"</td><td>"+outs+"</td></tr>"
            }
          }
        }
      }
      idx = idx+1
      xml = xml+"</table>"
    }
  }
  def printWSet(wSet:Set[String]): Unit ={
    xml+="<h3>Characterization set :</h3>"
    xml+="<h5>{"+wSet.mkString(",")+"}</h5>"
  }
  def printPSet(pSet:Set[String]): Unit ={
    xml+="<h3>Transitions cover set :</h3>"
    xml+="<h5>{"+pSet.mkString(",")+"}</h5>"
  }
  def printZSet(zSet:Set[String]): Unit ={
    xml+="<h3>ZSet :</h3>"
    xml+="<h5>{"+zSet.mkString(",")+"}</h5>"
  }
  def printTSet(tSet:Set[String], num:Int, numReduced:Int): Unit ={
    xml+="<h3>Test set :</h3>"
    xml+="<h5>{"+tSet.mkString(",")+"}</h5>"
    xml+="<br />Total test :"+num
    xml+="<br />Total Reduced test :"+numReduced

  }

}
