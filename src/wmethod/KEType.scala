package wmethod

import scala.collection.mutable.{ListBuffer, Set}

/**
  * Created by lucatrubbiani on 21/07/2016.
  */
trait KEType{
  type TRow = (String, Set[(String,Any)],Set[(String,(String,Int))])
  type TGroup = ListBuffer[(Int,ListBuffer[TRow])]
}
