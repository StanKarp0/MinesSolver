package miner

/**
  * Created by wojciech on 20.02.17.
  */
sealed trait FieldIn

case class Value(n: Int) extends FieldIn
case object Mine extends FieldIn
