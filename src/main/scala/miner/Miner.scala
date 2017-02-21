package miner

/**
  * Created by wojciech on 20.02.17.
  */
trait Miner {

  def width: Int
  def height: Int

  def apply(x: Int, y: Int): FieldIn

}
