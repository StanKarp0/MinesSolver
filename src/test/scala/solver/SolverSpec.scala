package solver

import miner.{FieldIn, Mine, Miner, Value}
import org.scalatest.{FlatSpec, Matchers}
import tools.ConvTool
import tools.ConvTool.M

/**
  * Created by wojciech on 20.02.17.
  */

class MinerImpl(board: List[List[FieldIn]], val width: Int, val height: Int) extends Miner {
  def apply(x: Int, y: Int): FieldIn = board(y)(x)
}

class SolverSpec extends FlatSpec with Matchers{

  ConvTool(List(
    1, 2, 2, 2, M, M, 1, 0,
    1, M, M, 2, 3, 3, 2, 0,
    1, 3, 3, 2, 1, M, 1, 0,
    0, 1, M, 2, 2, 2, 1, 0,
    1, 2, 3, 3, M, 1, 0, 0,
    1, M, 2, M, 2, 1, 0, 0,
    1, 1, 3, 2, 2, 0, 0, 0,
    0, 0, 1, M, 1, 0, 0, 0
  ),8,8).foreach { tab =>
    val miner = new MinerImpl(tab,8,8)
    val solver = new Solver(miner)
    val mines = List(
      Pos(1, 1), Pos(1, 5), Pos(2, 3), Pos(2, 1), Pos(4, 0), Pos(3, 5), Pos(3, 7), Pos(4, 4), Pos(5, 0), Pos(5, 2)
    )
    List.range(0, 30).foreach{ _ => solver.start() match {
      case Some(l) => l should contain allElementsOf mines
      case None =>
    }}
  }


  ConvTool(List(
    0, 1, 1, 1, 0, 0, 0, 0,
    0, 1, M, 2, 1, 0, 1, 1,
    0, 1, 2, M, 1, 1, 2, M,
    0, 0, 1, 1, 1, 1, M, 2,
    0, 1, 1, 2, 1, 3, 2, 2,
    1, 3, M, 3, M, 2, M, 1,
    M, 3, M, 3, 2, 3, 2, 1,
    1, 2, 1, 1, 1, M, 1, 0
  ),8,8).foreach { tab =>
    val miner = new MinerImpl(tab,8,8)
    val solver = new Solver(miner)
    val mines = List(
      Pos(0, 6), Pos(2, 1), Pos(2, 5), Pos(2, 6), Pos(3, 2), Pos(4, 5), Pos(5, 7), Pos(6, 3), Pos(6, 5), Pos(7, 2)
    )
    List.range(0, 30).foreach{ _ => solver.start() match {
      case Some(l) => l should contain allElementsOf mines
      case None =>
    }}
  }


  ConvTool(List(
    M, M, M, 2, 0, 0, 0, 0,
    3, M, M, 2, 0, 0, 0, 0,
    1, 2, 2, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 1, M, 1, 0,
    0, 1, 2, 2, 2, 1, 2, 1,
    0, 2, M, M, 1, 0, 1, M,
    0, 2, M, 3, 1, 0, 1, 1,
    0, 1, 1, 1, 0, 0, 0, 0
  ),8,8).foreach { tab =>
    val miner = new MinerImpl(tab,8,8)
    val solver = new Solver(miner)
    val mines = List(
      Pos(0, 0), Pos(1, 0), Pos(1, 1), Pos(2, 0), Pos(2, 1), Pos(2, 5), Pos(2, 6), Pos(3, 5), Pos(5, 3), Pos(7, 5)
    )
    List.range(0, 30).foreach{ _ => solver.start() match {
      case Some(l) => l should contain allElementsOf mines
      case None =>
    }}
  }


  ConvTool(List(
    1, M, 1, 1, M, 1, 0, 0,
    1, 1, 1, 1, 1, 2, 1, 1,
    0, 0, 0, 0, 0, 1, M, 1,
    0, 0, 0, 0, 1, 3, 3, 2,
    1, 1, 0, 1, 2, M, M, 1,
    M, 2, 1, 1, M, 3, 2, 1,
    3, M, 2, 1, 1, 1, 1, 1,
    2, M, 2, 0, 0, 0, 1, M
  ),8,8).foreach { tab =>
    val miner = new MinerImpl(tab,8,8)
    val solver = new Solver(miner)
    val mines = List(
      Pos(0, 5), Pos(1, 0), Pos(1, 6), Pos(1, 7), Pos(4, 0), Pos(4, 5), Pos(5, 4), Pos(6, 2), Pos(6, 4), Pos(7, 7)
    )
    List.range(0, 30).foreach{ _ => solver.start() match {
      case Some(l) => l should contain allElementsOf mines
      case None =>
    }}
  }

}
