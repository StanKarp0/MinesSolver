package solver

import miner.Value
import solver.Solver._

/**
  * Created by wojciech on 20.02.17.
  */
object Recursive {


  sealed abstract class RecField(val pos: Pos)
  case class RMine(p: Pos) extends RecField(p)
  case class REmpty(p: Pos) extends RecField(p)

  sealed abstract class ResField(pos: Pos)
  case class FMine(p: Pos) extends ResField(p)
  case class FEmpty(p: Pos) extends ResField(p)
  case class FProb(p: Pos, x: Double) extends ResField(p)


  def apply(border: List[Pos], visible: List[Position]): List[ResField] = {

    def appendMine(pos: Pos, v: List[Position]): List[Position] = v.map{
      case Position(p, Value(x)) if pos.neighbors.contains(p) => Position(p, Value(x-1))
      case f => f
    }

    def checkMines(v: List[Position]): Boolean = v.forall{
      case Position(_, Value(x)) if x >= 0 => true
      case _ => false
    }

    def checkEmpty(v: List[Position], right: List[Pos]): Boolean = v.forall{
      case Position(pos, Value(x)) =>
        val n = pos.neighbors
        right.count(n.contains) >= x
      case _ => false
    }

    def rec(left: List[RecField], v: List[Position]): List[List[RecField]] = left.size match {
      case size if size == border.size =>
        if(v.collect{
          case Position(_, Value(x)) => x
        }.forall(_ == 0)) List(left) else Nil
      case size =>
        val pos = border(size)
        val right = border.drop(size + 1)

        val leftMine = RMine(pos) :: left
        val newV = appendMine(pos, v)
        val recMine = if(checkMines(newV)) rec(leftMine, newV) else Nil

        val leftEmpty = REmpty(pos) :: left
        val recEmpty = if(checkEmpty(v, right)) rec(leftEmpty, v) else Nil

        recMine ::: recEmpty
    }

    val res = rec(Nil, visible)
    res.flatten
      .groupBy(_.pos)
      .mapValues(list => list.foldLeft((0,0)){
        case ((e, m), RMine(_)) => (e, m + 1)
        case ((e, m), REmpty(_)) => (e + 1, m)
      })
      .map{
        case (pos, (0, 0)) => FEmpty(pos) //TODO when
        case (pos, (0, m)) if m > 0 => FMine(pos)
        case (pos, (e, 0)) if e > 0 => FEmpty(pos)
        case (pos, (e, m)) => FProb(pos, m.toDouble / (e + m))
      }.toList
  }
}
