package solver

import miner._
import solver.Recursive.{FEmpty, FMine, FProb}

import scala.util.Random

/**
  * Created by wojciech on 20.02.17.
  */
class Solver(miner: Miner) {

  import Solver._

  def start(): Option[List[Pos]] = {
    val hiddenInit: List[Pos] = List.tabulate(miner.width, miner.height)(Pos).flatten
    val start = Lists(hiddenInit, Nil, Nil)

    val iterator = Iterator.iterate[IterateResult](start){
      case e: End => e
      case l@ Lists(hiddenPos, visible, mines) =>
        val visiblePos = visible.map(_.pos)
        val borderPos = visiblePos.flatMap(_.neighbors).distinct
          .filterNot(visiblePos.contains(_))
          .filter(hiddenPos.contains(_))

        borderPos match {
          case Nil =>
            /* Start */
            Random.shuffle(hiddenPos).headOption.map(pos => pos -> miner(pos.x, pos.y)).map {
              case (pos, Mine) => End(pos)
              case (pos,Value(v)) =>
                Lists(hiddenPos.filterNot(_ == pos),  List(Position(pos, Value(v))), mines)
            }.getOrElse(l)

          case border =>
            val rec = Recursive(border, visible)

            val recMines = rec.collect{
              case FMine(pos) => pos
            }

            val newVisible1 = recMines.foldLeft(visible){(v, pos) => v.map{
              case Position(p, Value(x)) if p.neighbors.contains(pos) => Position(p, Value(x - 1))
              case p => p
            }}.filterNot{
              case Position(_, Value(0)) => true
              case _ => false
            }

            val newMines = recMines ::: mines
            def subtractMines(field: Position): Position = Position(field.pos, field.f match {
              case v: Value => newMines.foldLeft(v){
                case (Value(x),minePos) if field.pos.neighbors.contains(minePos) => Value(x - 1)
                case (x, _) => x
              }
              case Mine => Mine
            })

            val newVisible2 = rec.collect{
              case FEmpty(pos) => subtractMines(Position(pos, miner(pos.x, pos.y)))
            }
            (if(newVisible2.isEmpty && rec.nonEmpty) {
              Some(Random.shuffle(rec).collect{ case f: FProb => f }.minBy(_.x))
            } else None).map{ f =>
              subtractMines(Position(f.p, miner(f.p.x, f.p.y)))
            } match {
              case Some(Position(pos, Mine)) =>
                End(pos)
              case Some(p@ Position(pos, Value(_))) =>
                Lists(hiddenPos.filterNot(_ == pos).filterNot(recMines.contains),
                  p :: newVisible1,
                  newMines)
              case None =>
                val nv2 = newVisible2.map(_.pos)
                Lists(hiddenPos.filterNot(nv2.contains).filterNot(recMines.contains),
                  newVisible1 ::: newVisible2,
                  newMines)
            }
        }
    }
    iterator.take(100).dropWhile{
      case End(_) => false
      case Lists(h, _, _) if h.isEmpty => false
      case _ => true
    }.next match {
      case End(_) => None
      case Lists(_, _, mines) => Some(mines)
      case _ => None
    }
  }

}

object Solver {

  case class Position(pos: Pos, f: FieldIn)

  private sealed trait IterateResult
  private case class Lists(hidden: List[Pos], visible: List[Position], mines: List[Pos]) extends IterateResult
  private case class End(mine: Pos) extends IterateResult

}
