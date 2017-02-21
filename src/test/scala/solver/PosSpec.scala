package solver

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wojciech on 20.02.17.
  */
class PosSpec extends FlatSpec with Matchers {

  val t = Pos(1,1)

  val neighbours: List[Pos] = t.neighbors

  neighbours should contain.theSameElementsAs(List(
    Pos(0,0), Pos(1,0), Pos(2, 0),
    Pos(0,1),           Pos(2, 1),
    Pos(0,2), Pos(1,2), Pos(2, 2)
  ))

  neighbours should contain.noElementsOf(List(
    Pos(1,1)
  ))

}
