package solver

/**
  * Created by wojciech on 20.02.17.
  */
case class Pos(x: Int, y: Int) {
  lazy val neighbors: List[Pos] =
    List.tabulate(3,3)((nx,ny) => Pos(x-1+nx, y-1+ny)).flatten
      .filterNot(p => p.x == x && p.y == y)
}
