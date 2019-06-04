package example

import scala.util.Random

object App {
  def main(args: Array[String]): Unit = World.run(100, Grid(10,20))
}

case class Cell(isAlive: Boolean) {
  def next(neighbourhood: List[Cell]): Cell =
    neighbourhood.count(_.isAlive) match {
      case x if x > 3 => Cell(false)
      case x if x < 2 => Cell (false)
      case _ => this
    }
}
object Cell {
  def apply(): Cell = Cell(Random.nextBoolean())
}

case class Grid(x: Int, y:Int, a: Array[Array[Cell]]) {
  val width: Int = this.x
  val height: Int = this.y

  def next: Grid = {
    val nextGrid: Array[Array[Cell]] =
      a.zipWithIndex.map {
        case(row, ix) => row.zipWithIndex.map {
          case (cell, iy) => cell.next(this.getNeighbours(ix,iy))
        }
      }

    this.copy(a=nextGrid)
  }

  private def cell(x: Int, y: Int): Cell = {
    def m(n: Int, d: Int): Int = ((n % d) + d) % d
    a(m(x, width))(m(y,height))
  }

  private def getNeighbours(x: Int, y:Int): List[Cell] = List(
      cell(x-1,y+1),cell(x,y-1),cell(x+1,y+1),
      cell(x-1,y),cell(x+1,y),
      cell(x-1,y-1),cell(x,y-1),cell(x+1,y-1)
    )

}
object Grid {
  def apply(x: Int, y: Int): Grid = Grid(x, y, Array.fill(x,y) { Cell() })
}

object World {
  def render(grid: Grid): Unit = grid.a.foreach(row =>
    println(row.foldLeft("")((m, cell) =>
      m + (if (cell.isAlive) "1" else "0"))))

  def run(t: Int, grid: Grid): Unit = if(t > 0) {
    render(grid)
    run(t - 1, grid.next)
  }
}
