package day20

//import day20.OrientationUtil._
import day20.Puzzle.{Coors, Puzzle}
import day20.Puzzle.PuzzleOps

import scala.util.{Success, Try}

object SeaMonsterHunter {

  def waterRoughness(puzzle: Puzzle): Int = {
    val puzzleOrientations = puzzle.orientations

    val res = for {
      y <- puzzle.indices
      x <- puzzle.indices
    } yield puzzleOrientations.map(isSeaMonster(_, Coors(y, x)))

    puzzle.map(_.count(_ == '#')).sum - (res.flatten.count(_ == true) * 15)
  }

  private def isSeaMonster(puzzle: List[String], coors: Coors): Boolean = {
    val seaMonsterCoors = List(
      coors,
      Coors(coors.y + 1, coors.x + 1),
      Coors(coors.y + 1, coors.x + 4),
      Coors(coors.y, coors.x + 5),
      Coors(coors.y, coors.x + 6),
      Coors(coors.y + 1, coors.x + 7),
      Coors(coors.y + 1, coors.x + 10),
      Coors(coors.y, coors.x + 11),
      Coors(coors.y, coors.x + 12),
      Coors(coors.y + 1, coors.x + 13),
      Coors(coors.y + 1, coors.x + 16),
      Coors(coors.y, coors.x + 17),
      Coors(coors.y, coors.x + 18),
      Coors(coors.y, coors.x + 19),
      Coors(coors.y - 1, coors.x + 18)
    )

    Try(seaMonsterCoors.forall(coors => puzzle(coors.y)(coors.x) == '#')) match {
      case Success(true) => true
      case _             => false
    }
  }
}
