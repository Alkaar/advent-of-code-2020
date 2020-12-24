package day20

import OrientationUtil._

object Puzzle {

  type Puzzle = List[String]

  private type PuzzlePieces = Map[Coors, Tile]

  def assemblePuzzle(tiles: Map[Int, Tile], puzzleSize: Int): List[String] = {
    val puzzle =
      solvePuzzle(tiles, puzzleSize).getOrElse(throw new RuntimeException("Can't solve the puzzle"))

    (0 until puzzleSize)
      .map { y =>
        (1 until puzzleSize).foldLeft(puzzle(Coors(y, 0)).trim.tileRows) {
          case (combineTiles, x) =>
            puzzle(Coors(y, x)).trim.tileRows.zipWithIndex.map {
              case (row, index) => combineTiles(index) + row
            }
        }
      }
      .foldLeft(List.empty[String]) { case (combineRows, rows) => combineRows ++ rows }
  }

  private def solvePuzzle(
    tiles: Map[Int, Tile],
    puzzleSize: Int,
    puzzlePieces: PuzzlePieces = Map.empty[Coors, Tile],
    coors: Coors = Coors(0, 0)
  ): Option[PuzzlePieces] = {
    if (tiles.isEmpty && puzzlePieces.isMatch(coors)) {
      Some(puzzlePieces)
    } else if (puzzlePieces.isEmpty) {
      findCornerTile(tiles).orientations
        .collectFirst { tile =>
          solvePuzzle(
            tiles.removed(tile.id),
            puzzleSize,
            puzzlePieces.updated(coors, tile),
            coors
          ) match { case Some(res) => res }
        }
    } else if (puzzlePieces.isMatch(coors)) {
      val newCoors = Option
        .when(coors.x < puzzleSize - 1)(Coors(coors.y, coors.x + 1))
        .getOrElse(Coors(coors.y + 1, 0))

      tiles.values
        .to(LazyList)
        .flatMap(_.orientations)
        .collectFirst { tile =>
          solvePuzzle(
            tiles.removed(tile.id),
            puzzleSize,
            puzzlePieces.updated(newCoors, tile),
            newCoors
          ) match { case Some(res) => res }
        }
    } else {
      None
    }
  }

  private def findCornerTile(tiles: Map[Int, Tile]): Tile = {
    def tileToAllOtherEdges(id: Int): Set[String] = {
      tiles
        .removed(id)
        .values
        .toSet
        .flatMap { tile: Tile =>
          List(tile.top, tile.bottom, tile.left, tile.right)
            .flatMap(edge => List(edge, edge.reverse))
        }
    }

    tiles.values
      .collectFirst { tile =>
        (Set(tile.top, tile.bottom, tile.left, tile.right) diff tileToAllOtherEdges(tile.id)).size match {
          case 2 => tile
        }
      }
      .getOrElse(throw new NoSuchElementException("No corner tile found"))
  }

  implicit class PuzzleOps(puzzle: Puzzle) {

    def orientations: List[List[String]] = {
      List(
        puzzle,
        puzzle.rotate(),
        puzzle.rotate(2),
        puzzle.rotate(3),
        puzzle.flip,
        puzzle.flip.rotate(),
        puzzle.flip.rotate(2),
        puzzle.flip.rotate(3)
      )
    }
  }

  implicit private class PuzzlePiecesOps(puzzle: PuzzlePieces) {

    def isMatch(coors: Coors): Boolean = {
      val top    = puzzle.get(Coors(coors.y - 1, coors.x))
      val bottom = puzzle.get(Coors(coors.y + 1, coors.x))
      val left   = puzzle.get(Coors(coors.y, coors.x - 1))
      val right  = puzzle.get(Coors(coors.y, coors.x + 1))

      val isTopMatch = top match {
        case Some(topTile) => topTile.bottom == puzzle(coors).top
        case None          => true
      }
      val isBottomMatch = bottom match {
        case Some(bottomTile) => bottomTile.top == puzzle(coors).bottom
        case None             => true
      }
      val isLeftMatch = left match {
        case Some(leftTile) => leftTile.right == puzzle(coors).left
        case None           => true
      }
      val isRightMatch = right match {
        case Some(rightTile) => rightTile.left == puzzle(coors).right
        case None            => true
      }

      isTopMatch && isBottomMatch && isLeftMatch && isRightMatch
    }
  }

  final case class Coors(y: Int, x: Int)
}
