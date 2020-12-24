import scala.io.Source
import scala.util.{Success, Try}

object Day20 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Long = {
    final case class Tile(
      id: Int,
      top: String,    // Left to right
      bottom: String, // Left to right
      left: String,   // Top to bottom
      right: String   // Top to bottom
    )

    object TileExtractor {

      def unapply(s: String): Option[Tile] = {
        Try {
          val tileRows = s.split("~")

          val tileId = tileRows.head match {
            case s"Tile ${tileId}:" => tileId.toInt
          }

          val top    = tileRows.tail.head
          val bottom = tileRows.last
          val left   = tileRows.tail.foldLeft("") { case (accLeft, row) => accLeft + row.head }
          val right  = tileRows.tail.foldLeft("") { case (accLeft, row) => accLeft + row.last }

          Tile(tileId, top, bottom, left, right)
        }.toOption
      }
    }

    val input = Source.fromResource("day20.txt").getLines
    val tiles = Iterator
      .continually(input.takeWhile(_.nonEmpty).mkString("~"))
      .takeWhile(_.nonEmpty)
      .foldLeft(Map.empty[Int, Tile]) {
        case (tiles, TileExtractor(tile)) => tiles.updated(tile.id, tile)
        case (tiles, _)                   => tiles
      }

    tiles.values.foldLeft(1L) { (cornerTileProduct, tile) =>
      val allTileEdges = tiles
        .removed(tile.id)
        .values
        .toSet
        .flatMap { tile: Tile =>
          Set(tile.top, tile.bottom, tile.left, tile.right).flatMap(edge => Set(edge, edge.reverse))
        }

      if ((Set(tile.top, tile.bottom, tile.left, tile.right) diff allTileEdges).size == 2)
        cornerTileProduct * tile.id
      else
        cornerTileProduct
    }
  }

  def solutionTwo: Int = {
    import day20.Puzzle._
    import day20.SeaMonsterHunter.waterRoughness
    import day20.{Tile, TileExtractor}

    val input = Source.fromResource("day20.txt").getLines
    val tiles = Iterator
      .continually(input.takeWhile(_.nonEmpty).mkString("~"))
      .takeWhile(_.nonEmpty)
      .foldLeft(Map.empty[Int, Tile]) {
        case (tiles, TileExtractor(tile)) => tiles.updated(tile.id, tile)
        case (tiles, _)                   => tiles
      }

    val puzzleSize = Math.sqrt(tiles.size).toInt

    val puzzle = assemblePuzzle(tiles, puzzleSize)

    waterRoughness(puzzle)
  }
}
