package day20

import day20.OrientationUtil._
import scala.util.Try

object TileExtractor {

  def unapply(s: String): Option[Tile] = {
    Try {
      val tileRows = s.split("~")

      val tileId = tileRows.head match {
        case s"Tile ${tileId}:" => tileId.toInt
      }

      Tile(tileId, tileRows.drop(1).toList)
    }.toOption
  }
}

final case class Tile(id: Int, tileRows: List[String]) {
  // Left to right
  def top: String = tileRows.head
  // Left to right
  def bottom: String = tileRows.last
  // Top to bottom
  def left: String = tileRows.foldLeft("") { case (accLeft, row) => accLeft + row.head }
  // Top to bottom
  def right: String = tileRows.foldLeft("") { case (accRight, row) => accRight + row.last }

  def rotate(rotations: Int = 1): Tile = Tile(id, tileRows.rotate(rotations))

  def flip: Tile = Tile(id, tileRows.flip)

  def orientations: List[Tile] =
    List(
      this,
      this.rotate(),
      this.rotate(2),
      this.rotate(3),
      this.flip,
      this.flip.rotate(),
      this.flip.rotate(2),
      this.flip.rotate(3)
    )

  def trim: Tile =
    Tile(id, tileRows.drop(1).dropRight(1).map(row => row.slice(1, row.length - 1)))
}
