package day20

object OrientationUtil {

  implicit class OrientationOps(rows: List[String]) {

    private def rotate: List[String] = rows.foldLeft(List.empty[String]) {
      case (newTileRows, row) if newTileRows.isEmpty => row.split("").toList
      case (newTileRows, row) =>
        row.zipWithIndex.map { case (pos, index) => pos +: newTileRows(index) }.toList
    }

    def rotate(rotations: Int = 1): List[String] =
      Option.when(rotations == 0)(rows).getOrElse(rows.rotate.rotate(rotations - 1))

    def flip: List[String] = rows.map(_.reverse)
  }
}
