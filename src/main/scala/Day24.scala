import scala.io.Source

object Day24 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  // The hexagonal floor is represented using the axial coordinate system
  // https://www.redblobgames.com/grids/hexagons/#coordinates
  final case class Coors(y: Int = 0, x: Int = 0) {
    def east: Coors      = Coors(this.y, this.x + 1)
    def southeast: Coors = Coors(this.y + 1, this.x)
    def southwest: Coors = Coors(this.y + 1, this.x - 1)
    def west: Coors      = Coors(this.y, this.x - 1)
    def northwest: Coors = Coors(this.y - 1, this.x)
    def northeast: Coors = Coors(this.y - 1, this.x + 1)

    def neighbors: List[Coors] =
      List(this.east, this.southeast, this.southwest, this.west, this.northeast, this.northwest)
  }

  object CoorsExtractor {

    def unapply(s: String): Option[Coors] = {
      val coors = s
        .foldLeft(Coors(), "") {
          case ((coors, dir), letter) =>
            dir + letter match {
              case "e"  => (coors.east, "")
              case "se" => (coors.southeast, "")
              case "sw" => (coors.southwest, "")
              case "w"  => (coors.west, "")
              case "nw" => (coors.northwest, "")
              case "ne" => (coors.northeast, "")
              case _    => (coors, dir + letter)
            }
        }
        ._1

      Some(coors)
    }
  }

  def solutionOne: Int = {
    Source
      .fromResource("day24.txt")
      .getLines
      .foldLeft(Set.empty[Coors]) {
        case (blackHexes, CoorsExtractor(coors)) =>
          Option
            .when(!blackHexes.contains(coors))(blackHexes + coors)
            .getOrElse(blackHexes - coors)
        case (blackHexes, _) => blackHexes
      }
      .size
  }

  def solutionTwo: Int = {

    type Color = Boolean

    def newDay(floor: Map[Coors, Color]): Map[Coors, Color] = {
      val newFloor = floor.keys
        .foldLeft(Set.empty[Coors]) {
          case (newWhiteHexes, coors) => newWhiteHexes ++ coors.neighbors
        }
        .diff(floor.keys.toSet)
        .foldLeft(floor) { case (floor, coors) => floor.updated(coors, false) }

      val hexesToFlip = newFloor.foldLeft(Set.empty[Coors]) {
        case (hexesToFlip, (coors, color)) =>
          val numBlack = coors.neighbors.flatMap(newFloor.get).count(_ == true)

          if ((color && (numBlack == 0 || numBlack > 2)) || (!color && numBlack == 2))
            hexesToFlip + coors
          else
            hexesToFlip
        case (hexesToFlip, _) => hexesToFlip
      }

      hexesToFlip.foldLeft(newFloor) { case (floor, coors) => floor.updated(coors, !floor(coors)) }
    }

    val day1Floor = Source
      .fromResource("day24.txt")
      .getLines
      .foldLeft(Map.empty[Coors, Color]) {
        case (floor, CoorsExtractor(coors)) =>
          Option
            .when(!floor.contains(coors))(floor.updated(coors, true))
            .getOrElse(floor.updated(coors, !floor(coors)))
        case (floor, _) => floor
      }

    (1 to 100)
      .foldLeft(day1Floor) { case (floor, _) => newDay(floor) }
      .values
      .count(_ == true)
  }
}
