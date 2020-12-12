import Day12.Direction.{East, North, South, West}
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.io.Source
import scala.util.Try

object Day12 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  sealed abstract class Direction(val value: Int) extends IntEnumEntry

  object Direction extends IntEnum[Direction] {
    val values = findValues

    case object North extends Direction(0)
    case object East  extends Direction(90)
    case object South extends Direction(180)
    case object West  extends Direction(270)
  }

  final case class Move(dir: Char, value: Int)

  object extractMove {
    def unapply(s: String): Option[Move] = Try(Move(s.charAt(0), s.drop(1).toInt)).toOption
  }

  def solutionOne: Int = {
    final case class Ship(y: Int = 0, x: Int = 0, dir: Direction = East)

    def moveShip(ship: Ship, dir: Direction, value: Int): Ship = {
      dir match {
        case North => ship.copy(y = ship.y + value)
        case South => ship.copy(y = ship.y - value)
        case East  => ship.copy(x = ship.x + value)
        case West  => ship.copy(x = ship.x - value)
      }
    }

    val position = Source.fromResource("day12.txt").getLines.foldLeft(Ship()) { (ship, move) =>
      extractMove.unapply(move) match {
        case Some(Move('N', value)) => moveShip(ship, North, value)
        case Some(Move('S', value)) => moveShip(ship, South, value)
        case Some(Move('E', value)) => moveShip(ship, East, value)
        case Some(Move('W', value)) => moveShip(ship, West, value)
        case Some(Move('L', value)) => {
          val adjust = if (ship.dir.value - value < 0) 360 else 0
          ship.copy(dir = Direction.withValue((ship.dir.value - value + adjust) % 360))
        }
        case Some(Move('R', value)) =>
          ship.copy(dir = Direction.withValue((ship.dir.value + value) % 360))
        case Some(Move('F', value)) => moveShip(ship, ship.dir, value)
        case _                      => ship
      }
    }

    Math.abs(position.y) + Math.abs(position.x)
  }

  def solutionTwo: Int = {
    final case class Waypoint(y: Int = 1, x: Int = 10)
    final case class Ship(y: Int = 0, x: Int = 0, dir: Direction = East, wp: Waypoint = Waypoint())

    def moveWaypoint(ship: Ship, dir: Direction, value: Int): Ship = {
      val waypoint = dir match {
        case North => ship.wp.copy(y = ship.wp.y + value)
        case South => ship.wp.copy(y = ship.wp.y - value)
        case East  => ship.wp.copy(x = ship.wp.x + value)
        case West  => ship.wp.copy(x = ship.wp.x - value)
      }
      ship.copy(wp = waypoint)
    }

    val position = Source.fromResource("day12.txt").getLines.foldLeft(Ship()) { (ship, move) =>
      extractMove.unapply(move) match {
        case Some(Move('N', value)) => moveWaypoint(ship, North, value)
        case Some(Move('S', value)) => moveWaypoint(ship, South, value)
        case Some(Move('E', value)) => moveWaypoint(ship, East, value)
        case Some(Move('W', value)) => moveWaypoint(ship, West, value)
        case Some(Move('L', value)) =>
          ship.copy(wp = (1 to value / 90).foldLeft(ship.wp) { (wp, _) =>
            Waypoint(wp.x, wp.y * -1)
          })
        case Some(Move('R', value)) =>
          ship.copy(wp = (1 to value / 90).foldLeft(ship.wp) { (wp, _) =>
            Waypoint(wp.x * -1, wp.y)
          })
        case Some(Move('F', value)) =>
          ship.copy(y = ship.wp.y * value + ship.y, x = ship.wp.x * value + ship.x)
        case _ => ship
      }
    }

    Math.abs(position.y) + Math.abs(position.x)
  }
}
