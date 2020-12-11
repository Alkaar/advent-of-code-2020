import scala.io.Source

object Day11 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input = Source.fromResource("day11.txt").getLines.toSeq

    def initSeatLayout(): Seq[Seq[Char]] = input.map(_.toList)

    def playRound(seatLayout: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      for {
        y <- input.indices
      } yield input(y).indices.map(x => updateState(seatLayout, y, x))
    }

    def updateState(seatLayout: Seq[Seq[Char]], y: Int, x: Int): Char = {
      val curElem = seatLayout(y)(x)
      if (curElem != '.') {
        val states = for {
          curY <- Math.max(0, y - 1) to Math.min(seatLayout.length - 1, y + 1)
          curX <- Math.max(0, x - 1) to Math.min(seatLayout.head.length - 1, x + 1)
        } yield seatLayout(curY)(curX)

        (states diff Seq(curElem)).count(_ == '#') match {
          case cnt if cnt == 0 => '#'
          case cnt if cnt >= 4 => 'L'
          case _               => curElem
        }
      } else curElem
    }

    LazyList
      .unfold(Seq.empty[Seq[Char]], initSeatLayout()) {
        case (oldBoard, curBoard) if oldBoard != curBoard => {
//          curBoard.foreach(elem => println(elem.foldLeft("")(_ + _)))
//          println()
          val newBoard = playRound(curBoard)
          Some(newBoard, (curBoard, newBoard))
        }
        case _ => None
      }
      .last
      .flatten
      .count(_ == '#')
  }

  def solutionTwo: Int = {
    val input = Source.fromResource("day11.txt").getLines.toSeq

    def initSeatLayout(): Seq[Seq[Char]] = input.map(_.toList)

    def playRound(seatLayout: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      for {
        y <- input.indices
      } yield input(y).indices.map(x => updateState(seatLayout, y, x))
    }

    def updateState(seatLayout: Seq[Seq[Char]], y: Int, x: Int): Char = {
      def findVisibleSeat(seats: Seq[(Int, Int)]): Option[Char] = {
        seats
          .drop(1)
          .map { case (y, x) => seatLayout(y)(x) }
          .dropWhile(_ == '.')
          .headOption
      }

      val curElem = seatLayout(y)(x)
      if (curElem != '.') {
        val up    = y to 0 by -1
        val down  = y until seatLayout.length
        val right = x until seatLayout.head.length
        val left  = x to 0 by -1
        val n     = up.zipAll(Seq(x), x, x)
        val s     = down.zipAll(Seq(x), x, x)
        val e     = right.zipAll(Seq(y), y, y).map(_.swap)
        val w     = left.zipAll(Seq(y), y, y).map(_.swap)
        val ne    = up.zip(right)
        val nw    = up.zip(left)
        val se    = down.zip(right)
        val sw    = down.zip(left)

        Seq(n, s, e, w, ne, nw, se, sw)
          .flatMap(findVisibleSeat)
          .count(_ == '#') match {
          case cnt if cnt == 0 => '#'
          case cnt if cnt >= 5 => 'L'
          case _               => curElem
        }
      } else curElem
    }

    LazyList
      .unfold(Seq.empty[Seq[Char]], initSeatLayout()) {
        case (oldBoard, curBoard) if oldBoard != curBoard => {
//          curBoard.foreach(elem => println(elem.foldLeft("")(_ + _)))
//          println()
          val newBoard = playRound(curBoard)
          Some(newBoard, (curBoard, newBoard))
        }
        case _ => None
      }
      .last
      .flatten
      .count(_ == '#')
  }
}
