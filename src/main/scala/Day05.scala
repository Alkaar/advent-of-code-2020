import scala.io.Source

object Day05 {

  case class Range(lower: Int, upper: Int)

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    def findRowOrCol(seatCode: String, isRowSearch: Boolean): Int = {
      val (lowerChar, maxRange) = if (isRowSearch) ('F', 128) else ('L', 8)
      seatCode
        .foldLeft(Range(0, maxRange)) { (r, letter) =>
          val delta = (r.upper - r.lower) / 2
          if (letter == lowerChar) Range(r.lower, r.upper - delta)
          else Range(r.lower + delta, r.upper)
        }
        .lower
    }

    Source.fromResource("day05.txt").getLines.toSeq.foldLeft(0) { (max, line) =>
      val row = findRowOrCol(seatCode = line.dropRight(3), isRowSearch = true)
      val col = findRowOrCol(seatCode = line.drop(7), isRowSearch      = false)

      Math.max(max, row * 8 + col)
    }
  }

  def solutionTwo: Long = {
    def findRowOrCol(seatCode: String, isRowSearch: Boolean): Int = {
      val (lowerChar, maxRange) = if (isRowSearch) ('F', 128) else ('L', 8)
      seatCode
        .foldLeft(Range(0, maxRange)) { (r, letter) =>
          val delta = (r.upper - r.lower) / 2
          if (letter == lowerChar) Range(r.lower, r.upper - delta)
          else Range(r.lower + delta, r.upper)
        }
        .lower
    }

    val seatIds = Source.fromResource("day05.txt").getLines.toSeq.foldLeft(Seq.empty[Long]) {
      (seatIds, line) =>
        val row = findRowOrCol(seatCode = line.dropRight(3), isRowSearch = true)
        val col = findRowOrCol(seatCode = line.drop(7), isRowSearch      = false)

        seatIds :+ row * 8 + col
    }

    val gaussFormula = seatIds.length * (seatIds.length + 1) / 2
    val shiftedSum   = seatIds.min * (seatIds.length + 1)
    gaussFormula - (seatIds.sum - shiftedSum)
  }
}
