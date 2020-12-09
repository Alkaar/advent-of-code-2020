import scala.annotation.tailrec
import scala.io.Source
import cats.implicits._

object Day09 {
  case class Acc(lowerPtr: Int, upperPtr: Int, sum: Long)

  object strToLong {
    def unapply(s: String): Option[Long] = s.toLongOption
  }

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Long = {
    val numPreamble = 25

    val input    = Source.fromResource("day09.txt").getLines
    val preamble = input.take(numPreamble).map(_.toLong).toSeq

    @tailrec
    def findInvalidNum(preamble: Seq[Long], lookup: Set[Long]): Long = {
      input.next match {
        case strToLong(num) if preamble.exists(a => lookup.contains(num - a)) =>
          findInvalidNum(preamble.drop(1) :+ num, (lookup - preamble.head) + num)
        case strToLong(num) => num
        case _              => throw new NumberFormatException("NAN")
      }
    }

    findInvalidNum(preamble, preamble.toSet)
  }

  def solutionTwo: Long = {
    val numPreamble = 25

    val input    = Source.fromResource("day09.txt").getLines.map(_.toLong).toList
    val preamble = input.take(numPreamble)

    @tailrec
    def findInvalidNum(preamble: Seq[Long], lookup: Set[Long], index: Int = numPreamble): Long = {
      input(index) match {
        case num if preamble.exists(a => lookup.contains(num - a)) =>
          findInvalidNum(preamble.drop(1) :+ num, (lookup - preamble.head) + num, index + 1)
        case num => num
        case _   => throw new NumberFormatException("NAN")
      }
    }

    val invalidNum = findInvalidNum(preamble, preamble.toSet)

    val res = input.drop(2).foldM(Acc(0, 1, input.head + input.tail.head)) {
      case (acc, _) if acc.sum == invalidNum =>
        val subSeq = input.slice(acc.lowerPtr, acc.upperPtr + 1)
        (subSeq.min + subSeq.max).asLeft
      case (acc, num) if acc.sum < invalidNum =>
        Acc(acc.lowerPtr, acc.upperPtr + 1, acc.sum + num).asRight
      case (acc, num) =>
        @tailrec
        def reduceSum(acc: Acc): Acc = {
          if (acc.sum == invalidNum) acc
          else if (acc.sum < invalidNum) Acc(acc.lowerPtr, acc.upperPtr + 1, acc.sum + num)
          else reduceSum(Acc(acc.lowerPtr + 1, acc.upperPtr, acc.sum - input(acc.lowerPtr)))
        }
        reduceSum(acc).asRight
    }

    res.left.getOrElse(throw new Exception("Could not determine encryption weakness"))
  }
}
