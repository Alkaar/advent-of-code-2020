import scala.annotation.tailrec
import scala.io.Source

object Day25 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: https://adventofcode.com/2020/day/25/answer")
  }

  def solutionOne: Long = {
    @tailrec
    def findLoopSize(publicKey: Int, loopSize: Int = 0, value: Long = 1): Int = {
      if (value == publicKey) loopSize
      else findLoopSize(publicKey, loopSize + 1, transformAlgorithm(value, 7))
    }

    @tailrec
    def transformSubjectNum(subjectNum: Int, loopSize: Int, value: Long = 1): Long = {
      if (loopSize == 0) value
      else transformSubjectNum(subjectNum, loopSize - 1, transformAlgorithm(value, subjectNum))
    }

    def transformAlgorithm(value: Long, subjectNum: Int): Long = value * subjectNum % 20201227

    val input         = Source.fromResource("day25.txt").getLines
    val cardPublicKey = input.next.toInt
    val doorPublicKey = input.next.toInt

    val loopSize = findLoopSize(cardPublicKey)
    transformSubjectNum(doorPublicKey, loopSize)
  }
}
