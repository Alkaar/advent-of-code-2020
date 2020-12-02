import scala.io.Source

object Day01 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input = Source.fromResource("day01.txt").getLines.toIndexedSeq

    val numPairs = for {
      i <- input.indices
      j <- i until input.length
    } yield (input(i).toInt, input(j).toInt)

    numPairs.foldLeft(0)((res, cur) => if (cur._1 + cur._2 == 2020) cur._1 * cur._2 else res)
  }

  def solutionTwo: Int = {
    val input = Source.fromResource("day01.txt").getLines.toIndexedSeq

    val numPairs = for {
      i <- input.indices
      j <- i until input.length
      k <- j until input.length
    } yield (input(i).toInt, input(j).toInt, input(k).toInt)

    numPairs.foldLeft(0)(
      (res, cur) => if (cur._1 + cur._2 + cur._3 == 2020) cur._1 * cur._2 * cur._3 else res
    )
  }
}
