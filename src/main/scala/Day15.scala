import scala.io.Source

object Day15 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  object MemoryGame {

    val input =
      Source.fromResource("day15.txt").getLines.next().split(",").flatMap(_.toIntOption).toSeq
    val map = input.dropRight(1).zipWithIndex.toMap

    def play(rounds: Int) = {
      val res = LazyList
        .unfold(input.last, input.length - 1, map) {
          case (num, index, map) if map.contains(num) => {
            val value = index - map(num)
            Some(value, (value, index + 1, map.updated(num, index)))
          }
          case (num, index, map) => Some(0, (0, index + 1, map.updated(num, index)))
        }
        .take(rounds - input.length)
        .toList

      (input ++ res)(rounds - 1)
    }
  }

  def solutionOne: Int = {
    MemoryGame.play(2020)
  }

  def solutionTwo: Int = {
    MemoryGame.play(30000000)
  }
}
