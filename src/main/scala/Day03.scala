import scala.Console.println
import scala.io.Source

object Day03 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input  = Source.fromResource("day03.txt").getLines.toSeq
    val forest = Array.ofDim[Char](input.length, input.head.length)

    for {
      y <- input.indices
      x <- input(y).indices
    } {
      forest(y)(x) = input(y)(x)
    }

    List
      .range(1, forest.length)
      .map { y =>
        val nextCoor = y * 3
        val x        = nextCoor + (nextCoor % 3)

        forest(y)(x % forest(y).length)
      }
      .count(_ == '#')
  }

  def solutionTwo: Long = {
    def numTrees(xSlope: Int, ySlope: Int): Long = {
      val input  = Source.fromResource("day03.txt").getLines.toSeq
      val forest = Array.ofDim[Char](input.length, input.head.length)

      for {
        y <- input.indices
        x <- input(y).indices
      } {
        forest(y)(x) = input(y)(x)
      }

      List
        .range(ySlope, forest.length, ySlope)
        .map { y =>
          val nextCol = y / ySlope * xSlope
          val x       = nextCol + (nextCol % xSlope)

          forest(y)(x % forest(y).length)
        }
        .count(_ == '#')
        .toLong
    }

    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(slope => numTrees(slope._1, slope._2))
      .product
  }
}
