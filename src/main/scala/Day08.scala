import scala.annotation.tailrec
import scala.io.Source

object Day08 {

  object signedNum {
    def unapply(s: String): Option[Int] = s.toIntOption
  }

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {

    val steps = Source
      .fromResource("day08.txt")
      .getLines
      .map {
        case s"$step ${signedNum(num)}" => (step, num)
      }
      .toIndexedSeq

    @tailrec
    def getTotal(visited: Set[Int] = Set.empty, index: Int = 0, counter: Int = 0): Int = {
      if (visited.contains(index)) counter
      else {
        val (newIndex, newCounter) = steps(index) match {
          case ("nop", _)   => (index + 1, counter)
          case ("acc", num) => (index + 1, counter + num)
          case ("jmp", num) => (index + num, counter)
        }

        getTotal(visited + index, newIndex, newCounter)
      }
    }

    getTotal()
  }

  def solutionTwo: Int = {
    val steps = Source
      .fromResource("day08.txt")
      .getLines
      .map {
        case s"$step ${signedNum(num)}" => (step, num)
      }
      .toIndexedSeq

    def getFiniteLoopNum(
      steps: IndexedSeq[(String, Int)],
      visited: Set[Int] = Set.empty,
      index: Int = 0,
      counter: Int = 0
    ): Option[Int] = {
      @tailrec
      def getFiniteLoopNumImpl(visited: Set[Int], index: Int, counter: Int): Option[Int] = {
        if (visited.contains(index)) None
        else {
          val (newIndex, newCounter) = steps(index) match {
            case ("nop", _)   => (index + 1, counter)
            case ("acc", num) => (index + 1, counter + num)
            case ("jmp", num) => (index + num, counter)
          }

          if (steps.length == newIndex) Some(newCounter)
          else getFiniteLoopNumImpl(visited + index, newIndex, newCounter)
        }
      }

      getFiniteLoopNumImpl(visited, index, counter)
    }

    @tailrec
    def fixBootCode(testSteps: IndexedSeq[(String, Int)] = steps, index: Int = 0): Int = {
      getFiniteLoopNum(testSteps) match {
        case None => {
          val newSteps =
            steps(index) match {
              case ("nop", num) => (steps.take(index) :+ ("jmp", num)) ++ steps.drop(index + 1)
              case ("jmp", num) => (steps.take(index) :+ ("nop", num)) ++ steps.drop(index + 1)
              case _            => steps
            }

          fixBootCode(newSteps, index + 1)
        }
        case Some(res) => res
      }
    }

    fixBootCode()
  }
}
