import scala.annotation.tailrec
import scala.io.Source

object Day23 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: String = {
    def makeMove(cups: List[Int]): List[Int] = {
      val removedCups      = cups.slice(1, 4)
      val newCupList       = cups.drop(4)
      val possibleDestCups = newCupList.filter(_ < cups.head)
      val destCup =
        Option.when(possibleDestCups.nonEmpty)(possibleDestCups.max).getOrElse(newCupList.max)

      newCupList.take(newCupList.indexOf(destCup) + 1) ++
        removedCups ++
        newCupList.drop(newCupList.indexOf(destCup) + 1) ++
        cups.take(1)
    }

    val cups =
      Source.fromResource("day23.txt").getLines.next.split("").flatMap(_.toIntOption).toList

    val res = (1 to 100)
      .foldLeft(cups) { (cups, _) =>
        makeMove(cups)
      }

    (res.dropWhile(_ != 1).drop(1) ++ res.takeWhile(_ != 1)).mkString("")
  }

  def solutionTwo: Long = {
    val totalNumCups = 1000000

    def makeMove(currCup: Int, cupToCup: Map[Int, Int]): (Int, Map[Int, Int]) = {
      @tailrec
      def findDestCup(cup: Int, removedCups: List[Int]): Int = {
        val maybeDestCup = cup - 1
        cupToCup.get(maybeDestCup) match {
          case Some(_) if !removedCups.contains(maybeDestCup) => maybeDestCup
          case None =>
            @tailrec
            def findExceptionDestCup(testCup: Int): Int = {
              if (!removedCups.contains(testCup))
                testCup
              else
                findExceptionDestCup(testCup - 1)
            }

            findExceptionDestCup(totalNumCups)
          case _ => findDestCup(maybeDestCup, removedCups)
        }
      }

      val removedCup1 = cupToCup(currCup)
      val removedCup2 = cupToCup(removedCup1)
      val removedCup3 = cupToCup(removedCup2)
      val destCup     = findDestCup(currCup, List(removedCup1, removedCup2, removedCup3))

      val newCurrCup = cupToCup(removedCup3)

      val oldPointer = cupToCup(destCup)

      (
        newCurrCup,
        cupToCup
          .updated(currCup, newCurrCup)
          .updated(destCup, removedCup1)
          .updated(removedCup3, oldPointer)
      )
    }

    val startCups = Source.fromResource("day23.txt").getLines.next.split("").flatMap(_.toIntOption)
    val extraCups = startCups ++ (startCups.max + 1 to (totalNumCups - startCups.length + startCups.max)).toList
    val allCups   = startCups ++ extraCups
    val currCup   = startCups.head
    val lastCup   = allCups.last

    val cupToCup =
      allCups
        .sliding(2)
        .foldLeft(Map.empty[Int, Int]) {
          case (cupToCup, cup) =>
            cupToCup.updated(cup.head, cup(1))
        }

    val cupState = (1 to 10000000)
      .foldLeft(currCup, cupToCup.updated(lastCup, currCup)) {
        case ((currCup, cupToCup), _) =>
          makeMove(currCup, cupToCup)
      }
      ._2

    val firstCup = cupState(1)

    firstCup.toLong * cupState(firstCup).toLong
  }
}
