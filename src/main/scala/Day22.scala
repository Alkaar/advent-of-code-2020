import enumeratum._
import scala.annotation.tailrec
import scala.io.Source

object Day22 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  object IntExtractor {
    def unapply(s: String): Option[Int] = s.toIntOption
  }

  type Deck = List[Int]

  def getDecks: List[Deck] = {
    (::.apply[Deck] _).tupled(
      Source
        .fromResource("day22.txt")
        .getLines
        .foldLeft(List.empty: Deck, List.empty[Deck]) {
          case ((newDeck, decks), IntExtractor(card)) => (newDeck :+ card, decks)
          case ((newDeck, decks), "")                 => (List.empty, decks :+ newDeck)
          case ((newDeck, decks), _)                  => (newDeck, decks)
        }
    )
  }

  def solutionOne: Int = {
    @tailrec
    def playCombat(deck1: Deck, deck2: Deck): Int = {
      if (deck1.isEmpty || deck2.isEmpty) {
        val winningDeck = deck1 ++ deck2
        winningDeck.zip(winningDeck.length to 0 by -1).foldLeft(0) {
          case (total, (card, multiplier)) => total + (card * multiplier)
        }
      } else if (deck1.head > deck2.head) {
        playCombat(deck1.drop(1) :+ deck1.head :+ deck2.head, deck2.drop(1))
      } else {
        playCombat(deck1.drop(1), deck2.drop(1) :+ deck2.head :+ deck1.head)
      }
    }

    val decks = getDecks
    playCombat(decks(1), decks.head)
  }

  def solutionTwo: Int = {
    sealed trait Winner extends EnumEntry

    object Winner extends Enum[Winner] {
      val values = findValues

      case object Player1 extends Winner
      case object Player2 extends Winner
    }

    import Winner._

    def playRecursiveCombat(
      deck1: Deck,
      deck2: Deck,
      prevRounds: Set[Int] = Set.empty
    ): (Winner, Int) = {
      if (prevRounds.contains(List(deck1, deck2).hashCode)) {
        (Player1, 0)
      } else if (deck1.isEmpty || deck2.isEmpty) {
        val winningDeck = deck1 ++ deck2
        val score = winningDeck.zip(winningDeck.length to 0 by -1).foldLeft(0) {
          case (total, (card, multiplier)) => total + (card * multiplier)
        }

        Option.when(deck1.nonEmpty)(Player1, score).getOrElse(Player2, score)
      } else {
        val winner = Option.when(deck1.head < deck1.length && deck2.head < deck2.length)(
          playRecursiveCombat(
            deck1.slice(1, deck1.head + 1),
            deck2.slice(1, deck2.head + 1),
            Set.empty
          )._1
        )

        val newPrevRounds = prevRounds + List(deck1, deck2).hashCode

        if (winner.map(_ == Player1).getOrElse(deck1.head > deck2.head)) {
          playRecursiveCombat(
            deck1.drop(1) :+ deck1.head :+ deck2.head,
            deck2.drop(1),
            newPrevRounds
          )
        } else {
          playRecursiveCombat(
            deck1.drop(1),
            deck2.drop(1) :+ deck2.head :+ deck1.head,
            newPrevRounds
          )
        }
      }
    }

    val decks = getDecks
    playRecursiveCombat(decks(1), decks.head)._2
  }
}
