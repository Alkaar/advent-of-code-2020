import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val adapters = Source
      .fromResource("day10.txt")
      .getLines
      .flatMap(_.toIntOption)
      .toSeq

    (adapters ++ Seq(0, adapters.max + 3)).sorted
      .sliding(2)
      .foldLeft(Map[Int, Int]().withDefaultValue(0)) { (acc, a) =>
        val diff = a(1) - a.head
        acc.updated(diff, acc(diff) + 1)
      }
      .sliding(3)
      .map(m => m(1) * m(3))
      .next
  }

  def solutionTwo: Long = {
    val adapters = Source
      .fromResource("day10.txt")
      .getLines
      .flatMap(_.toIntOption)
      .toIndexedSeq
      .sorted

    def numDistinct(
      testSeq: IndexedSeq[Int] = IndexedSeq(0),
      index: Int = 0,
      memo: Map[Int, Long] = Map.empty[Int, Long]
    ): (Map[Int, Long], Long) = {
      if (index == adapters.length) (Map.empty[Int, Long], 1)
      else {
        memo.get(testSeq.head) match {
          case Some(_) => (Map.empty[Int, Long], memo(testSeq.head))
          case None =>
            adapters
              .drop(index)
              .takeWhile(_ <= testSeq.head + 3)
              .zipWithIndex
              .foldLeft(memo, 0L) {
                case ((oldMemo, oldRes), (adapter, incrIndex)) => {
                  val (newMemo, newRes) =
                    numDistinct(adapter +: testSeq, index + incrIndex + 1, oldMemo)

                  (newMemo.updated(adapter, newRes), oldRes + newRes)
                }
              }
        }
      }
    }
    numDistinct()._2
  }
}
