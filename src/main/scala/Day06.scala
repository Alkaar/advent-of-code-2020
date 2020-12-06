import scala.io.Source

object Day06 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    (Source
      .fromResource("day06.txt")
      .getLines ++ Iterator(""))
      .foldLeft((0, Set.empty[String])) {
        case ((count, numYes), "")   => (count + numYes.size, Set.empty[String])
        case ((count, numYes), form) => (count, numYes ++ form.split(""))
      }
      ._1
  }

  def solutionTwo: Int = {
    (Source
      .fromResource("day06.txt")
      .getLines ++ Iterator(""))
      .foldLeft((0, List.empty[List[String]])) {
        case ((count, numYes), "") =>
          (count + numYes.reduce(_ intersect _).length, List.empty[List[String]])
        case ((count, numYes), form) => (count, numYes :+ form.split("").toList)
      }
      ._1
  }
}
