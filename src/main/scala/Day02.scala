import scala.io.Source

object Day02 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    Source.fromResource("day02.txt").getLines.toSeq.foldLeft(0) { (totalValid, line) =>
      val regex                         = """(\d+)-(\d+)\s(\w):\s(\w+)""".r
      val regex(min, max, letter, pass) = line
      val occurrences                   = pass.map(c => if (c.toString == letter) 1 else 0).sum

      if (min.toInt <= occurrences && occurrences <= max.toInt) totalValid + 1 else totalValid
    }
  }

  def solutionTwo: Int = {
    Source.fromResource("day02.txt").getLines.toSeq.foldLeft(0) { (totalValid, line) =>
      val regex                         = """(\d+)-(\d+)\s(\w):\s(\w+)""".r
      val regex(min, max, letter, pass) = line

      (pass.toList(min.toInt - 1).toString, pass.toList(max.toInt - 1).toString) match {
        case (first, second) if first != second && (first == letter || second == letter) =>
          totalValid + 1
        case _ => totalValid
      }
    }
  }
}
