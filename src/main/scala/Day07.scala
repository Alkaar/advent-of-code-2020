import scala.io.Source

object Day07 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  object SetExtractor {
    def unapply[T](s: Set[T]): Option[Set[T]] = if (s.nonEmpty) Some(s) else None
  }

  def solutionOne: Int = {
    val rules = Source
      .fromResource("day07.txt")
      .getLines
      .toSeq
      .foldLeft(Seq.empty[(String, Set[String])]) { (rules, rule) =>
        val keyValueRules = rule.split("contain")

        val newRules =
          keyValueRules(1)
            .split(",")
            .map { v =>
              (
                """^[0-9\s]*(.*?) bag.*$""".r
                  .findAllIn(v)
                  .group(1),
                Set(keyValueRules.head.replaceAll(" bag.*$", ""))
              )
            }
        rules ++ newRules
      }
      .groupMapReduce(_._1)(_._2)(_ ++ _)

    LazyList
      .unfold(Set("shiny gold")) {
        case SetExtractor(bags) => Some(bags, bags.flatMap(rules.get).flatten)
        case _                  => None
      }
      .flatten
      .toSet
      .size - 1
  }

  def solutionTwo: Int = {
    val rules = Source
      .fromResource("day07.txt")
      .getLines
      .toSeq
      .foldLeft(Seq.empty[(String, Map[String, String])]) { (rules, rule) =>
        val keyValueRules = rule.split("contain")

        val newRules =
          keyValueRules(1)
            .split(",")
            .map { v =>
              val res = """^\s([0-9]*)\s*(.*?) bag.*$""".r.findAllIn(v)
              (
                keyValueRules.head.replaceAll(" bag.*$", ""),
                Map((res.group(2), res.group(1)))
              )
            }
        rules ++ newRules
      }
      .groupMapReduce(_._1)(_._2)(_ ++ _)

    def numBags(bag: String): Int = {
      rules.get(bag) match {
        case Some(anotherBag) if anotherBag.head._1 == "no other" => 1
        case Some(anotherBag) =>
          anotherBag
            .map(b => numBags(b._1) * b._2.toInt)
            .sum + 1
        case _ => 1
      }
    }

    numBags(bag = "shiny gold") - 1
  }
}
