import scala.io.Source
import cats.implicits._

object Day16 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input = Source.fromResource("day16.txt").getLines
    val finalRules = input.takeWhile(_.nonEmpty).foldLeft(Set.empty[Int]) {
      case (rules, s"${_}: ${low1}-${high1} or ${low2}-${high2}") =>
        rules ++ (low1.toInt to high1.toInt) ++ (low2.toInt to high2.toInt)
      case (rules, _) => rules
    }

    input
      .dropWhile(_ != "nearby tickets:")
      .foldLeft(Seq.empty[Int]) {
        case (invalidValues, values) =>
          invalidValues ++ values.split(",").flatMap(_.toIntOption).toSet.diff(finalRules)
        case (invalidValues, _) => invalidValues
      }
      .sum
  }

  def solutionTwo: Long = {
    val input = Source.fromResource("day16.txt").getLines

    val finalRules = input.takeWhile(_.nonEmpty).foldLeft(Map.empty[String, Int => Boolean]) {
      case (rules, s"${ruleName}: ${low1}-${high1} or ${low2}-${high2}") =>
        rules.updated(ruleName, { value: Int =>
          (low1.toInt <= value && value <= high1.toInt) || (low2.toInt <= value && value <= high2.toInt)
        })
      case (rules, _) => rules
    }

    val yourTicket = input.drop(1).next().split(",").flatMap(_.toLongOption)

    val startingFields = finalRules.keys.map(_ -> yourTicket.indices.toSet).toMap

    val validFields = input.drop(2).foldLeft(startingFields) { (fields, line) =>
      val values = line.split(",").flatMap(_.toIntOption)
      if (values.exists(value => finalRules.values.forall(!_(value))))
        fields
      else
        fields.foldLeft(Map.empty[String, Set[Int]]) {
          case (newFields, (fieldName, possibleFields)) =>
            val invalidFields = values.zipWithIndex.toList.mapFilter {
              case (value, index) if !finalRules(fieldName)(value) => Some(index)
              case _                                               => None
            }

            newFields.updated(fieldName, possibleFields diff invalidFields.toSet)
        }
    }

    LazyList
      .unfold(validFields) { validFields =>
        validFields.find(_._2.size == 1) match {
          case Some((fieldName, fieldIndex)) =>
            Some(
              fieldName -> fieldIndex.head,
              (validFields - fieldName).map(kv => kv._1 -> (kv._2 diff fieldIndex))
            )
          case _ => None
        }
      }
      .toList
      .mapFilter {
        case (fieldName, fieldIndex) if fieldName contains "departure" => Some(fieldIndex)
        case _                                                         => None
      }
      .map(yourTicket.apply)
      .product
  }
}
