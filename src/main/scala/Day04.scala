import scala.io.Source
import scala.util
import scala.util.{Failure, Success, Try}

object Day04 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input = Source.fromResource("day04.txt").getLines

    Iterator
      .continually(input.takeWhile(_.length != 0).mkString(" "))
      .takeWhile(_.nonEmpty)
      .map {
        val validFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

        _.split(" ")
          .map(_.takeWhile(_ != ':'))
          .intersect(validFields)
          .length == 7
      }
      .count(_ == true)
  }

  def solutionTwo: Int = {
    def isValidBirthYear(value: String): Boolean =
      Try(value.toInt).map(1920 to 2002 contains _).getOrElse(false)
    def isValidIssueYear(value: String): Boolean =
      Try(value.toInt).map(2010 to 2020 contains _).getOrElse(false)
    def isValidExpirationYear(value: String): Boolean =
      Try(value.toInt).map(2020 to 2030 contains _).getOrElse(false)
    def isValidHeight(value: String): Boolean = {
      value.splitAt(value.length - 2) match {
        case (height, "cm") if Try(height.toInt).map(150 to 193 contains _).getOrElse(false) => true
        case (height, "in") if Try(height.toInt).map(59 to 76 contains _).getOrElse(false)   => true
        case _                                                                               => false
      }
    }
    def isValidHairColor(value: String): Boolean = value.matches("#[0-9a-f]+")
    def isValidEyeColor(value: String): Boolean =
      Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
    def isValidPassportId(value: String): Boolean = value.length == 9 && value.matches("[0-9]+")

    def isValid(key: String, value: String) = {
      if ((key == "byr" && isValidBirthYear(value)) ||
          (key == "iyr" && isValidIssueYear(value)) ||
          (key == "eyr" && isValidExpirationYear(value)) ||
          (key == "hgt" && isValidHeight(value)) ||
          (key == "hcl" && isValidHairColor(value)) ||
          (key == "ecl" && isValidEyeColor(value)) ||
          (key == "pid" && isValidPassportId(value)))
        Some(key)
      else None
    }

    val input = Source.fromResource("day04.txt").getLines

    Iterator
      .continually(input.takeWhile(_.length != 0).mkString(" "))
      .takeWhile(_.nonEmpty)
      .map {
        val validFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

        _.split(" ")
          .flatMap { field =>
            val s"${key}:${value}" = field
            isValid(key, value)
          }
          .intersect(validFields)
          .length == 7
      }
      .count(_ == true)
  }
}
