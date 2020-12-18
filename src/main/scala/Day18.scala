import scala.io.Source
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.matching.Regex

object Day18 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def changePrecedence(expr: String, operators: List[Char]): String = {
    val ops = operators.foldLeft("")(_ + _)

    val fixOps = operators.map { op => s: String =>
      s.replace(s""" ${op} (""", s""".${op}(""")
    }

    val wrapNumsInParens = new Regex(s""" ([${ops}]) (\\d+)""", "operator", "int")
      .replaceAllIn(expr, m => s".${m.group("operator")}(${m.group("int")})")

    val wrapSubExprInParens = fixOps
      .foldLeft(wrapNumsInParens) {
        case (newExpr, fixOps) => fixOps(newExpr)
      }

    new Regex("""(\d+)""", "int").replaceAllIn(wrapSubExprInParens, m => s"${m.group("int")}L")
  }

  def runExpr(stringExpr: String, operators: List[Char]): Long = {
    val toolbox = currentMirror.mkToolBox()

    val expr = toolbox.parse(changePrecedence(stringExpr, operators))

    toolbox.compile(expr)().asInstanceOf[Long]
  }

  def solutionOne: Long = {
    val input = Source.fromResource("day18.txt").getLines

    input.foldLeft(0L) {
      case (sum, stringExpr) => {
        sum + runExpr(stringExpr, List('+', '*'))
      }
    }
  }

  def solutionTwo: Long = {
    val input = Source.fromResource("day18.txt").getLines
    input.foldLeft(0L) {
      case (sum, stringExpr) => {
        sum + runExpr(stringExpr, List('+'))
      }
    }
  }
}
