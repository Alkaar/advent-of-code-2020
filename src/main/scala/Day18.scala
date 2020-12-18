import scala.io.Source
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.matching.Regex

object Day18 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  val toolbox = currentMirror.mkToolBox()

  def solutionOne: Long = {
    val input = Source.fromResource("day18.txt").getLines

    input.foldLeft(0L) {
      case (sum, stringExpr) => {
        val newExpr =
          new Regex(""" ([+*]) (\d+)""", "operator", "int")
            .replaceAllIn(stringExpr, m => s".${m.group("operator")}(${m.group("int")})")
            .replace(" + (", ".+(")
            .replace(" * (", ".*(")

        val newLongExpr =
          new Regex("""(\d+)""", "int").replaceAllIn(newExpr, m => s"${m.group("int")}L")

        val expr = toolbox.parse(newLongExpr)

        sum + toolbox.compile(expr)().asInstanceOf[Long]
      }
    }
  }

  def solutionTwo: Long = {
    val input = Source.fromResource("day18.txt").getLines
    input.foldLeft(0L) {
      case (sum, stringExpr) => {
        val newExpr =
          new Regex(""" \+ (\d+)""", "int")
            .replaceAllIn(stringExpr, m => s".+(${m.group("int")})")
            .replace(" + (", ".+(")

        val newLongExpr =
          new Regex("""(\d+)""", "int").replaceAllIn(newExpr, m => s"${m.group("int")}L")

        val expr = toolbox.parse(newLongExpr)

        sum + toolbox.compile(expr)().asInstanceOf[Long]
      }
    }
  }
}
