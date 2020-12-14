import scala.io.Source
import scala.util.Try
import java.lang.{Long => JavaLong}
import cats.implicits._

object Day14 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Long = {
    final case class Mask(
      private val zerosMask: Long = Long.MaxValue,
      private val onesMask: Long = 0
    ) {
      def +(that: Long): Long = {
        onesMask | that & zerosMask
      }
    }

    object Mask {
      def unapply(s: String): Option[Mask] = {
        s match {
          case s"mask = ${mask}" =>
            Try(
              Mask(
                zerosMask = JavaLong.parseLong(mask.replace('X', '1'), 2),
                onesMask  = JavaLong.parseLong(mask.replace('X', '0'), 2)
              )
            ).toOption
          case _ => None
        }
      }
    }

    Source
      .fromResource("day14.txt")
      .getLines
      .foldLeft(Mask(), Map.empty[String, Long]) {
        case ((_, memory), Mask(newMask)) => (newMask, memory)
        case ((mask, memory), s"mem[${address}] = ${value}") =>
          (mask, memory.updated(address, mask + value.toLong))
        case ((mask, memory), _) => (mask, memory)
      }
      ._2
      .values
      .sum
  }

  def solutionTwo: Long = {
    final case class Mask(
      private val onesMask: Long = 0,
      private val preFloatingMask: Long = 1,
      private val floatingMasks: Seq[Long] = Seq.empty
    ) {
      def +(that: Long): Seq[Long] = {
        floatingMasks.map(_ | (onesMask | that & preFloatingMask))
      }
    }

    object Mask {
      def unapply(s: String): Option[Mask] = {
        s match {
          case s"mask = ${mask}" => {
            val xCols = mask.reverse
              .split("")
              .zipWithIndex
              .toList
              .mapFilter {
                case (value, index) if value == "X" => Some(index)
                case _                              => None
              }

            val floatingMasks = LazyList
              .unfold(Set(0L), 0) {
                case (_, index) if xCols.length == index => None
                case (acc, index) => {
                  val floatingNum = Math.pow(2, xCols(index)).toLong
                  val newAcc      = acc ++ acc.map(_ + floatingNum) ++ Set(floatingNum)
                  Some(newAcc, (newAcc, index + 1))
                }
              }
              .last

            Try(
              Mask(
                onesMask        = JavaLong.parseLong(mask.replace('X', '0'), 2),
                preFloatingMask = JavaLong.parseLong(mask.replace('0', '1').replace('X', '0'), 2),
                floatingMasks   = floatingMasks.toSeq
              )
            ).toOption
          }
          case _ => None
        }
      }
    }

    Source
      .fromResource("day14.txt")
      .getLines
      .foldLeft(Mask(), Map.empty[Long, Long]) {
        case ((_, memory), Mask(newMask)) => (newMask, memory)
        case ((mask, memory), s"mem[${address}] = ${value}") =>
          (mask, memory ++ (mask + address.toLong).map((_, value.toLong)).toMap)
        case ((mask, memory), _) => (mask, memory)
      }
      ._2
      .values
      .sum
  }
}
