import scala.io.Source
import scala.util.Try

object Day13 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    val input     = Source.fromResource("day13.txt").getLines
    val startTime = input.next().toDouble
    val bestBus = input
      .next()
      .split(",")
      .flatMap(_.toIntOption)
      // The the first common multiple for the busTime after the startTime is the first bus you can take
      .map(busTime => (busTime, (Math.ceil(startTime / busTime) * busTime) - startTime))
      .minBy(_._2)

    bestBus._1 * bestBus._2.toInt
  }

  def solutionTwo: Long = {
    object extractBusTimes {
      def unapply(busTime: (String, Int)): Option[BusTime] = {
        Try(BusTime(busTime._1.toInt, busTime._2)).toOption
      }
    }

    final case class BusTime(time: Int, timeShift: Int)

    val input = Source.fromResource("day13.txt").getLines.drop(1)
    val busTimes = input
      .next()
      .split(",")
      .zipWithIndex
      .flatMap(extractBusTimes.unapply)
      .sortBy(-_.time)

    val validators = busTimes.map {
      busTime =>
        { testTime: Long =>
          (testTime + busTime.timeShift) % busTime.time == 0
        }
    }.toSeq

    val startingTestTime = busTimes.head.time - busTimes.head.timeShift

    // Find a time that fits for a subset of busTimes including a new busTime, incrementing by the LCM of the subset of
    // those busTimes. If a time is found, adjust the LCM incrementer to include the new busTime. Continue until a
    // time is found that satisfies all busTimes.
    val res = LazyList.unfold((startingTestTime.toLong, busTimes.head.time.toLong, 1)) {
      case (_, _, index) if validators.length == index => None
      case (time, incrLcm, index) if !validators(index)(time) =>
        Some(time, (time + incrLcm, incrLcm, index))
      case (time, incrLcm, index) if validators(index)(time) => {
        // This incrementer works because all busTimes must be prime. This is also why there is no need to revalidate
        // prior busTimes.
        val newIncrLcm = incrLcm * busTimes(index).time
        Some(time, (time + newIncrLcm, newIncrLcm, index + 1))
      }
      case _ => None
    }

    res.last
  }
}
