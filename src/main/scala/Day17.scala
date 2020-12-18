import scala.annotation.tailrec
import scala.io.Source
import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe._

object Day17 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def initConwayCube[T <: Product with Serializable: TypeTag: ClassTag](): Set[T] = {
    Source
      .fromResource("day17.txt")
      .getLines
      .foldLeft(Set.empty[T], 0) {
        case ((conwayCube, rowIndex), row) =>
          val newConwayCube = row
            .split("")
            .zipWithIndex
            .collect[T] {
              case (cubeState, colIndex) if cubeState == "#" =>
                val numExtraAxes = typeTag[T].tpe.decls.collect {
                  case meth: MethodSymbol if meth.isCaseAccessor => true
                }.size

                classTag[T].runtimeClass.getConstructors.head
                  .newInstance(List(rowIndex, colIndex).padTo(numExtraAxes, 0): _*)
                  .asInstanceOf[T]
            }
            .toSet
          (conwayCube ++ newConwayCube, rowIndex + 1)
      }
      ._1
  }

  def playCycle[T <: Product with Serializable: ClassTag](conwayCube: Set[T]): Set[T] = {
    conwayCube
      .foldLeft(Set.empty[T], Set.empty[T]) {
        case ((newConwayCube, visited), cube) =>
          val cubeNeighbors = generateNeighbors(cube)
          val partialConwayCube = cubeNeighbors
            .filterNot(visited.contains)
            .map(isCubeActive(conwayCube, _, false))
          (
            newConwayCube ++ (partialConwayCube :+ isCubeActive(conwayCube, cube, true)).flatten,
            visited ++ (cubeNeighbors :+ cube)
          )
      }
      ._1
  }

  def generateNeighbors[T <: Product with Serializable: ClassTag](cube: T): List[T] = {
    @tailrec
    def generateNeighborsInner[T <: Product with Serializable: ClassTag](
      cubeAxes: List[Int],
      accNeighborCubes: List[List[Int]],
      index: Int = 0
    ): List[T] = {
      if (index == cubeAxes.length) {
        accNeighborCubes.map(
          classTag[T].runtimeClass.getConstructors.head
            .newInstance(_: _*)
            .asInstanceOf[T]
        )
      } else {
        val newAcc = for {
          i <- accNeighborCubes
          j <- List(cubeAxes(index) - 1, cubeAxes(index), cubeAxes(index) + 1)
        } yield i :+ j

        generateNeighborsInner[T](cubeAxes, newAcc, index + 1)
      }
    }

    val cubeAxes = cube.productIterator.toList.asInstanceOf[List[Int]]

    generateNeighborsInner(
      cubeAxes.drop(1),
      List(List(cubeAxes.head - 1), List(cubeAxes.head), List(cubeAxes.head + 1))
    ).diff(List(cube))
  }

  def isCubeActive[T <: Product with Serializable: ClassTag](
    conwayCube: Set[T],
    cube: T,
    isActive: Boolean
  ): Option[T] = {
    val numActive = generateNeighbors(cube).count(conwayCube.contains)

    if (numActive == 3 || (isActive && 2 == numActive)) Some(cube) else None
  }

  @tailrec
  def playCycles[T <: Product with Serializable: ClassTag](
    conwayCube: Set[T],
    numCycles: Int
  ): Set[T] = {
    if (numCycles == 0) conwayCube else playCycles(playCycle(conwayCube), numCycles - 1)
  }

  final case class Cube(y: Int, x: Int, z: Int)

  def solutionOne: Int = {
    playCycles[Cube](initConwayCube[Cube](), 6).size
  }

  final case class HyperCube(y: Int, x: Int, z: Int, w: Int)

  def solutionTwo: Int = {
    playCycles[HyperCube](initConwayCube[HyperCube](), 6).size
  }
}
