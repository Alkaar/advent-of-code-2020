import cats.implicits._

import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  def solutionOne: Int = {
    final case class Cube(y: Int, x: Int, z: Int)

    def initConwayCube(): Set[Cube] = {
      Source
        .fromResource("day17.txt")
        .getLines
        .foldLeft(Set.empty[Cube], 0) {
          case ((conwayCube, rowIndex), row) =>
            val newConwayCube = row
              .split("")
              .zipWithIndex
              .collect {
                case (cubeState, colIndex) if cubeState == "#" => Cube(rowIndex, colIndex, 0)
              }
              .toSet
            (conwayCube ++ newConwayCube, rowIndex + 1)
        }
        ._1
    }

    def playCycle(conwayCube: Set[Cube]): Set[Cube] = {
      conwayCube
        .foldLeft(Set.empty[Cube], Set.empty[Cube]) {
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

    def generateNeighbors(cube: Cube): List[Cube] = {
      (for {
        y <- cube.y - 1 to cube.y + 1
        x <- cube.x - 1 to cube.x + 1
        z <- cube.z - 1 to cube.z + 1
      } yield Cube(y, x, z)).diff(List(cube)).toList
    }

    def isCubeActive(conwayCube: Set[Cube], cube: Cube, isActive: Boolean): Option[Cube] = {
      val numActive = generateNeighbors(cube).count(conwayCube.contains)

      if (numActive == 3 || (isActive && 2 == numActive)) Some(cube) else None
    }

    @tailrec
    def playCycles(numCycles: Int, conwayCube: Set[Cube] = initConwayCube()): Set[Cube] = {
      if (numCycles == 0) conwayCube else playCycles(numCycles - 1, playCycle(conwayCube))
    }

    playCycles(6).size
  }

  def solutionTwo: Long = {
    final case class HyperCube(y: Int, x: Int, z: Int, w: Int)

    def initConwayCube(): Set[HyperCube] = {
      Source
        .fromResource("day17.txt")
        .getLines
        .foldLeft(Set.empty[HyperCube], 0) {
          case ((conwayCube, rowIndex), row) =>
            val newConwayCube = row
              .split("")
              .zipWithIndex
              .collect {
                case (cubeState, colIndex) if cubeState == "#" =>
                  HyperCube(rowIndex, colIndex, 0, 0)
              }
              .toSet
            (conwayCube ++ newConwayCube, rowIndex + 1)
        }
        ._1
    }

    def playCycle(conwayCube: Set[HyperCube]): Set[HyperCube] = {
      conwayCube
        .foldLeft(Set.empty[HyperCube], Set.empty[HyperCube]) {
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

    def generateNeighbors(cube: HyperCube): List[HyperCube] = {
      (for {
        y <- cube.y - 1 to cube.y + 1
        x <- cube.x - 1 to cube.x + 1
        z <- cube.z - 1 to cube.z + 1
        w <- cube.w - 1 to cube.w + 1
      } yield HyperCube(y, x, z, w)).diff(List(cube)).toList
    }

    def isCubeActive(
      conwayCube: Set[HyperCube],
      cube: HyperCube,
      isActive: Boolean
    ): Option[HyperCube] = {
      val numActive = generateNeighbors(cube).count(conwayCube.contains)

      if (numActive == 3 || (isActive && 2 == numActive)) Some(cube) else None
    }

    @tailrec
    def playCycles(
      numCycles: Int,
      conwayCube: Set[HyperCube] = initConwayCube()
    ): Set[HyperCube] = {
      if (numCycles == 0) conwayCube else playCycles(numCycles - 1, playCycle(conwayCube))
    }

    playCycles(6).size
  }
}
