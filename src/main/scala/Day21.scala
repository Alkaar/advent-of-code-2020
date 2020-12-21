import scala.io.Source
import scala.util.Try
import cats.implicits._

object Day21 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  type Ingredient = (String, Int)
  final case class Allergy(allergen: String, ingredients: Set[String])

  object FoodExtractor {

    def unapply(s: String): Option[(List[Ingredient], List[Allergy])] = {
      s match {
        case s"${ingredients} (contains ${allergens})" =>
          Try {
            val ingredidentList = ingredients.split(" ")

            val foods = ingredidentList.map(_ -> 1).toList
            val allergies = allergens
              .replace(",", "")
              .split(" ")
              .map(allergen => Allergy(allergen, ingredidentList.toSet))
              .toList

            (foods, allergies)
          }.toOption
        case _ => None
      }
    }
  }

  def getAllIngredients: (Map[String, Int], Set[(String, String)]) = {
    val (ingredients, allergies) = Source
      .fromResource("day21.txt")
      .getLines
      .foldLeft(Map.empty[String, Int], List.empty[Allergy]) {
        case ((ingredients, allergies), FoodExtractor(ingredient, allergy)) =>
          (ingredients.combine(ingredient.toMap), allergies ++ allergy)
        case ((ingredients, allergies), _) => (ingredients, allergies)
      }

    val possibleAllergies = allergies.groupMapReduce(_.allergen)(_.ingredients)(_ intersect _)

    val allergicIngredients = LazyList
      .unfold(possibleAllergies) { possibleAllergies =>
        possibleAllergies.find { case (_, ingredients) => ingredients.size == 1 } match {
          case Some((allergen, ingredient)) =>
            Some(
              (ingredient.head, allergen),
              (possibleAllergies - allergen).map(kv => kv._1 -> (kv._2 diff ingredient))
            )
          case _ => None
        }
      }
      .toSet

    (ingredients, allergicIngredients)
  }

  def solutionOne: Int = {
    val (ingredients, allergicIngredients) = getAllIngredients

    ingredients.removedAll(allergicIngredients.map { case (_, allergen) => allergen }).values.sum
  }

  def solutionTwo: String = {
    val (_, allergicIngredients) = getAllIngredients

    allergicIngredients.toList
      .sortBy { case (_, allergen) => allergen }
      .map { case (ingredient, _) => ingredient }
      .mkString(",")
  }
}
