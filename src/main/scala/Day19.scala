import scala.io.Source

object Day19 {

  def main(args: Array[String]): Unit = {
    println(s"First answer: ${solutionOne}")
    println(s"Second answer: ${solutionTwo}")
  }

  sealed trait Rule
  final case class BaseRules(rules: List[Int])                          extends Rule
  final case class OrRules(leftRules: List[Int], rightRules: List[Int]) extends Rule
  final case class LetterRule(letter: String)                           extends Rule

  object RuleExtractor {

    def unapply(s: String): Option[Rule] = {
      s match {
        case s""""${letter}"""" => Some(LetterRule(letter))
        case s"${subRules1} | ${subRules2}" =>
          Some(
            OrRules(
              subRules1.split(" ").flatMap(_.toIntOption).toList,
              subRules2.split(" ").flatMap(_.toIntOption).toList
            )
          )
        case s"${subRules}" => Some(BaseRules(subRules.split(" ").flatMap(_.toIntOption).toList))
        case _              => None
      }
    }
  }

  def buildValidStrings(rulesMap: Map[Int, Rule], ruleNum: Int = 0) = {
    def ruleNumsToPatterns(ruleNums: List[Int]): Set[String] = {
      ruleNums
        .map(ruleNum => buildValidStringsInner(rulesMap(ruleNum)))
        .foldLeft(Set(""))(cartesianJoin)
    }

    def cartesianJoin(accPatterns: Set[String], patterns: Set[String]): Set[String] = {
      accPatterns.flatMap(pattern => patterns.map(pattern + _))
    }

    def buildValidStringsInner(rule: Rule): Set[String] = {
      rule match {
        case letter: LetterRule => Set(letter.letter)
        case orRules: OrRules =>
          ruleNumsToPatterns(orRules.leftRules) ++ ruleNumsToPatterns(orRules.rightRules)
        case baseRules: BaseRules =>
          ruleNumsToPatterns(baseRules.rules)
      }
    }

    buildValidStringsInner(rulesMap(ruleNum))
  }

  def solutionOne: Int = {
    val input = Source.fromResource("day19.txt").getLines

    val rulesMap = input.takeWhile(_.nonEmpty).foldLeft(Map.empty[Int, Rule]) {
      case (rules, s"""${ruleNum}: ${RuleExtractor(rule)}""") => rules.updated(ruleNum.toInt, rule)
    }

    val validStrings = buildValidStrings(rulesMap)

    input.foldLeft(0) {
      case (acc, str) if validStrings.contains(str) => acc + 1
      case (acc, _)                                 => acc
    }
  }

  // The below solution is coded assuming the following overrides in the input file which creates cycles
  // 8: 42 | 42 8
  // 11: 42 31 | 42 11 31
  def solutionTwo: Int = {
    val input = Source.fromResource("day19.txt").getLines

    val rulesMap = input.takeWhile(_.nonEmpty).foldLeft(Map.empty[Int, Rule]) {
      case (rules, s"""${ruleNum}: ${RuleExtractor(rule)}""") => rules.updated(ruleNum.toInt, rule)
    }

    val rule42patterns = buildValidStrings(rulesMap.updated(11, BaseRules(List(42))), 11)
    val rule31patterns = buildValidStrings(rulesMap.updated(11, BaseRules(List(31))), 11)

    // Not sure if this was by accident or on purpose but the length of both the 42 and 31 patterns are always the
    // same length. Operating with this assumption simplifies the code below.
    val numPatternLength = (rule42patterns ++ rule31patterns).groupBy(_.length).head._1

    def validWithRuleLoops(s: String, rule42Cnt: Int = 0, rule31Cnt: Int = 0): Boolean = {
      lazy val isPattern42Match = rule42patterns.contains(s.take(numPatternLength))
      lazy val isPattern31Match = rule31patterns.contains(s.takeRight(numPatternLength))

      // The reason the below works is because the only possible patterns are 42 patterns followed by 31 patterns
      // following the validation rules in the conditional below
      if (s.isEmpty && rule42Cnt > rule31Cnt && rule42Cnt >= 2 && rule31Cnt >= 1)
        true
      else if (isPattern42Match && isPattern31Match)
        validWithRuleLoops(s.drop(numPatternLength), rule42Cnt + 1, rule31Cnt) ||
        validWithRuleLoops(s.dropRight(numPatternLength), rule42Cnt, rule31Cnt + 1)
      else if (isPattern42Match)
        validWithRuleLoops(s.drop(numPatternLength), rule42Cnt + 1, rule31Cnt)
      else if (isPattern31Match)
        validWithRuleLoops(s.dropRight(numPatternLength), rule42Cnt, rule31Cnt + 1)
      else
        false
    }

    input.foldLeft(0) {
      case (acc, str) if validWithRuleLoops(str) => acc + 1
      case (acc, _)                              => acc
    }
  }
}
