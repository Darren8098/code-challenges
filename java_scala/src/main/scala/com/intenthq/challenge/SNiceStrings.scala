package com.intenthq.challenge

import com.intenthq.challenge.Rule.AggregateRule
import com.intenthq.challenge.TrailingChar.{Empty, Passed, Previous}
import com.intenthq.challenge.Verdict.{Naughty, Nice}

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?
object SNiceStrings {
  def nice(xs: List[String]): Int = {
    xs.map(s => judge(s, AggregateRule.empty)).count(_ == Nice)
  }

  private def judge(s: String, rule: Rule): Verdict = {
    val result = s.foldLeft(rule) {
      case (rule, c) => rule.evaluate(c)
    }
    result.verdict
  }
}

sealed trait Verdict {
  /**
   * Combines two Verdicts
   * @param other
   * @return Nice if this and other are both Nice, otherwise Naughty is returned
   */
  def combine(other: Verdict): Verdict =
    if (this == Nice && other == Nice) Nice else Naughty
}

object Verdict {
  case object Nice extends Verdict
  case object Naughty extends Verdict
}

/**
 * A rule to be which can be iteratively applied to a string character by character.
 * The result can be computed at any point resulting in the [[Verdict]] of all processed characters up to that point.
 */
sealed trait Rule {
  /**
   * @return - the verdict of this rule in its current state
   */
  def verdict: Verdict

  /**
   * Given the next char, applies the rule using its current state
   * @param char
   * @return a new rule with the newly calculated state
   */
  def evaluate(char: Char): Rule
}

object Rule {

  /**
   * Keeps a count of each character found from [[mustContain]] on each [[evaluate]] step
   * Results in a [[Nice]] verdict if the number of characters found reaches the [[target]] or [[Naughty]] otherwise
   * @param mustContain - the characters the string must contain
   * @param target - the number of characters from [[mustContain]] characters that must be in the string
   * @param count - the number of characters from [[mustContain]] that have been found in the string so far
   */
  final case class VowelsRule(mustContain: Set[Char], target: Int, count: Int = 0) extends Rule {
    def evaluate(next: Char): VowelsRule =
      if (count < target && mustContain.contains(next)) copy(count = count + 1) else this

    override val verdict: Verdict = if (count >= target) Nice else Naughty
  }

  object VowelsRule {
    private val Vowels = Set('a', 'e', 'i', 'o', 'u')

    def empty: VowelsRule = VowelsRule(Vowels, 3)
  }

  /**
   * Checks for consecutive characters in consecutive evaluate steps
   * Results in a [[Nice]] verdict if a consecutive is found or [[Naughty]] otherwise
   * @param state - keeps tracks the previously evaluated character
   */
  final case class ConsecutiveRule(state: TrailingChar) extends Rule {
    override def verdict: Verdict = if (state == Passed) Nice else Naughty

    override def evaluate(char: Char): ConsecutiveRule = state match {
      case Empty => ConsecutiveRule(Previous(char))
      case Previous(`char`) => ConsecutiveRule(Passed)
      case Previous(_) => ConsecutiveRule(Previous(char))
      case Passed => this
    }
  }

  object ConsecutiveRule {
    def empty: ConsecutiveRule = ConsecutiveRule(TrailingChar.Empty)
  }

  /**
   * Checks for a list of [[disallowedStrings]] resulting in a [[Naughty]] verdict if any are found or [[Nice]] otherwise
   * @param state - keeps track of the previously evaluated character
   * @param disallowedStrings - the strings must not be found
   */
  final case class DisallowedSubstringRule(state: TrailingChar, disallowedStrings: List[String]) extends Rule {
    override def verdict: Verdict = if (state == Passed) Naughty else Nice

    override def evaluate(char: Char): DisallowedSubstringRule = state match {
      case Passed => this
      case Previous(previous) if isDisallowedSubstring(previous, char) => copy(Passed)
      case _ => copy(Previous(char))
    }

    private def isDisallowedSubstring(previous: Char, current: Char): Boolean =
      disallowedStrings.contains(previous.toString + current.toString)
  }

  object DisallowedSubstringRule {
    private val DisallowedSubstrings = List("ab", "cd", "pq", "xy")

    def empty: DisallowedSubstringRule = DisallowedSubstringRule(Empty, DisallowedSubstrings)
  }

  /**
   * Applies a list of [[rules]] on each evaluate step
   * The resulting [[verdict]] is the combination of the verdicts of all rules
   * @param rules - the rules to be applied on each [[evaluate]] step
   */
  final class AggregateRule(rules: List[Rule]) extends Rule {
    override def verdict: Verdict =
      rules.foldRight(Nice: Verdict) { case (rule, verdict) => rule.verdict.combine(verdict) }

    override def evaluate(char: Char): AggregateRule = new AggregateRule(rules.map(_.evaluate(char)))
  }

  object AggregateRule {
    def empty: AggregateRule = new AggregateRule(
      List(VowelsRule.empty, ConsecutiveRule.empty, DisallowedSubstringRule.empty)
    )
  }
}

sealed trait TrailingChar

object TrailingChar {
  final case class Previous(last: Char) extends TrailingChar
  case object Empty extends TrailingChar
  case object Passed extends TrailingChar
}
