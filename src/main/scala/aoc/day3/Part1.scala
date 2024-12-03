package aoc
package day3

import scala.util.matching.Regex.Match

object Part1 extends Challenge(day(3).part(1)):
  def execute: Long =
    val regex = """mul\((\d+)\,(\d+)\)""".r
    val numbers = input.flatMap(i =>
      val multiplications = regex.findAllMatchIn(i).map(Multiply(_)).toList
      multiplications
    )
    numbers.map(_.evaluate).sum

  case class Multiply(left: Long, right: Long) {
    def evaluate = left * right
  }
  object Multiply {
    def apply(m: Match): Multiply =
      Multiply(m.group(1).toLong, m.group(2).toLong)
  }
