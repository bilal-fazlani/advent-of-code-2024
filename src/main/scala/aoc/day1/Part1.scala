package aoc
package day1

object Part1 extends Challenge(day(1).part(1)):
  def execute: Long =
    val regex = """(\d+)\s+(\d+)""".r
    val parsed = input.map { str =>
      val regex(a, b) = str
      (a.toInt, b.toInt)
    }
    val (colA, colB) = parsed.foldLeft[(List[Int], List[Int])]((List.empty, List.empty)) { (acc, cur) =>
      ((acc._1 :+ cur._1).sorted, (acc._2 :+ cur._2).sorted)
    }
    (colA zip colB).map((a, b) => Math.abs(a - b)).sum
