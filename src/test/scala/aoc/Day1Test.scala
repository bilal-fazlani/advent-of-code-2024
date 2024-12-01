package aoc

import aoc.day1.*
import zio.test.*
import zio.test.Assertion.*

object Day1Test extends ZIOSpecDefault {
  val spec = suite("Day 1 Tests")(
    test("part 1 : sum of differences") {
      assertTrue(Part1.execute == 11)
    },
    test("part 2 : similarity") {
      assertTrue(Part2.execute == 31)
    }
  )
}
