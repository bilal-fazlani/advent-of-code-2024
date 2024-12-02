package aoc

import aoc.day2.*
import zio.test.*
import zio.test.Assertion.*

object Day2Test extends ZIOSpecDefault {
  val spec = suite("Day 1 Tests")(
    test("part 1 : safety report") {
      assertTrue(Part1.execute == 2)
    },
    test("part 2 : safety report with dampening effect") {
      assertTrue(Part2.execute == 4)
    }
  )
}
