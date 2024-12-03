package aoc

import aoc.day2.*

class Day2Test extends munit.FunSuite {
  test("part 1 : safety report") {
    assertEquals(Part1.execute, 2L)
  }

  test("part 2 : safety report with dampening effect") {
    assertEquals(Part2.execute, 4L)
  }
}
