package aoc

import aoc.day1.*

class Day1Test extends munit.FunSuite {
  test("part 1 : sum of differences") {
    assertEquals(Part1.execute, 11L)
  }

  test("part 2 : similarity") {
    assertEquals(Part2.execute, 31L)
  }
}
