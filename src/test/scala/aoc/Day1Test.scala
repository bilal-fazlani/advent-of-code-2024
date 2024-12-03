package aoc

import aoc.day1.*

class Day1Test extends munit.FunSuite {
  test("part 1 : sum of differences") {
    assert(Part1.execute == 11)
  }

  test("part 2 : similarity") {
    assert(Part2.execute == 31)
  }
}
