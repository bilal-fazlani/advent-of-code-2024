package aoc

import aoc.day2.*

class Day2Test extends munit.FunSuite {
  test("part 1 : safety report") {
    assert(Part1.execute == 2)
  }

  test("part 2 : safety report with dampening effect") {
    assert(Part2.execute == 4)
  }
}
