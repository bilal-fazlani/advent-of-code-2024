package aoc

import aoc.day3.*

class Day3Test extends munit.FunSuite {
  test("part 1 : parse and multiply") {
    assert(Part1.execute == 161)
  }
  test("part 2 : do and dont"){
    assert(Part2.execute == 48)
  }
}
