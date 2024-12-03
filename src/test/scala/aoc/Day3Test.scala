package aoc

import aoc.day3.*

class Day3Test extends munit.FunSuite {
  test("part 1 : parse and multiply") {
    assertEquals(Part1.execute, 161L)
  }
  test("part 2 : do and dont") {
    assertEquals(Part2.execute, 48L)
  }
}
