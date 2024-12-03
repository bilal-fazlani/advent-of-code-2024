package aoc

import java.io.IOException
import scala.io.Source

extension (n: Long) def between(a: Long, b: Long): Boolean = n >= a && n <= b

def readSync(inputFile: InputFile): Iterator[String] =
  def iteratorOf(name: String) = Source.fromResource(name).getLines().filter(_.nonEmpty)

  inputFile match
    case InputFile.Day(day) =>
      iteratorOf(s"day${day}.txt")
    case InputFile.Part(InputFile.Day(day), part) =>
      val path = s"day${day}/part${part}.txt"
      val resource = this.getClass.getClassLoader.getResource(path.replace('\\', '/'))
      if resource != null
      then iteratorOf(path)
      else iteratorOf(s"day${day}.txt")

trait Challenge(val file: InputFile):
  def execute: Long
  val input: List[String] = readSync(file).toList
  def main(args: Array[String]) =
    val output = execute
    println(output)

enum InputFile:
  case Day(value: Int)
  case Part(day: Day, value: Int)

def day(number: Int): InputFile.Day = InputFile.Day(number)

extension (day: InputFile.Day) def part(number: Int): InputFile.Part = InputFile.Part(day, number)
