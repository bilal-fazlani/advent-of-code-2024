package aoc

import zio.stream.ZStream
import zio.ZIO
import zio.stream.ZPipeline
import zio.ZIOAppDefault
import java.io.IOException
import scala.io.Source

extension [E, A](e: Either[E, A]) def orDie(f: E => String): A = e.fold(e => throw new Exception(f(e)), identity)

extension (n: Long) def between(a: Long, b: Long): Boolean = n >= a && n <= b

private def read(inputFile: InputFile): ZStream[Any, IOException, String] =
  (inputFile match
    case InputFile.Day(day) =>
      ZStream.fromResource(s"day${day}.txt")

    case InputFile.Part(day, part) =>
      val path = s"day${day.value}/part${part}.txt"
      ZStream
        .whenCaseZIO(resourceFileExists(path)) {
          case true  => ZStream.fromResource(path)
          case false => ZStream.fromResource(s"day${day.value}.txt")
        }
  )
  .via(ZPipeline.utfDecode)
    .via(ZPipeline.splitLines)
    .filter(_.nonEmpty)

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

private def resourceFileExists(path: String) =
  ZIO
    .attemptBlockingIO(
      this.getClass.getClassLoader.getResource(path.replace('\\', '/'))
    )
    .flatMap { url =>
      if (url == null) ZIO.succeed(false)
      else ZIO.succeed(true)
    }

trait ChallengeAsync(val file: InputFile) extends ZIOAppDefault:
  val input: ZStream[Any, IOException, String] = read(file)
  def execute: ZIO[Any, Throwable, Long]
  def run =
    execute.debug
      .flatMap(value => zio.Console.printLine(value.toString))
      .tapError(error => zio.Console.printLineError(error.getMessage))
      .exitCode

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
