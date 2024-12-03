package aoc
package day3

import Part2.Expression.*
import zio.parser.*

object Part2 extends Challenge(day(3).part(2)):
  def execute: Long = input |> parse |> compile |> run

  enum Token:
    case Multiply(left: Long, right: Long)
    case Do
    case Dont
    case Unknown

  def parse(input: Seq[String]): Seq[Token] =
    val doParser = Parser.string("do()", Token.Do)
    val dontParser = Parser.string("don't()", Token.Dont)
    val mulParser = {
      val number = Parser.digit.repeat0.map(x => x.mkString.toLong)
      (Parser.string("mul(", ()) ~ number ~ Parser.string(",", ()) ~ number ~ Parser.string(")", ())).to[Token.Multiply]
    }
    val parser = doParser.orElse(dontParser).orElse(mulParser).orElse(Parser.any.as(Token.Unknown)).*
    input.flatMap(parser.parseString(_).getOrElse(throw Exception("Failed to parse")))

  def compile(tokens: Seq[Token]): Program =
    enum CompilingState:
      case Collecting(dos: Seq[Do], muls: Seq[Multiply])
      case Stopped(dos: Seq[Do])

    import CompilingState.*

    tokens.foldLeft[CompilingState](Collecting(Seq.empty, Seq.empty)) {
      case (x, Token.Unknown)                       => x
      case (Collecting(d, muls), m: Token.Multiply) => Collecting(d, muls :+ Multiply(m.left, m.right))
      case (c: Collecting, Token.Do)                => c
      case (c: Collecting, Token.Dont)              => Stopped(c.dos :+ Do(c.muls))
      case (s: Stopped, Token.Do)                   => Collecting(s.dos, Seq.empty)
      case (s: Stopped, _)                          => s
    } match
      case Collecting(dos, muls) if muls.nonEmpty => Program(dos :+ Do(muls))
      case Collecting(dos, _)                     => Program(dos)
      case Stopped(dos)                           => Program(dos)

  def run(expression: Expression): Long = expression match
    case Program(dos)          => dos.foldLeft(0L)(_ + run(_))
    case Do(muls)              => muls.foldLeft(0L)(_ + run(_))
    case Multiply(left, right) => left * right

  enum Expression:
    case Program(dos: Seq[Do])
    case Do(multiplications: Seq[Multiply])
    case Multiply(left: Long, right: Long)
