package aoc
package day3

import Part2.Expression.*

object Part2 extends Challenge(day(3).part(2)):
  def execute: Long = input |> parse |> compile |> run

  enum Token:
    case Multiply(left: Long, right: Long)
    case Do
    case Dont

  def parse(input: Seq[String]): Seq[Token] =
    val mulRegex = """mul\((\d+)\,(\d+)\)""".r
    val doRegex = """(?:do\(\))""".r
    val dontRegex = """(?:don\'t\(\))""".r
    val reg = s"""$mulRegex|$doRegex|$dontRegex""".r

    input.flatMap {
      reg.findAllMatchIn(_).map {
        case m if mulRegex matches m.matched  => Token.Multiply(m.group(1).toLong, m.group(2).toLong)
        case m if doRegex matches m.matched   => Token.Do
        case m if dontRegex matches m.matched => Token.Dont
      }
    }

  def compile(tokens: Seq[Token]): Program =
    enum CompilingState:
      case Collecting(dos: Seq[Do], muls: Seq[Multiply])
      case Stopped(dos: Seq[Do])

    import CompilingState.*

    tokens.foldLeft[CompilingState](Collecting(Seq.empty, Seq.empty)) {
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
