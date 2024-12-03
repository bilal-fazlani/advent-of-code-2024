package aoc
package day3

import aoc.day3.Part2.Expression.*

object Part2 extends Challenge(day(3).part(2)):
  def execute: Long = input
    |> Compiler.parse
    |> Compiler.compile
    |> (_.evaluate)

  object Compiler {
    sealed trait Token
    object Token {
      case class Multiply(left: Long, right: Long) extends Token
      case object Do extends Token
      case object Dont extends Token
    }

    def parse(input: Seq[String]): Seq[Token] =
      val reg = """mul\((\d+)\,(\d+)\)|(?:don\'t\(\))|(?:do\(\))""".r
      input.flatMap { i =>
        val mulRegex = """mul\((\d+)\,(\d+)\)""".r
        val doRegex = """(?:do\(\))""".r
        val dontRegex = """(?:don\'t\(\))""".r
        reg.findAllMatchIn(i).map {
          case m if mulRegex matches m.toString()  => Token.Multiply(m.group(1).toLong, m.group(2).toLong)
          case m if doRegex matches m.toString()   => Token.Do
          case m if dontRegex matches m.toString() => Token.Dont
        }
      }

    def compile(tokens: Seq[Token]): Expression.Program =
      enum CompilingState:
        case Collecting(prev: Seq[Expression.Do], current: Seq[Expression.Multiply])
        case Stopped(prev: Seq[Expression.Do])

      val state = {
        tokens.foldLeft[CompilingState](CompilingState.Collecting(Seq.empty, Seq.empty)) {
          case (CompilingState.Collecting(prev, current), Token.Multiply(l, r)) =>
            CompilingState.Collecting(prev, current.appended(Expression.Multiply(l, r)))
          case (c: CompilingState.Collecting, Token.Do) => c
          case (c: CompilingState.Collecting, Token.Dont) =>
            CompilingState.Stopped(c.prev.appended(Expression.Do(c.current)))
          case (s: CompilingState.Stopped, Token.Do) => CompilingState.Collecting(s.prev, Seq.empty)
          case (s: CompilingState.Stopped, _)        => s
        }
      }
      state match
        case CompilingState.Collecting(prev, current) if current.nonEmpty =>
          Expression.Program(prev.appended(Expression.Do(current)))
        case CompilingState.Collecting(prev, _) => Expression.Program(prev)
        case CompilingState.Stopped(prev)       => Expression.Program(prev)

  }

  sealed trait Expression {
    def evaluate: Long = this match
      case Program(dos)          => dos.foldLeft(0L)(_ + _.evaluate)
      case Do(multis)            => multis.foldLeft(0L)(_ + _.evaluate)
      case Multiply(left, right) => left * right
  }
  object Expression {
    case class Program(dos: Seq[Do]) extends Expression
    case class Do(multiplications: Seq[Multiply]) extends Expression
    case class Multiply(left: Long, right: Long) extends Expression
  }
