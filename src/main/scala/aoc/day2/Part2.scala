package aoc
package day2

import com.bilalfazlani.rainbowcli.*

object Part2 extends Challenge(day(2).part(1)):
  def execute: Long =
    val regex = """\d+""".r
    val reports = input.map { str =>
      val numbers = regex.findAllIn(str).toList.map(_.toInt)
      val report = Report(numbers)
      println(report)
      report
    }
    reports.count(_.isSafe)

  enum Damping:
    case None
    case Damped(index: Int)

  enum State:
    case Init
    case Started(prev: Int, damping: Damping)
    case Increasing(prev: Int, damping: Damping)
    case Decreasing(prev: Int, damping: Damping)
    case Errored(reason: ErrorReason, damping: Damping, index: Int, dir: Option[Direction])

  enum Direction:
    case Increase
    case Decrease

  enum ErrorReason:
    case LargeJump(prev: Int, next: Int)
    case ChangedDirection(prev: Int, next: Int, newDirection: Direction)
    case SameNumber(prev: Int)

  case class Report(numbers: List[Int]) {
    val state = calculate(numbers)

    private def calculate(numbers: List[Int]) = numbers.zipWithIndex.foldLeft[State](State.Init) {
      case (agg, (num, index)) =>
        agg match
          case e: State.Errored => e

          case State.Init => State.Started(num, Damping.None)

          case State.Started(prev, Damping.None) =>
            if num == prev then State.Started(prev, Damping.Damped(index))
            else if num > prev then State.Increasing(num, Damping.None)
            else State.Decreasing(num, Damping.None)

          case State.Started(prev, d @ Damping.Damped(_)) =>
            if num == prev then State.Errored(ErrorReason.SameNumber(prev), d, index, None)
            else if num > prev then State.Increasing(num, d)
            else State.Decreasing(num, d)

          case State.Increasing(prev, d @ Damping.Damped(_)) =>
            if num == prev then State.Errored(ErrorReason.SameNumber(prev), d, index, Some(Direction.Increase))
            else if num < prev then
              State.Errored(
                ErrorReason.ChangedDirection(prev, num, Direction.Decrease),
                d,
                index,
                Some(Direction.Increase)
              )
            else if Math.abs(num - prev) > 3 then
              State.Errored(ErrorReason.LargeJump(prev, num), d, index, Some(Direction.Increase))
            else State.Increasing(num, d)

          case State.Increasing(prev, Damping.None) =>
            if num == prev then State.Increasing(prev, Damping.Damped(index))
            else if Math.abs(num - prev) > 3 then State.Increasing(prev, Damping.Damped(index))
            else if num < prev then State.Increasing(prev, Damping.Damped(index))
            else State.Increasing(num, Damping.None)

          case State.Decreasing(prev, d @ Damping.Damped(_)) =>
            if num == prev then State.Errored(ErrorReason.SameNumber(prev), d, index, Some(Direction.Decrease))
            else if num > prev then
              State.Errored(
                ErrorReason.ChangedDirection(prev, num, Direction.Increase),
                d,
                index,
                Some(Direction.Decrease)
              )
            else if Math.abs(num - prev) > 3 then
              State.Errored(ErrorReason.LargeJump(prev, num), d, index, Some(Direction.Decrease))
            else State.Decreasing(num, d)

          case State.Decreasing(prev, Damping.None) =>
            if num == prev then State.Decreasing(prev, Damping.Damped(index))
            else if num > prev then State.Decreasing(prev, Damping.Damped(index))
            else if Math.abs(num - prev) > 3 then State.Decreasing(prev, Damping.Damped(index))
            else State.Decreasing(num, Damping.None)
    }

    def bruteForced = Range(0, numbers.length)
      .map(i => numbers.zipWithIndex.filter(_._2 != i).map(_._1))
      .map(calculate(_))

    val isSafe: Boolean = state match
      case State.Increasing(_, _) | State.Decreasing(_, _) => true
      case _                                               => false

    override def toString(): String =
      given ColorContext = ColorContext(true)
      val numbersString = s": ${numbers.zipWithIndex
          .map { (n, i) =>
            val dampIndex = state match
              case State.Started(_, Damping.Damped(i))       => i
              case State.Increasing(_, Damping.Damped(i))    => i
              case State.Decreasing(_, Damping.Damped(i))    => i
              case State.Errored(_, Damping.Damped(i), _, _) => i
              case _                                         => -1
            val erroredIndex = state match
              case State.Errored(_, _, i, _) => i
              case _                         => -1
            if i == dampIndex then s"${n.toString.yellow}"
            else if i == erroredIndex then s"${n.toString.red}"
            else n.toString
          }
          .mkString(", ")}"
      state match
        case State.Init          => s"❗️ Init: $numbersString"
        case s: State.Started    => s"❗️ Started $numbersString"
        case s: State.Increasing => s"✅ ⬆️".green + s" $numbersString"
        case s: State.Decreasing => s"✅ ⬇️".green + s" $numbersString"
        case State.Errored(r, _, _, d) =>
          val dir = d match
            case Some(Direction.Increase) => "⬆️".red
            case Some(Direction.Decrease) => "⬇️".red
            case None                     => " "
          s"❌ $dir $numbersString ${r.toString().cyan}"
  }
