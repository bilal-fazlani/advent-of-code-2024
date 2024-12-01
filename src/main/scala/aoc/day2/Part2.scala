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

  enum Dampening:
    case None
    case Damped(index: Int)

    def isEquals(other: Int): Boolean = (this, other) match
      case (Damped(i1), _) if i1 == other => true
      case _                              => false

  enum State(val isSafe: Boolean, val dampening: Dampening):
    case Init(override val dampening: Dampening) extends State(false, dampening)
    case Started(prev: Int, override val dampening: Dampening) extends State(false, dampening)
    case Increasing(prev: Int, override val dampening: Dampening) extends State(true, dampening)
    case Decreasing(prev: Int, override val dampening: Dampening) extends State(true, dampening)
    case Errored(reason: ErrorReason, index: Int, dir: Option[Direction], override val dampening: Dampening)
        extends State(false, dampening)

  enum Direction:
    case Increase
    case Decrease

  enum ErrorReason:
    case LargeJump(prev: Int, next: Int)
    case ChangedDirection(prev: Int, next: Int, newDirection: Direction)
    case SameNumber(prev: Int)

  case class Report(numbers: List[Int]) {

    private def calculate(target: Dampening): State = numbers.zipWithIndex.foldLeft[State](State.Init(target)) {
      case (agg, (cur, index)) if !(target isEquals index) =>
        agg match
          case e: State.Errored => e
          case State.Init(_)    => State.Started(cur, target)
          case State.Started(prev, _) =>
            if cur == prev then State.Errored(ErrorReason.SameNumber(prev), index, None, target)
            else if cur - prev > 3 then
              State.Errored(ErrorReason.LargeJump(prev, cur), index, Some(Direction.Increase), target)
            else if prev - cur > 3 then
              State.Errored(ErrorReason.LargeJump(prev, cur), index, Some(Direction.Decrease), target)  
            else if cur > prev then State.Increasing(cur, target)
            else State.Decreasing(cur, target)
          case State.Increasing(prev, _) =>
            if cur == prev then State.Errored(ErrorReason.SameNumber(prev), index, Some(Direction.Increase), target)
            else if cur < prev then
              State.Errored(
                ErrorReason.ChangedDirection(prev, cur, Direction.Decrease),
                index,
                Some(Direction.Increase),
                target
              )
            else if Math.abs(cur - prev) > 3 then
              State.Errored(ErrorReason.LargeJump(prev, cur), index, Some(Direction.Increase), target)
            else State.Increasing(cur, target)
          case State.Decreasing(prev, _) =>
            if cur == prev then State.Errored(ErrorReason.SameNumber(prev), index, Some(Direction.Decrease), target)
            else if cur > prev then
              State.Errored(
                ErrorReason.ChangedDirection(prev, cur, Direction.Increase),
                index,
                Some(Direction.Decrease),
                target
              )
            else if Math.abs(cur - prev) > 3 then
              State.Errored(ErrorReason.LargeJump(prev, cur), index, Some(Direction.Decrease), target)
            else State.Decreasing(cur, target)
      case (agg, (cur, index)) => agg
    }

    val state = {
      val initial = calculate(Dampening.None)
      val dampened = LazyList(numbers.indices*).map(i => calculate(Dampening.Damped(i)))
      dampened.prepended(initial).find(_.isSafe).getOrElse(initial)
    }

    val isSafe: Boolean = state.isSafe

    override def toString(): String =
      given ColorContext = ColorContext(true)
      val numbersString = s": ${numbers.zipWithIndex
          .map { (n, i) =>
            val dampIndex = state.dampening match
              case Dampening.Damped(d) => d
              case _                   => -1
            val erroredIndex = state match
              case State.Errored(_, i, _, _) => i
              case _                         => -1
            if i == erroredIndex then s"${n.toString.red}"
            else if i == dampIndex then s"${n.toString.yellow}"
            else n.toString
          }
          .mkString(", ")}"
      state match
        case s: State.Init       => s"❗️ Init $numbersString"
        case s: State.Started    => s"❗️ Started $numbersString"
        case s: State.Increasing => s"✅ ⬆️".green + s" $numbersString"
        case s: State.Decreasing => s"✅ ⬇️".green + s" $numbersString"
        case State.Errored(r, _, d, _) =>
          val dir = d match
            case Some(Direction.Increase) => "⬆️".red
            case Some(Direction.Decrease) => "⬇️".red
            case None                     => " ".red
          s"❌ $dir $numbersString ${r.toString().cyan}"
  }
