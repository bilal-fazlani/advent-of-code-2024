package aoc
package day2

object Part1 extends Challenge(day(2)):
  def execute: Long =
    val regex = """\d+""".r
    val reports = input.map { str =>
      val numbers = regex.findAllIn(str).toList.map(_.toInt)
      Report(numbers)
    }
    reports.count(_.isSafe)

  enum Agg:
    case Init
    case Started(prev: Int)
    case Safe(prev: Int, diff: Diff)
    case Unsafe

  enum Diff:
    case Increase
    case Decrease

  case class Report(numbers: List[Int]) {
    val isSafe: Boolean = {
      val aggregated = numbers.foldLeft[Agg](Agg.Init) { (agg, num) =>
        agg match
          case Agg.Unsafe => Agg.Unsafe
          case Agg.Init   => Agg.Started(num)

          case Agg.Started(prev) if (num - prev).between(1, 3) => Agg.Safe(num, Diff.Increase)
          case Agg.Started(prev) if (prev - num).between(1, 3) => Agg.Safe(num, Diff.Decrease)
          case Agg.Started(_)                                  => Agg.Unsafe

          case Agg.Safe(prev, Diff.Increase) if (num - prev).between(1, 3) => Agg.Safe(num, Diff.Increase)
          case Agg.Safe(prev, Diff.Decrease) if (prev - num).between(1, 3) => Agg.Safe(num, Diff.Decrease)
          case Agg.Safe(_, _)                                              => Agg.Unsafe
      }
      aggregated match
        case Agg.Safe(_, _) => true
        case _              => false
    }
  }
