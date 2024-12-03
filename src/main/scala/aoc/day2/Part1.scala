package aoc
package day2

object Part1 extends Challenge(day(2)):
  def execute: Long =
    val regex = """\d+""".r
    input.count { str =>
      val numbers = regex.findAllIn(str).toList.map(_.toInt)
      safe(getRow(numbers))
    }

  enum State:
    case Increasing, Decreasing, Errored

  case class Pair(prev: Int, current: Int) {
    val transition = (prev, current) match
      case (a, b) if Math.abs(a - b) > 3 => State.Errored
      case (a, b) if a == b              => State.Errored
      case (a, b) if a < b               => State.Increasing
      case (a, b) if a > b               => State.Decreasing
  }

  def getRow(numbers: List[Int]): List[Pair] =
    numbers.sliding(2).toList.map(x => Pair(x.head, x.last))

  def safe(pairs: List[Pair]): Boolean =
    val transition: State = pairs
      .foldLeft[Option[State]](None) { (transitionMaybe, pair) =>
        transitionMaybe match
          case None => Some(pair.transition)
          case Some(prev) =>
            (prev, pair.transition) match
              case (a, b) if a == b && a != State.Errored => Some(pair.transition)
              case _                                      => Some(State.Errored)
      }
      .getOrElse(throw Exception("No transition found"))
    !transition.isInstanceOf[State.Errored.type]
