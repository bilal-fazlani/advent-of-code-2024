package aoc
package day4

object Part1 extends Challenge(day(4)):
  val width = input.head.length
  val height = input.length
  case class Position(x: Int, y: Int):
    def isInGrid: Boolean = x.between(0, width - 1) && y.between(0, height - 1)
  case class Combination(seq: IndexedSeq[Position]):
    override def toString(): String = seq.map(p => input(p.y)(p.x)).mkString
    def isValid: Boolean = seq.forall(_.isInGrid)

  def execute: Long =
    (for
      x <- 0 until width
      y <- 0 until height
      n <- neighbours(Position(x, y), 4)
      if n.toString == "XMAS"
    yield n).length

  def neighbours(p: Position, length: Int): Seq[Combination] =
    enum Dir:
      case Right, Left, Top, Bottom, None
    def select(y: Dir, x: Dir): Combination =
      Combination((0 until length).map { i =>
        val xVal = x match
          case Dir.Right => 1 * i
          case Dir.Left  => -1 * i
          case _         => 0
        val yVal = y match
          case Dir.Top    => -1 * i
          case Dir.Bottom => 1 * i
          case _          => 0
        Position(p.x + xVal, p.y + yVal)
      })
    import Dir.*
    Seq(
      select(Top, None),
      select(Top, Right),
      select(None, Right),
      select(Bottom, Right),
      select(Bottom, None),
      select(Bottom, Left),
      select(None, Left),
      select(Top, Left)
    ).filter(_.isValid)
