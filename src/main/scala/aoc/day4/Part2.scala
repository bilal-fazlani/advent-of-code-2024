package aoc
package day4

object Part2 extends Challenge(day(4).part(2)):
  val width = input.head.length
  val height = input.length
  case class Position(x: Int, y: Int):
    def isInGrid: Boolean = x.between(0, width - 1) && y.between(0, height - 1)
    def value: Char = input(y)(x)
  case class Shape(
      center: Position,
      topLeft: Position,
      topRight: Position,
      bottomLeft: Position,
      bottomRight: Position
  ):
    def isXmas =
      Seq(center, topLeft, topRight, bottomLeft, bottomRight).forall(_.isInGrid) &&
        center.value == 'A' &&
        ((topLeft.value == 'M' && bottomRight.value == 'S') || (topLeft.value == 'S' && bottomRight.value == 'M')) &&
        ((topRight.value == 'M' && bottomLeft.value == 'S') || (topRight.value == 'S' && bottomLeft.value == 'M'))

  def execute: Long =
    (for
      x <- 0 until width
      y <- 0 until height
      s = shape(Position(x, y))
      if s.isXmas
    yield s).length

  def shape(center: Position): Shape =
    enum Dir:
      case Right, Left, Top, Bottom

    def select(y: Dir, x: Dir): Position =
      val xVal = x match
        case Dir.Right => 1
        case Dir.Left  => -1
        case _         => 0
      val yVal = y match
        case Dir.Top    => -1
        case Dir.Bottom => 1
        case _          => 0
      Position(center.x + xVal, center.y + yVal)

    import Dir.*
    Shape(center, select(Top, Left), select(Top, Right), select(Bottom, Left), select(Bottom, Right))
