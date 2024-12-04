package aoc
package day4

object Part2 extends Challenge(day(4).part(2)):
  val width = input.head.length
  val height = input.length

  def execute: Long =
    (for
      x <- 0 until width
      y <- 0 until height
      if Shape(Position(x, y)).isXmas
    yield ()).length

  case class Position(x: Int, y: Int):
    def isInGrid: Boolean = x.between(0, width - 1) && y.between(0, height - 1)
    def value: Char = input(y)(x)
  case class Line(seq: Seq[Position]):
    override def toString(): String = seq.map(_.value).mkString
  case class Shape(cells: Seq[Position]):
    def isXmas =
      val isValid = cells.forall(_.isInGrid)
      val line1 = Line(Seq(cells(1), cells(0), cells(4)))
      val line2 = Line(Seq(cells(2), cells(0), cells(3)))
      isValid && (line1.toString == "MAS" || line1.toString == "SAM") && (line2.toString == "MAS" || line2.toString == "SAM")

  object Shape:
    def apply(p: Position): Shape =
      def move(x: Int, y: Int): Position = Position(p.x + x, p.y + y)
      Shape(Seq(p, move(-1, -1), move(-1, 1), move(1, -1), move(1, 1)))
