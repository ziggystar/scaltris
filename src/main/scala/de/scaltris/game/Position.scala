package de.scaltris.game

/**
 * Representing coordinates on the stack.
 * The bottom left cell is 0,0. Rotation centers lie in the center of cell. When they should be on intersections,
 * the lower left corner of the specified cell is considered the rotation center.
 */

case class Position(x: Int, y: Int) {
  def translate(p: Position) = Position(x + p.x, y + p.y)

  def +(p: Position) = this.translate(p)

  def -(p: Position) = this.translate(-p)

  def unary_- = Position(-x, -y)

  def rotateAround(c: Position, angle: Int, aroundIntersection: Boolean): Position = {
    val normalizedPos = this - c

    val rotated = if (!aroundIntersection)
      (((angle % 4) + 4) % 4) match {
        case 0 => normalizedPos
        case 1 => Position(normalizedPos.y, -normalizedPos.x)
        case 2 => Position(-normalizedPos.x, -normalizedPos.y)
        case 3 => Position(-normalizedPos.y, normalizedPos.x)
      }
    else{
      val nn = Position(1 + 2 * normalizedPos.x, 1 + 2 * normalizedPos.y )
      val r = nn.rotateAround(Position(0,0), angle, false)
      Position((r.x - 1) / 2, (r.y - 1) / 2)
    }

    rotated + c
  }
}