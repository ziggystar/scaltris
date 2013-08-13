package de.scaltris.rl

/**
 * @author Thomas Geier
 * @since 4/29/13
 */

trait System {
  type Action

  def act(a: Action): System
  def availableActions: IndexedSeq[Action]
}

case class InfiniteTetris(stack: EndlessStack, seed: Long) extends System{
  type Action = DropAction

  def act(a: InfiniteTetris#Action): System = ???

  def availableActions: IndexedSeq[InfiniteTetris#Action] = ???
}

class DropAction(val data: Int) extends AnyVal {
  //return four rows, first row is highest row
  def rows: IndexedSeq[Row] = ???
  def pieceType: Int = (data & 0xFF) % 7
  def rotation: Int = ((data >> 8) & 0xFF) % 4
  def xpos: Int = ((data >> 16) & 0xFF) % 10
}

class Row(val data: Integer) extends AnyVal
case class EndlessStack(stack: Seq[Row],topHeight: Int, seed: Long)
