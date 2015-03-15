package de.scaltris.rl

import de.scaltris.game._

import scala.util.Random

/**
 * HL means high-level implementation (slow).
 */
object InfiniteTetrisHL extends FlatActionMDP {
  override type State = this.type
  //an index into `moves`
  override type Action = Int
  //the moves are all rotations/translations of pieces, with their bottom-most y-coordinate equaling 0
  val moves: IndexedSeq[Block[Unit]] = {for {
    x <- 0 to 10
    pos = Position(x,0)
    which <- 0 to 6
    b = Block.stdShape(which, pos, ()) if b.components.map(_.x).max <= 9
    rot <- 0 to 3
  } yield b.rotate(rot)}.toSet.toIndexedSeq

  override def initialState(r: Random): State = ???

  def actions(state: State): IndexedSeq[Action] = (0 until moves.size).toArray

  override def act(state: State, action: Action, r: Random): (State, Double) = ???
}
