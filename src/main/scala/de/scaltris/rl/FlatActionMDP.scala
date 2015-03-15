package de.scaltris.rl

import de.scaltris.game.{Position, Block}

import scala.util.Random

/**
 * @author Thomas Geier
 * @since 11/20/13
 */

trait FlatActionMDP {
  type State
  type Action

  def initialState(r: Random): State
  def act(state: State, action: Action, r: Random): (State,Double)
  def actions(state: State): IndexedSeq[Action]
}