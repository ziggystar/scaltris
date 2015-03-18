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

object FlatActionMDP {
  def randomPlayout(mdp: FlatActionMDP,r: Random): Iterator[(mdp.State, Double)] =
    Iterator.iterate((mdp.initialState(r),0d)){case (state,rew) =>
      val acts = mdp.actions(state)
      mdp.act(state,acts(r.nextInt(acts.size)),r)
  }
}