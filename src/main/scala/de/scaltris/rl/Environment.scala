package de.scaltris.rl

import scala.util.Random

/**
 * @author Thomas Geier
 * @since 11/20/13
 */

trait MDP{
  type State
  type Action

  def initialState(seed: Long): State
  def act(state: State, action: Action, seed: Long): (State,Double)
}

case class Observation(ints: Array[Int], floats: Array[Double])
case class ObservationSpec(ints: Seq[(Int,Int)], floats: Seq[(Double,Double)])
case class Action(ints: Array[Int], floats: Array[Double])
case class ActionSpec(ints: Seq[(Int,Int)], floats: Seq[(Double,Double)])