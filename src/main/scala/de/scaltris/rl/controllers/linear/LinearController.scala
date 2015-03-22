package de.scaltris.rl.controllers.linear

import de.scaltris.rl.{InfTetris, Policy}

import scala.util.Random

class LinearController(val mdp: InfTetris)(_weightedFeatures: Seq[(InfTetris#Feature,Double)]) extends Policy {
  val features: IndexedSeq[mdp.Feature] =
    _weightedFeatures.map(_._1.asInstanceOf[mdp.Feature])(collection.breakOut)

  val weights: IndexedSeq[Double] = _weightedFeatures.map(_._2)(collection.breakOut)

  def computeValue(state: mdp.State): Double = {
    var r = 0d
    var fi = 0
    while(fi < features.size){
      r += features(fi).compute(state) * weights(fi)
      fi += 1
    }
    r
  }

  override def getAction(state: mdp.State, r: Random): mdp.Action = {
    val (stack, tetromino) = state
    val actions = mdp.actions(state)
    actions.maxBy{action =>
      val (nextState@(nextStack,_),reward) = mdp.act(state,action,r)
      reward + mdp.discount * computeValue(nextState)
    }
  }
}

object LinearController {

}
