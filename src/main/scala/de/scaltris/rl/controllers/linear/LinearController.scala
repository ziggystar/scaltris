package de.scaltris.rl.controllers.linear

import de.scaltris.rl.{FlatActionMDP, InfTetris, Policy}
import org.apache.commons.math3.stat.descriptive.moment.{Variance, Mean}

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

  override def getAction(state: mdp.State, r: Random): mdp.Action = mdp.actions(state).maxBy{action =>
    val (nextState,reward) = mdp.act(state,action,r)
    reward + mdp.discount * computeValue(nextState)
  }


  /** Train by minimizing the squared TD-error. Doesn't work well. */
  def trainTD(steps: Int = 10000, learningRate: Double = 0.1, r: Random = new Random()): LinearController = {
    val playout: IndexedSeq[(mdp.State, Double)] = FlatActionMDP.rollout(this,r).take(steps).toIndexedSeq
    val rewards: Array[Double] = playout.map(_._2)(collection.breakOut)

    def backPropValue(rewards: Array[Double], lastValue: Double = 0): Array[Double] =
      rewards.scanRight(lastValue){case (immediate,future) => immediate + mdp.discount * future}.init

    val errors: Array[Double] =
      playout.map(_._1).zip(backPropValue(rewards)).map{case (s,value) => computeValue(s) - value}(collection.breakOut)
//    val errors: IndexedSeq[(mdp.State, Double)] = playout.sliding(2,1).map{ss =>
//      (ss.head._1,ss.head._2 + mdp.discount * computeValue(ss(1)._1) - computeValue(ss.head._1))
//    }.toIndexedSeq


    println(s"mean: ${new Mean().evaluate(errors)} sd: ${math.sqrt(new Variance().evaluate(errors))}")
    println("mean reward: " + new Mean().evaluate(playout.map(_._2)(collection.breakOut)))


    val corrections = playout.map(_._1).zip(errors).map{case (state,tdError) =>
        features.map{f => tdError * f.compute(state)}(collection.breakOut): Array[Double]
    }.transpose.map(ds => ds.sum / steps)

    val newWeights = weights.zip(corrections).map{case (w,c) =>
      (1-learningRate) * w + learningRate * c
    }
    new LinearController(mdp)(features zip newWeights)
  }
}
