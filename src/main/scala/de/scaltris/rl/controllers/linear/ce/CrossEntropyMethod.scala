package de.scaltris.rl.controllers.linear.ce

import de.scaltris.rl.controllers.linear.LinearController
import de.scaltris.rl.{FlatActionMDP, InfTetris}
import org.apache.commons.math3.stat.descriptive.moment.{Variance, Mean}
import de.scaltris.util._
import scala.util.Random

/**
 * Created by thomas on 26.03.15.
 */
case class CrossEntropyMethod(mdp: InfTetris,
                              numSamples: Int = 30,
                              nBest: Int = 10,
                              sampleLength: Int = 1000)(val features: Seq[InfTetris#Feature]) {
  class Distribution(val meanSD: IndexedSeq[(Double,Double)], val r: Random) {
    val samples: IndexedSeq[(IndexedSeq[Double], Double)] = (1 to numSamples).toIndexedSeq.par.map{_ =>
      val params = drawSample()
      params -> evaluateParameters(params)
    }.seq.toIndexedSeq

    val meanValue: Double = mean(samples.map(_._2))
    val sdValue: Double = sd(samples.map(_._2))

    val eliteSamples: IndexedSeq[(IndexedSeq[Double], Double)] = samples.sortBy(_._2).reverse.take(nBest)
    val meanEliteValue: Double = mean(eliteSamples.map(_._2))
    val sdEliteValue: Double = sd(eliteSamples.map(_._2))

    val (bestParam, bestValue)= eliteSamples.head

    val newParams: IndexedSeq[(Double, Double)] = {
      val transposed: IndexedSeq[Array[Double]] = eliteSamples.map(_._1).transpose.map(_.toArray)
      val nmeans = transposed.map(mean(_))
      val nsds = transposed.map(sd(_))
      nmeans zip nsds
    }

    def next(rand: Random) = new Distribution(newParams, rand)

    private def drawSample(): IndexedSeq[Double] = meanSD.map{case (m,sd) => r.nextGaussian() * sd + m}
    private def evaluateParameters(p: IndexedSeq[Double]): Double =
      FlatActionMDP.rollout(new LinearController(mdp)(features zip p),r).take(sampleLength).foldLeft(0d)(_ + _._2) / sampleLength

    override def toString: String = {
      f"CE-Dist mean/ds: $meanValue%.2f/$sdValue%.2f parameters: ${meanSD.map{case (m,sd) => f"$m%.1f/$sd%.1f"}.mkString(" ")}"
    }

    def controller = new LinearController(mdp)(features zip meanSD.map(_._1))
  }

  def init(meansd: IndexedSeq[(Double,Double)], r: Random): Distribution =
    new Distribution(meansd, r).ensuring(meansd.length == features.length)
}
