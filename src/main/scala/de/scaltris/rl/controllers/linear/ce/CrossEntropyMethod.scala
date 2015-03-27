package de.scaltris.rl.controllers.linear.ce

import de.scaltris.rl.controllers.linear.LinearController
import de.scaltris.rl.{FlatActionMDP, InfTetris}
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.linear.{DiagonalMatrix, RealMatrix}
import org.apache.commons.math3.stat.correlation.Covariance
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
  trait Distribution {
    type Param
    type Weights = IndexedSeq[Double]

    val samples: IndexedSeq[(Weights, Double)] = (1 to numSamples).toIndexedSeq.par.map{_ =>
      val params = sample()
      params -> evaluate(params)
    }.seq.toIndexedSeq

    val eliteSamples: IndexedSeq[(Weights, Double)] = samples.sortBy(_._2).reverse.take(nBest)

    def rand: Random
    def parameters: Param

    def sample(): IndexedSeq[Double]
    def fit(samples: IndexedSeq[Weights]): Param
    def paramMode(param: Param): IndexedSeq[Double]
    def construct(p: Param, r: Random): Distribution

    def next(r: Random): Distribution = construct(fit(eliteSamples.map(_._1)),r)
    def evaluate(weights: IndexedSeq[Double]) =
      FlatActionMDP.rollout(new LinearController(mdp)(features zip weights),rand)
        .take(sampleLength)
        .foldLeft(0d)(_ + _._2) / sampleLength
    def controller = new LinearController(mdp)(features zip paramMode(parameters))
    override def toString: String = {
      import de.scaltris.util._
      f"CE-Dist mean/ds: ${mean(samples.map(_._2))}%.2f/${sd(samples.map(_._2))}%.2f parameters: ${parameters}"
    }
  }

  class IndependentNormal(val parameters: IndexedSeq[(Double,Double)], val rand: Random) extends Distribution {
    type Param = IndexedSeq[(Double,Double)]

    def sample(): IndexedSeq[Double] = parameters.map{case (m,sd) => rand.nextGaussian() * sd + m}
    def fit(samples: IndexedSeq[Weights]): Param = {
      val transposed: IndexedSeq[Array[Double]] = samples.transpose.map(_.toArray)
      val nmeans = transposed.map(mean(_))
      val nsds = transposed.map(sd(_))
      nmeans zip nsds
    }
    def paramMode(param: Param): IndexedSeq[Double] = param.map(_._1)
    def construct(p: Param, r: Random): Distribution = new IndependentNormal(p,r)
  }

  class MVNormal(val parameters: MultivariateNormalDistribution, val rand: Random) extends Distribution {
    override type Param = MultivariateNormalDistribution

    override def fit(samples: IndexedSeq[Weights]): Param = {
      val t: Array[Array[Double]] = samples.transpose.map(_.toArray)(collection.breakOut)
      val cov = new Covariance(t.transpose)
      new MultivariateNormalDistribution(t.map(mean(_)), cov.getCovarianceMatrix.getData)
    }

    override def paramMode(param: Param): IndexedSeq[Double] = param.getMeans

    override def sample(): IndexedSeq[Double] = parameters.sample()

    override def construct(p: Param, r: Random): Distribution = new MVNormal(p,r)
  }
  object MVNormal {
    def apply(meanSD: IndexedSeq[(Double,Double)], r: Random) = {
      val dia = new DiagonalMatrix(meanSD.map(_._2)(collection.breakOut): Array[Double])
      val mvn = new MultivariateNormalDistribution(meanSD.map(_._1).toArray,dia.getData)
      new MVNormal(mvn, r)
    }
  }
}
