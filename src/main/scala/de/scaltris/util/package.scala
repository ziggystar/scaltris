package de.scaltris

import org.apache.commons.math3.stat.descriptive.moment.{Variance, Mean}

/**
 * Created by thomas on 26.03.15.
 */
package object util {
  val myMean = new Mean()
  val myVariance = new Variance()
  def mean(xs: Iterable[Double]): Double = myMean.evaluate(xs.toArray)
  def variance(xs: Iterable[Double]): Double = myVariance.evaluate(xs.toArray)
  def sd(xs: Iterable[Double]): Double = math.sqrt(variance(xs))

}
