package de.scaltris.rl.viewer

import de.scaltris.rl.{FlatActionMDP, InfTetris}
import de.scaltris.rl.controllers.linear.LinearController

import scala.util.Random

/**
 * Created by thomas on 27.03.15.
 */
object FeatureInspect {
  val tetris = InfTetris(minHeight = 0)
  val policy = LinearController.fixed1(tetris)

  def sampleStack(r: Random): (tetris.Stack, tetris.Tetromino) =
    FlatActionMDP.rollout(policy, r).drop(r.nextInt(10)).next()._1.asInstanceOf[tetris.State]

  def main(args: Array[String]) {
    val numStacks = 5

    val r = new Random(0)
    (1 to numStacks).foreach{_ =>
      val state@(stack,_) = sampleStack(r)
      println("Stack:")
      println(stack.toString)
      println(tetris.allFeatures.map(f => s"\t$f -> ${f.compute(state)}").mkString("\n"))
    }

  }
}
