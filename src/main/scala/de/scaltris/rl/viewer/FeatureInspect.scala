package de.scaltris.rl.viewer

import de.scaltris.rl.{FlatActionMDP, InfTetris}
import de.scaltris.rl.controllers.linear.LinearController

import scala.util.Random

/**
 * Created by thomas on 27.03.15.
 */
object FeatureInspect {
  val tetris = InfTetris(minHeight = 0)
  val policy = new LinearController(tetris)(
    Seq(
      tetris.PotentialEnergy -> -4,
      tetris.VTransitions -> -10,
      tetris.MaxHeight -> 1,
      tetris.BlockCount -> -3,
      tetris.DistinctHeights -> 3,
      tetris.Hops -> -2
    )
  )

  def sampleStack(r: Random): (tetris.Stack, tetris.Tetromino) =
    FlatActionMDP.rollout(policy, r).drop(40).next()._1.asInstanceOf[tetris.State]

  def main(args: Array[String]) {
    val numStacks = 5

    val r = new Random(0)
    (1 to numStacks).foreach{_ =>
      val state@(stack,_) = sampleStack(r)
      println("Stack:")
      println(stack.toString)
      println(tetris.allFeatures.map(f => s"\t${f} -> ${f.compute(state)}").mkString("\n"))
    }

  }
}
