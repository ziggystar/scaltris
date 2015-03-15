package de.scaltris.rl

import org.specs2.Specification
import org.specs2.specification.Fragments

import scala.util.Random

class InfTetrisTest extends Specification {
  val stdGame = InfTetris()
  override def is: Fragments =
    "empty stack has 0 as topRow" ! (stdGame.Stack.empty.topRow === 0) ^
      "fill lines into empty stack" ! (stdGame.Stack.empty.fillToMin(new Random(0))._2 === stdGame.minHeight) ^
      "initial stack has minHeight height" ! {stdGame.initialState(new Random(0))._1.topRow === stdGame.minHeight}
}
