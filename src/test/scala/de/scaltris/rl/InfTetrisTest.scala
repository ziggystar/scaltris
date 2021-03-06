package de.scaltris.rl

import org.specs2.Specification
import org.specs2.specification.Fragments

import scala.util.Random

class InfTetrisTest extends Specification {
  val stdGame = InfTetris()
  val narrow = InfTetris(width = 5, minHeight = 0, maxHeight = 6)
  implicit class RichtString(s: String) {
    def narrowStack: narrow.Stack = narrow.Stack.fromString(s.stripMargin)
  }
  override def is: Fragments =
    "empty stack has 0 as topRow" ! (stdGame.Stack.empty.topRow === 0) ^
      "fill lines into empty stack" ! (stdGame.Stack.empty.fillToMin(new Random(0))._2 === stdGame.minHeight) ^
      "initial stack has minHeight height" ! {stdGame.initialState(new Random(0))._1.topRow === stdGame.minHeight} ^
    "toprow 0" ! (stdGame.Stack(Array(0,0)).topRow === 0) ^
    "toprow 1" ! (stdGame.Stack(Array(1,0)).topRow === 1) ^
    "play 100 random moves" !
      (FlatActionMDP.randomPlayout(stdGame, new Random(0)).take(100).toArray must not(throwA[Exception])) ^
    "play 100 random moves without minimumHeight" !
      (FlatActionMDP.randomPlayout(InfTetris(minHeight = 0), new Random(0)).take(100).toArray must not(throwA[Exception])) ^
    "play 10 random moves without minimumHeight, number of blocks on board must be divisible by 4 (no line removed)" ! {
      val seq = FlatActionMDP.randomPlayout(InfTetris(minHeight = 0), new Random(0)).take(10).toList
      foreach(seq){case ((stack,_),reward) =>
        (stack.rows.map(Integer.bitCount).sum.aka("block count") must ((i:Int) => (i % 4) === 0)) and (reward === 0d)
      }
    } ^
    "parse a stack and print it again" ! {
      val str = "     \n     \n     \n     \n  #  \n## ##"
      val stack = narrow.Stack.fromString(str)
      stack.toString === str
    } ^
    "drop a piece in narrow" ! {
      val stack = 
        """
          |##   """.narrowStack
      val piece =
        """   #
          |  ###""".narrowStack
      val result =
        """
          |   # """.narrowStack
      
      stack.dropOn(piece, null) === (result,narrow.DropResult(1,0,0))
    } ^
      "clear two non-adjacent lines with tetris" ! {
        val stack =
          """
            |####
            |## #
            |####""".narrowStack
        val piece =
          """    #
            |    #
            |    #
            |    #""".narrowStack
        val result =
          """    #
            |## ##""".narrowStack

        stack.dropOn(piece, null) === (result,narrow.DropResult(2,0,0))

    }^
      "drop something" ! {
        val stack =
          """
            |####
            |## #""".narrowStack
        val piece =
          """
            |
            |  ##
            | ##  """.narrowStack
        val result =
          """  ##
            | ##
            |####
            |## #""".narrowStack

        stack.dropOn(piece, null) === (result,narrow.DropResult(0,0,0))

      }
  
}
