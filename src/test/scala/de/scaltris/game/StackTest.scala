package de.scaltris.game

import de.scaltris.game.Conversions._
import org.specs2.mutable.Specification

/**
 * Created by IntelliJ IDEA.
 * User: ´Thomas
 * Date: 05.09.2010
 * Time: 11:09:11
 * To change this template use File | Settings | File Templates.
 */

class StackTest extends Specification{

  def createBlock(positions: (Int,Int)*) = new Block(positions.map(t => Position(t._1,t._2)).toSet,Unit,(0,0),false)

  /*
  Simple test stack (width 4, height 4) looks like this

  |    |
  |   T|
  |ooTT|
  |oo T|

   */
  "Simple test stack" should {

    val stack = new Stack(
      Set(
        createBlock((0,0),(1,0),(0,1),(1,1)),
        createBlock((3,0),(2,1),(3,1),(3,2))
      ),
      4,4
    )

    "have one complete line at 1" in {
      stack.completedLines must beEqualTo(Set(1))
    }

    "contain two blocks after removing line 1" in {
      val newStack = stack.removeLines(Set(1))
      newStack.blocks.size must beEqualTo(2)
    }

    "must have the right positions occupied after removing line 1" in {
      val newStack = stack.removeLines(Set(1))
      val expected = Set[Position]((0,0),(1,0),(3,0),(3,1))
      newStack.blocks.flatMap(_.components) must beEqualTo(expected)
    }

    "must receive garbage correctly" in {
      val newStack = stack.addAsGarbage(Set(createBlock((0,0),(2,0),(3,0))))
      val expected = Set[Position](
        //the two blocks shifted by one
        (0,1),(1,1),(0,2),(1,2),
        (3,1),(2,2),(3,2),(3,3),
        //the garbage line
        (0,0),(2,0),(3,0)
      )

      newStack.blocks.flatMap(_.components) must beEqualTo(expected)
    }
  }
  
}