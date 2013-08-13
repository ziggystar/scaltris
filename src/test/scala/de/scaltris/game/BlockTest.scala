package de.scaltris.game

import de.scaltris.game.Conversions._
import org.specs2.mutable.Specification

class BlockTest extends Specification {
  "Long on (0,0) to (0,3) rotating around bottom left of (0,2)" should {
    val block = new Block(Set((0,0),(0,1),(0,2),(0,3)),None,(0,2),true)

    "translate right" in {
      block.translate((2,0)).components must be equalTo(Set((2,0),(2,1),(2,2),(2,3)))
    }

    "be occupy (0,0) and (0,1) after removing lines 1,2" in {
      block.removeLines(Set(1,2)).map(_.components) must beEqualTo(Some(Set((0,0),(0,1)):Set[Position]))
    }
  }

  "Long on (6,15) to (6,18) rotating around bottom left of (6,17)" should {
    val block = new Block(Set((6,15),(6,16),(6,17),(6,18)),None,(6,17),true)
    "rotate right" in {
      block.rotate(1).components must be equalTo(Set((4,16),(5,16),(6,16),(7,16)))
    }
  }
}