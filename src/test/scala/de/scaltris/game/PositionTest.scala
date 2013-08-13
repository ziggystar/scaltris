package de.scaltris.game

import org.specs2.mutable.Specification

class PositionTest extends Specification {
  "Position 0,0" should {
    "translate correctly by 1,2" in {
      Position(0, 0).translate(Position(1, 2)) must be equalTo (Position(1, 2))
    }

    "translate using - operator" in {
      Position(0, 0) - Position(2, 3) must be equalTo (Position(-2, -3))
    }

//    "rotate to 0,0 when rotating around center of 0,0" in {
//      (-5 to 9).foreach {
//        rot =>
//          Position(0, 0).rotateAround(Position(0, 0), rot, false) must be equalTo (Position(0, 0))
//      }
//    }

    "rotate to 0,-1 when rotating 1 around corner of 0,0" in {
      Position(0,0).rotateAround(Position(0,0), 1, true) must be equalTo(Position(0,-1))
    }

    "rotate to -1,-1 when rotating 2 around corner of 0,0" in {
      Position(0,0).rotateAround(Position(0,0), 2, true) must be equalTo(Position(-1,-1))
    }

    "rotate to -1,0 when rotating 3 around corner of 0,0" in {
      Position(0,0).rotateAround(Position(0,0), 3, true) must be equalTo(Position(-1,0))
    }

    "rotate to -1,1 when rotating 1 around center of 0,1" in {
      Position(0,0).rotateAround(Position(0,1), 1, false) must be equalTo(Position(-1,1))
    }

    "rotate to 0,3 when rotating 2 around center of 0,1" in {
      Position(0,0).rotateAround(Position(0,1), 2, false) must be equalTo(Position(0,2))
    }

    "rotate to 1,1 when rotating 3 around center of 0,1" in {
      Position(0,0).rotateAround(Position(0,1), 3, false) must be equalTo(Position(1,1))
    }

    "rotate to 1,1 when rotating -1 (3) around center of 0,1" in {
      Position(0,0).rotateAround(Position(0,1), -1, false) must be equalTo(Position(1,1))
    }
  }
}