package de.scaltris.rl

import org.specs2.Specification

/**
 * Created with IntelliJ IDEA.
 * User: thomas
 * Date: 8/13/13
 * Time: 2:10 PM
 * To change this template use File | Settings | File Templates.
 */
class LMoveTest extends Specification {
  def is =
    "convert single block to LMove" ! (LMove.fromSet(Set((0, 0))).data === 1L) ^
      "convert double block to LMove" ! (LMove.fromSet(Set((0, 0), (1, 1))).data === (0x1L | 0x20000L)) ^
      "convert double block LMove to set" ! (LMove.fromSet(Set((0, 0), (1, 1))).asSet === Set((0, 0), (1, 1))) ^
      "convert all pieces to LMove and back" !
        ((bs: Set[(Int, Int)]) => LMove.fromSet(bs).asSet === bs).forall(Pieces.allPiecesRotatedShifted(6))
}
