package de.scaltris.rl

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier
 * @since 11/24/13
 */

class InfiniteTetris$Test extends Specification {
  import InfiniteTetris._
  import Pieces._

  def fullLine: Int = seq2Bitset(0 to 9)
  def fullLineStack = Stream(fullLine)

  def is: Fragments =
    "bitset methods" ^
      "bs to seq" ^
        "empty seq" ! (bitSet2Seq(0) === Seq()) ^
        "one" ! (bitSet2Seq(1) === Seq(0)) ^
        "two" ! (bitSet2Seq(2) === Seq(1)) ^
        "one,two" ! (bitSet2Seq(3) === Seq(0,1)) ^
        p^
      "seq to bs" ^
        "empty seq" ! (seq2Bitset(Seq()) === 0) ^
        "one" ! (seq2Bitset(Seq(0)) === 1) ^
        "two" ! (seq2Bitset(Seq(1)) === 2) ^
        "one,two" ! (seq2Bitset(Seq(0,1)) === 3) ^
      p^
    p^
    "profile method" ^
      "full line profile" ! (profile(Stream(fullLine)).deep === Seq.fill(10)(0)) ^
      "full line profile at 1" ! (profile(Stream(0,fullLine)).deep === Seq.fill(10)(1)) ^
      "random config" ! {
        val stack = Stream(seq2Bitset(Seq(3,6,7)),seq2Bitset(Seq(0,1,2)),fullLine)
        profile(stack).deep === Seq(1,1,1,0,2,2,0,0,2,2)
      } ^
      p^
    "add piece to stack" ^
      "add 'O on top of full line" ! (addPiece(fullLineStack,Pieces.pieceByName('O,0,0).translate(0,-2)) === Stream(3,3,fullLine)) ^
      "add 'O one above full line" ! (addPiece(fullLineStack,Pieces.pieceByName('O,0,0).translate(0,-3)) === Stream(3,3,0,fullLine)) ^
      p^
    "update stream" ^
      "do an update" ! (updateStream(Stream(1,2,3),0,1)((x,_) => x + 1, 0) === Stream(2,2,3)) ^
      "extend beyond zero" ! (updateStream(Stream(1,2,3),-1,1)((x,_) => x + 1, 0) === Stream(1,2,2,3)) ^
      "overwrite with index" ! (updateStream(Stream(1,2,3),-2,1)((_,i) => i,0) === Stream(-2,-1,0,2,3)) ^
    "act" ^
      "something" ! (act(initialState(0),(0,0),0) must not throwAn)


}