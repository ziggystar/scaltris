package de.scaltris.rl

import scala.util.Random
import scala.annotation.tailrec

/**
 * @author Thomas Geier
 * @since 4/29/13
 */

object InfiniteTetris extends FlatActionMDP {
  type State = (Stream[Int],Int)
  type Action = (Int,Int)

  final val stackWidth: Int = 10
  final val fullLine: Int = seq2Bitset(0 until stackWidth)

  /**
   * pieces[type][rotation][shift]
   */
  val pieces: IndexedSeq[IndexedSeq[IndexedSeq[Set[(Int,Int)]]]] = {
    import Pieces._
    typeRotShift(stackWidth).map(_.map(_.map(p =>
      p.map{case(x,y) => (x,-y)}.translate(0,-p.minY)
    )))
  }

  def bitSet2Seq(i: Int): Seq[Int] = for (s <- 0 until stackWidth if ((i >> s) & 1) == 1) yield s
  def seq2Bitset(xs: Iterable[Int]): Int = xs.map(1 << _).foldLeft(0)(_ | _)

  @tailrec
  def profile(stack: Stream[Int], depth: Int = 0, acc: Array[Int] = Array.fill(stackWidth)(-1), hit: Int = 0): Array[Int] =
    if(hit == stackWidth)
      acc
    else{
      var newHit = hit
      for(col <- bitSet2Seq(stack.head) if acc(col) < 0){
        acc(col) = depth
        newHit += 1
      }
      profile(stack.tail, depth + 1, acc, newHit)
    }

  def initialState(seed: Long): State = {
    val random = new Random(seed)
    (Stream.continually(random.nextInt(stackWidth)).map(x => ((1 << stackWidth) - 1) & ~(1 << x)),random.nextInt(7))
  }

  def act(state: State, action: Action, seed: Long): (State, Double) = {
    import Pieces._

    val (stack,nextType) = state

    val piece = pieces(nextType)(action._1)(action._2)
    val pieceProfile: Map[Int,Int] = piece.groupBy(_._1).mapValues(_.minY)
    val stackProfile = profile(stack)
    //y direction for piece is upwards, for stack profile is downwards; so we need to add them
    val deltaY: Int = (for {
      (x,pieceBottom) <- pieceProfile.iterator
    } yield stackProfile(x) + pieceBottom).min

    val addedStack = addPiece(stack,piece.translate(0,deltaY))
    val (newStack,cleared) = clearLines(addedStack)
    val random = new Random(seed)
    ((newStack,random.nextInt(7)),cleared)
  }

  /**
   * Adds a piece to a stack.
   * @param stack The stack, with the first Int representing the top row. Least-significant bit is on the left.
   * @param piece Representation of a piece. Block coordinates have y-direction going up.
   * @return
   */
  def addPiece(stack: Stream[Int], piece: Set[(Int,Int)]): Stream[Int] = {
    import Pieces._
    val rows = pieceToRows(piece).withDefaultValue(0)
    updateStream(stack,piece.minY,piece.maxY+1)({ (row, rowIdx) =>
      val pieceRow = rows(rowIdx)
      assert((pieceRow & row) == 0, "block collision while adding piece")
      row | pieceRow
    }, 0)
  }

  def pieceToRows(piece: Set[(Int,Int)]): Map[Int,Int] = piece.groupBy(_._2).mapValues(row => seq2Bitset(row.map(_._1)))

  def updateStream[A](_s: Stream[A],_from: Int, _to: Int)(f: (A,Int) => A, zero: A): Stream[A] = {
    //extend stream
    val (s,from,to) =
      if(_from < 0) (Stream.fill(-_from)(zero) ++ _s,0,_to - _from)
      else (_s,_from,_to)

    val (prefix, rest1) = s.splitAt(from)
    val (segment, tail) = rest1.splitAt(to - from)
    prefix ++ segment.zipWithIndex.map{case (a,i) => (a,i+_from)}.map(f.tupled) ++ tail
  }

  def clearLines(stack: Stream[Int], maxLook: Int = -1): (Stream[Int], Int) = {
    val (examine,rest) = stack.splitAt(if(maxLook >= 0) maxLook else profile(stack).max + 4)
    (examine.filterNot(_ == fullLine) ++ rest,examine.count(_ == fullLine))
  }

  def printStack(stack: Stream[Int], rows: Int = 10): String = {
    stack.take(rows).map{r =>
      val rowSet = bitSet2Seq(r).toSet
      String.valueOf((0 until stackWidth).map(x => if(rowSet(x)) '#' else ' '))
    }.mkString("\n")
  }

  override def initialState(r: Random): (Stream[Int], Int) = ???

  override def actions(state: (Stream[Int], Int)): IndexedSeq[(Int, Int)] = ???

  override def act(state: (Stream[Int], Int), action: (Int, Int), r: Random): ((Stream[Int], Int), Double) = ???
}
