package de.scaltris.rl

import scala.collection.immutable
import scala.util.Random
import scala.util.hashing.MurmurHash3

/** Inifitite Tetris MDP implementation.
  * If the player tops out, the stack "scrolls" upwards and there is some penalty for each topped out line.
  * If the stack height drops below `minHeight`, then garbage lines are added to the bottom and some reward is given.
  * @param width Width of the stack.
  * @param maxHeight If the stack becomes higher than this, then lines at the bottom get removed with a penalty.
  * @param minHeight If the height of the stack falls below this, then garbage gets added at the bottom.
  * @param clearReward Reward gained when clearing lines, depending on how many lines were cleared at once.
  * @param newLineReward Reward for getting a new garbage line.
  * @param topOutLineReward Reward for every topping out one line. Should be negative.*/
case class InfTetris(width: Int = 10, maxHeight: Int = 22,
                     minHeight: Int = 4,
                     clearReward: Int => Double = _ * 1,
                     newLineReward: Int => Double = _ * 1,
                     topOutLineReward: Int => Double = _ * -1000,
                     override val discount: Double = 0.9) extends FlatActionMDP {
  require(width < 32)
  type Tetromino = Int
  type Line = Int

  /**First is the stack, second is next piece. */
  type State = (Stack,Tetromino)

  /** Can be used to represent blocks that rest at y=0, using at most four Integers, each encoding a line. */
  type PartialStack = Array[Line]

  val fullLine: Line = (0 until width).map(1 << _).reduce(_|_)

  /** index with [Tetromino][rotations][x-offset]. Pieces touch the bottom.*/
  val blocks: IndexedSeq[IndexedSeq[IndexedSeq[PartialStack]]] = {
    import Pieces.SetPiece
    def blockToPartialStack(b: Set[(Int,Int)]): PartialStack = {
      val r = new Array[Line](b.maxY + 1)
      b.foreach{ case (x,y) =>
        r(y) = r(y) | (1 << x)
      }
      r
    }

    Pieces.allPiecesRotatedShiftedOrdered(width).map(_.map(_.map(blockToPartialStack)))
  }

  case class Action(rotation: Int, xOffset: Int)

  val actionCache: IndexedSeq[IndexedSeq[Action]] = (0 to 6).map(t =>
    for {
      (offs,rot) <- blocks(t).zipWithIndex
      (_,off) <- offs.zipWithIndex
    } yield Action(rot,off)
  )

  case class DropResult(cleared: Int, receivedGarbage: Int, topoutLines: Int) {
    def reward: Double = clearReward(cleared) + newLineReward(receivedGarbage) + topOutLineReward(topoutLines)
  }

  def randomTetromino(r: Random): Tetromino = r.nextInt(7)

  override def initialState(r: Random): State = (Stack.empty.fillToMin(r)._1, randomTetromino(r))

  override def actions(state: State): IndexedSeq[Action] = actionCache(state._2)

  override def act(state: State, action: Action, r: Random): (State, Double) = {
    val (nextStack, dropResult) = state._1.dropOn(blocks(state._2)(action.rotation)(action.xOffset), r)
    ((nextStack,randomTetromino(r)),dropResult.reward)
  }

  def printPieces(): Unit = {
    for {
      xs <- blocks
      ys <- xs
      z <- ys
    }{
      println(Stack(z,partial = true))
    }
  }

  case class Stack(rows: Array[Line], partial: Boolean = false) {
    assert(partial || rows.length == maxHeight, "created short stack")

    //first empty row
    lazy val topRow: Int = {
      var i = 0
      while(i < rows.length && rows(i) != 0){
        i += 1
      }
      i
    }

    def fullLines: Array[Int] = rows.zipWithIndex.filter(li => isFullLine(li._1)).map(_._2)
    def isFullLine(l: Line): Boolean = (l & fullLine) == fullLine
    /** @return Second is number of added garbage lines. */
    def fillToMin(r: Random): (Stack,Int) = {
      val requiredLines = minHeight - topRow
      if(requiredLines <= 0)
        return (this,0)
      var i = maxHeight - 1
      val res = new Array[Int](maxHeight)
      while(i >= 0){
        res(i) = if(i >= requiredLines) rows(i - requiredLines) else fullLine & ~(1 << r.nextInt(width))
        i -= 1
      }
      (Stack(res),requiredLines)
    }


    /** Adds one line of garbage with a random hole at the bottom. */
    private def addGarbageLineUnsafe(r: Random): Stack = Stack((fullLine & ~(1 << r.nextInt(width))) +: rows.init)
    def dropOn(piece: PartialStack, random: Random): (Stack,DropResult) = {

      val piecePS: Stack = Stack(piece, partial = true)

      //find the height where to place the piece
      def overlaps(height: Int): Boolean = {
        if(height >= maxHeight)
          return false
        var pi = 0
        //don't try to match beyond the stack height
        val topcmp = math.min(piece.length, maxHeight - height)
        while(pi < topcmp){
          if((piece(pi) & rows(height + pi)) != 0)
            return true
          pi += 1
        }
        false
      }

      //how far to translate the piece upwards
      val contactHeight = {
        var tmp = topRow
        while(tmp > 0 && !overlaps(tmp - 1) ) tmp -= 1
        tmp
      }
      //topout penalty, and shrink stack
      val topout = math.max(contactHeight + piecePS.topRow - maxHeight, 0)
      val shrunk = shrink(topout)
      val dropHeight = contactHeight - topout

      //add the piece to the stack
      val added = Stack {
        shrunk.rows.zipWithIndex.map{
          case (line,i) if i >= dropHeight && (i-dropHeight) < piece.length => line | piece(i-dropHeight)
          case (line,i) => line
        }
      }

      //check for full lines
      val (cleared, clearedLines) = added.clearFullLines

      //fill with garbage
      val (refilled, filledLines) = cleared.fillToMin(random)

      (refilled,DropResult(clearedLines, filledLines, topout))
    }

    def clearFullLines: (Stack, Int) = {
      val cleared = rows.filterNot(_ == fullLine)
      val numCleared = maxHeight - cleared.length
      (Stack(cleared ++ Array.fill(numCleared)(0)), numCleared)
    }

    def shrink(lines: Int): Stack = Stack(rows.drop(lines) ++ Array.fill(lines)(0))

    def profile: Array[Int] = (0 until width).map{ col =>
      var h = 0
      while(h < maxHeight && isSet(col,h)) h += 1
      h - 1
    }(collection.breakOut)

    def isSet(col: Int, row: Int): Boolean = (rows(row) & (1 << col)) == (1 << col)

    override def toString: String = {
      def l2s(line: Line): String =
        Iterator.iterate(line)(_ >> 1).map(shifted => if((shifted & 1) == 1) "#" else "·").take(width).mkString
      Seq.fill(width)("*").mkString + "\n" + rows.reverseMap(l2s).mkString("\n") + "\n" + Seq.fill(width)("*").mkString
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case Stack(or,_) => or.sameElements(rows)
      case _         => false
    }

    override def hashCode(): Line = MurmurHash3.arrayHash(rows)
  }

  object Stack{
    def empty: Stack = new Stack(Array.fill(maxHeight)(0))
  }

  /** Weigh eich block with its height.
    * `{(x,y): b(x,y) * (y + 1)}`.*/
  object PotentialEnergy extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val stack = state._1
      var result = 0d
      var i = 0
      while(i < stack.rows.length && stack.rows(i) != 0){
        result += (i+1) * Integer.bitCount(stack.rows(i))
        i += 1
      }
      result
    }
  }
}
