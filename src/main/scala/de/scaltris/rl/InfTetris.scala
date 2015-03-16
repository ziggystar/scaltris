package de.scaltris.rl

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
                     topOutLineReward: Int => Double = _ * -10) extends FlatActionMDP {
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

  case class DropResult(cleared: Int, receivedGarbage: Int, topoutLines: Int) {
    def reward: Double = clearReward(cleared) + newLineReward(receivedGarbage) + topOutLineReward(topoutLines)
  }
  
  case class Stack(rows: Array[Line], partial: Boolean = false) {
    require(partial || rows.length == maxHeight, "created short stack")
    
    def isRowEmpty(row: Line): Boolean = row == 0

    def topRow: Int = rows.zipWithIndex.foldLeft(0){
    case (m,(r,i)) if isRowEmpty(r) => m
    case (m,(r,i)) => i + 1
  }
    def fullLines: Array[Int] = rows.zipWithIndex.filter(li => isFullLine(li._1)).map(_._2)
    def isFullLine(l: Line): Boolean = (l & fullLine) == fullLine
    /** @return Second is number of added garbage lines. */
    def fillToMin(r: Random): (Stack,Int) = {
      val requiredLines = math.max(minHeight - topRow,0)
      val newStack = Iterator.iterate(this)(_.addGarbageLineUnsafe(r)).drop(requiredLines).next()
      (newStack,requiredLines)
    }
    /** Adds one line of garbage with a random hole at the bottom. */
    private def addGarbageLineUnsafe(r: Random): Stack = Stack((fullLine & ~(1 << r.nextInt(width))) +: rows.init)
    
    def dropOn(piece: PartialStack, random: Random): (Stack,DropResult) = {
      //find the height where to place the piece
      def overlaps(height: Int): Boolean = piece.zip(rows.drop(height)).exists{case (l1,l2) => (l1 & l2) != 0}
      val contactHeight = {
        var tmp = topRow
        while(!overlaps(tmp)) tmp -= 1
        tmp
      }
      
      //topout penalty, and shrink stack
      val topout = math.max(contactHeight + Stack(piece, partial = false).topRow - maxHeight, 0)
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
      var h = topRow
      while(isSet(col,h)) h -= 1
      h
    }(collection.breakOut)
    
    def isSet(col: Int, row: Int): Boolean = (rows(row) & (1 << col)) == (1 << col) 

    override def toString: String = {
      def l2s(line: Line): String =
        Iterator.iterate(line)(_ >> 1).map(shifted => if((shifted & 1) == 1) "#" else "·").take(width).mkString
      Seq.fill(width)("*").mkString + "\n" + rows.map(l2s).mkString("\n") + "\n" + Seq.fill(width)("*").mkString
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


  def randomTetromino(r: Random): Tetromino = r.nextInt(7)

  override def initialState(r: Random): State = (Stack.empty.fillToMin(r)._1, randomTetromino(r))

  override def actions(state: State): IndexedSeq[Action] = for {
    (offs,rot) <- blocks(state._2).zipWithIndex
    (_,off) <- offs.zipWithIndex
  } yield Action(rot,off)

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
      println(Stack(z,true))
    }
  }
}
