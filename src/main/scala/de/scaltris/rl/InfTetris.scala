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
case class InfTetris(width: Int = 10,
                     maxHeight: Int = 22,
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
  type PartialStack = Stack

  val fullLine: Line = (0 until width).map(1 << _).reduce(_|_)

  /** index with [Tetromino][rotations][x-offset]. Pieces touch the bottom.*/
  val blocks: IndexedSeq[IndexedSeq[IndexedSeq[PartialStack]]] = {
    import Pieces.SetPiece
    def blockToPartialStack(b: Set[(Int,Int)]): PartialStack = {
      val r = new Array[Line](b.maxY + 1)
      b.foreach{ case (x,y) =>
        r(y) = r(y) | (1 << x)
      }
      Stack(r)
    }

    Pieces.allPiecesRotatedShiftedOrdered(width).map(_.map(_.map(blockToPartialStack)))
  }

  case class Action(rotation: Int, xOffset: Int){
    def getBlock(piece: Tetromino): PartialStack = blocks(piece)(rotation)(xOffset)
  }

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
    val (nextStack, dropResult) = state._1.dropOn(action.getBlock(state._2), r)
    ((nextStack,randomTetromino(r)),dropResult.reward)
  }

  def printPieces(): Unit = {
    for {
      xs <- blocks
      ys <- xs
      z <- ys
    }{
      println(z)
    }
  }

  case class Stack(rows: Array[Line]) {
    /** The first empty row. */
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
    
    def contactHeight(other: Stack): Int = {
      val pieceRows = other.rows
      //find the height where to place the piece
      def overlaps(height: Int): Boolean = {
        if(height >= rows.length)
          return false
        var pi = 0
        //don't try to match beyond the stack height
        val topcmp = math.min(pieceRows.length, rows.length - height)
        while(pi < topcmp){
          if((pieceRows(pi) & rows(height + pi)) != 0)
            return true
          pi += 1
        }
        false
      }

      //how far to translate the piece upwards
      var tmp = topRow
      while(tmp > 0 && !overlaps(tmp - 1) ) tmp -= 1
      tmp      
    }


    /** Adds one line of garbage with a random hole at the bottom. */
    private def addGarbageLineUnsafe(r: Random): Stack = Stack((fullLine & ~(1 << r.nextInt(width))) +: rows.init)

    def dropOn(piece: PartialStack, random: Random): (Stack,DropResult) = {
      val pieceRows = piece.rows

      val contact = contactHeight(piece)

      //topout penalty, and shrink stack
      val topout = math.max(contact + piece.topRow - maxHeight, 0)
      //create the result shrunken by topout lines
      val result: Array[Line] = {
        val r = new Array[Line](maxHeight)
        var i = 0
        while(i < maxHeight){
          r(i) = if(i + topout >= rows.length) 0 else rows(i + topout)
          i += 1
        }
        r
      }

      val dropHeight = contact - topout

      //add the piece to the stack
      {
        var i = 0
        while(i < piece.topRow){
          result(i + dropHeight) = result(i + dropHeight) | pieceRows(i)
          i += 1
        }
      }

      //check for full lines
      val clearedLines: Tetromino = {
        var i = dropHeight - topout
        var cleared = 0
        var j = i
        while(i < result.length){
          while(j < result.length && ((result(j) & fullLine) == fullLine)) {
            cleared += 1
            j += 1
          }

          result(i) = if (j < result.length) result(j) else 0

          i += 1
          j += 1
        }
        cleared
      }

      //fill with garbage
      val (refilled, filledLines) = Stack(result).fillToMin(random)

      (refilled,DropResult(clearedLines, filledLines, topout))
    }

    def clearFullLines: (Stack, Int) = {
      val cleared = rows.filterNot(_ == fullLine)
      val numCleared = maxHeight - cleared.length
      (Stack(cleared ++ Array.fill(numCleared)(0)), numCleared)
    }

    //maxihatops solution

    lazy val profile: Array[Int] = {
      val result = new Array[Int](width)

      // Convert (1 << n) to n for n == 0-10
      var row = maxHeight - 1
      // create bitset for columns for check
      var testbits: Int = (1 << width) - 1
      // Iterate rows from up to bottom, and while exist columns for check
      while(row >= 0 && testbits != 0) {
        var rowtestbits = testbits & rows(row)
        while(rowtestbits != 0) {
          // extract lowest bit_1 from bitset rowtestbits
          val curbit = rowtestbits & -rowtestbits
          result(Stack.bit2ndx(curbit % 11)) = row + 1
          rowtestbits = rowtestbits ^ curbit
          testbits    = testbits ^ curbit
        }
        row -= 1
      }
      result
    }

    def isSet(col: Int, row: Int): Boolean = (rows(row) >> col & 1) == 1

    def occupations: Iterable[(Int,Int)] = for{
      row <- 0 until rows.length
      col <- 0 until width if isSet(col,row)
    } yield (col,row)

    override def toString: String = {
      def l2s(line: Line): String =
        Iterator.iterate(line)(_ >> 1).map(shifted => if((shifted & 1) == 1) "#" else " ").take(width).mkString
      rows.reverseMap(l2s).mkString("\n")
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case Stack(or) => or.sameElements(rows)
      case _         => false
    }

    override def hashCode(): Line = MurmurHash3.arrayHash(rows)
  }

  object Stack{
    final val bit2ndx: Array[Int] = Array(-1, 0, 1, 8, 2, 4, 9, 7, 3, 6, 5)
    def empty: Stack = new Stack(Array.fill(maxHeight)(0))
    def fromString(s: String, blockChar: Char = '#', pad: Boolean = true): Stack = {
      val lines = s.lines.toSeq.reverse
      val array: Array[Int] = lines.zipWithIndex.map{case (row,i) =>
          row.zipWithIndex.filter(_._1 == blockChar).map(x => (1 << x._2) & fullLine).foldLeft(0)(_ | _)
      }(collection.breakOut)
      Stack(if(pad) array.padTo(maxHeight,0) else array)
    }
  }

  /** Weigh eich block with its height.
    * `{(x,y): b(x,y) * (y + 1)}`.*/
  case object PotentialEnergy extends Feature {
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

  case object VTransitions extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val stack = state._1
      var result = 0d
      var i = 1
      while(i < stack.rows.length && stack.rows(i) != 0) {
        result += Integer.bitCount(~stack.rows(i-1) & stack.rows(i))
        i += 1
      }
      result
    }
  }

  case object MaxHeight extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = state._1.topRow
  }

  case object BlockCount extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val rows = state._1.rows
      var i = 0
      var result = 0
      while(i < maxHeight && rows(i) != 0){
        result += Integer.bitCount(rows(i))
        i += 1
      }
      result
    }
  }
  case object DistinctHeights extends Feature {
    require(maxHeight < 32)
    val bits: Array[Int] = (0 to maxHeight).map(1 << _)(collection.breakOut)
    override def compute(state: (Stack, Tetromino)): Double = {
      val p = state._1.profile
      var acc = 0
      var i = 0
      while(i < width){
        acc = acc | bits(p(i))
        i += 1
      }
      Integer.bitCount(acc)
    }
  }
  /** Number of uneven transitions within the profile. */
  case object Hops extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val p = state._1.profile
      var hops = 0
      var i = 0
      while(i < width-1){
        if(p(i) != p(i+1))
          hops += 1
        i += 1
      }
      hops
    }
  }
  /** How often the profile changes direction. */
  case object HopAlternations extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val p = state._1.profile
      var dir = 0 // 1 up, -1 down
      var alternations = 0
      var col = 0
      while(col < width - 1){
        val curDir = p(col) - p(col + 1)
        if((curDir > 0 && dir < 0) || (curDir < 0 && dir > 0)){
          alternations += 1
        }
        if(curDir != 0)
          dir = curDir
        col += 1
      }
      alternations
    }
  }
  /** The number of blocks that are above holes. */
  case object CoveringBlocks extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      //bits become set when the sweep encounters a clear block
      var clearMask = 0
      var result = 0
      var row = 0
      while(row < maxHeight){
        val line = state._1.rows(row)
        //count the set blocks that are also set in the clear mask
        result += Integer.bitCount(line & clearMask)
        clearMask = clearMask | ~line
        row += 1
      }
      result
    }
  }
  /** The number of unset locations covered by blocks. */
  case object NumberOfHoles extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      //same as CoveringBlocks, but inverted (also sweeping downwards)
      //bits become set when the sweep encounters a set block
      var setMaskMask = 0
      var result = 0
      var row = maxHeight - 1
      while(row >= 0){
        val line = state._1.rows(row)
        //count the set blocks that are also set in the clear mask
        result += Integer.bitCount(~line & setMaskMask)
        setMaskMask = setMaskMask | line
        row -= 1
      }
      result
    }
  }
  case object MinHeight extends Feature {
    override def compute(state: (Stack, Tetromino)): Double = {
      val s = state._1
      var mask = 0
      var i = math.min(s.topRow,maxHeight - 1)
      while(i > 0 && mask != fullLine) {
        i -= 1
        mask = mask | s.rows(i)
      }
      if(mask == fullLine) i + 1
      else 0
    }
  }
  val allFeatures: Seq[Feature] = Seq(
    PotentialEnergy,
    VTransitions,
    MaxHeight,
    BlockCount,
    DistinctHeights,
    Hops,
    HopAlternations,
    CoveringBlocks,
    NumberOfHoles,
    MinHeight
  )
}
