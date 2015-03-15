package de.scaltris.rl

/**
 * Programatic access to Tetris pieces. Used for building other representations.
 */
object Pieces{
  val stringPieces: Seq[(Symbol, Seq[String])] = Seq(    //pieces
    'I -> Seq(
      "0100",
      "0100",
      "0100",
      "0100"
    ),
    'O -> Seq(
      "0000",
      "0110",
      "0110",
      "0000"
    ),
    'S -> Seq(
      "0000",
      "0011",
      "0110",
      "0000"
    ),
    'Z -> Seq(
      "0000",
      "1100",
      "0110",
      "0000"
    ),
    'L -> Seq(
      "0100",
      "0100",
      "0110",
      "0000"
    ),
    'J -> Seq(
      "0010",
      "0010",
      "0110",
      "0000"
    ),
    'T -> Seq(
      "0000",
      "0100",
      "1110",
      "0000"
    )
  )

  def pieceByName(pType: Symbol, rotation: Int, xshift: Int): Set[(Int,Int)] =
    stringToSet(stringPieces.toMap.apply(pType)).generateRotations(rotation).align(0,0).xshift(xshift)

  implicit class SetPiece(val piece: Set[(Int,Int)]) extends AnyVal {
    def minX = piece.map(_._1).min
    def maxX = piece.map(_._1).max
    def minY = piece.map(_._2).min
    def maxY = piece.map(_._2).max

    /** Shift a piece by `shift` steps to the right. */
    def xshift(shift: Int): Set[(Int,Int)] = translate(shift,0)
    def translate(tx: Int, ty: Int): Set[(Int, Int)] = piece.map{case (px,py) => (px+tx,py+ty)}

    /** Translates, such that minimum x-coordinate is at `x` and minimum y-coordinate is at `y`. */
    def align(x: Int, y: Int) = translate(tx = -minX, ty = -minY)

    /** Generate all four rotations of a set piece. */
    def generateRotations: IndexedSeq[Set[(Int,Int)]] = {
      def rotatePoint(x: Int, y: Int): (Int,Int) = (y,3-x)
      IndexedSeq.iterate(piece,4)(_.map((rotatePoint _).tupled))
    }

    /** @return All possible x-shifted versions of a piece for given stack width. */
    def generateShifts(stackWidth: Int = 10): IndexedSeq[Set[(Int,Int)]] = {
      val atZero = xshift(-minX)
      for{
        shift <- 0 until stackWidth
        shifted = atZero.xshift(shift) if shifted.maxX < stackWidth
      } yield shifted
    }

    def printPiece(): Unit = {
      val (tx,ty) = (minX,minY)
      val shifted = align(-minX,-minY)
      println(f"translated by $tx/$ty:")
      println(tuplesToString(shifted).mkString("\n"))
    }
  }

  /** Parse a string representation to a set of int-tuples. */
  def stringToSet(strings: Seq[String]): Set[(Int,Int)] = (for{
    (row,y) <- strings.zipWithIndex
    (c,x) <- row.zipWithIndex if c == '1'
  } yield (x,y))(collection.breakOut)

  /** All rotations of all pieces as set pieces. */
  val setPieces: IndexedSeq[IndexedSeq[Set[(Int,Int)]]] =
    stringPieces.map(_._2).map(stringToSet).map(_.generateRotations)(collection.breakOut)

  def typeRotShift(stackWidth: Int = 10): IndexedSeq[IndexedSeq[IndexedSeq[Set[(Int,Int)]]]] =
    setPieces.map(_.map(_.generateShifts(stackWidth)))

  /** Get a piece from type, rotation and xshift. */
  def getPiece(pieceType: Int, rotation: Int, shift: Int): Set[(Int,Int)] =
    setPieces(pieceType)(rotation).xshift(shift)

  def tuplesToString(tuples: Set[(Int,Int)]): Seq[String] =
    for(y <- 0 to 3) yield (0 to 3).map(x => if(tuples((x,y))) '#' else ' ').mkString

  def main(args: Array[String]) {
    for(p <- allPiecesRotatedShifted(4)) {
      p.printPiece()
    }
  }

  /** All pieces in all rotations and all shifts. y-aligned, such that the lowermost block is at `y=0` and extends upwards.
    * The rightmost block is at `x=0` and the left-most block is at `x = stackwidth - 1`.
    * @param stackwidth Width of the stack used for shifting.
    * @return */

  def allPiecesRotatedShifted(stackwidth: Int): Seq[Set[(Int,Int)]] = for {
    piece: Set[(Int,Int)] <- stringPieces.map(_._2).map(stringToSet)
    rotP <- piece.generateRotations.map(_.align(0,0)).toSeq
    shifted <- rotP.generateShifts(stackwidth)
  } yield shifted

  def allPiecesRotatedShiftedOrdered(stackwidth: Int): IndexedSeq[IndexedSeq[IndexedSeq[Set[(Int,Int)]]]] =
    stringPieces.map(_._2).map(stringToSet).map(piece =>
      piece.generateRotations.map(_.align(0,0)).distinct.map(rotP =>
        rotP.generateShifts(stackwidth)
      )(collection.breakOut)
    )(collection.breakOut)
}