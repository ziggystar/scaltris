package de.scaltris.rl

/**
 * Programatic access to Tetris pieces. Used for building other representations.
 */
object Pieces{
  val stringPieces = Seq(    //pieces
   //'I'
    Seq(
      "0100",
      "0100",
      "0100",
      "0100"
    ),
  //'O'
    Seq(
      "0000",
      "0110",
      "0110",
      "0000"
    ),
  //'S'
    Seq(
      "0000",
      "0011",
      "0110",
      "0000"
    ),
  //'Z'
    Seq(
      "0000",
      "1100",
      "0110",
      "0000"
    ),
  //'L'
    Seq(
      "0100",
      "0100",
      "0110",
      "0000"
    ),
  //'J'
    Seq(
      "0010",
      "0010",
      "0110",
      "0000"
    ),
  //'T'
    Seq(
      "0000",
      "0100",
      "1110",
      "0000"
    )
  )

  /** Parse a string representation to a set of int-tuples. */
  def stringToSet(strings: Seq[String]): Set[(Int,Int)] = (for{
    (row,y) <- strings.zipWithIndex
    (c,x) <- row.zipWithIndex if c == '1'
  } yield (x,y))(collection.breakOut)

  /** Generate all four rotations of a set piece. */
  def generateRotations(template: Set[(Int,Int)]): IndexedSeq[Set[(Int,Int)]] = {
    def rotatePoint(x: Int, y: Int): (Int,Int) = (y,3-x)
    Iterator.iterate(template)(_.map((rotatePoint _).tupled)).take(4).toIndexedSeq
  }
  /** All rotations of all pieces as set pieces. */
  val setPieces: IndexedSeq[IndexedSeq[Set[(Int,Int)]]] =
    stringPieces.map(stringToSet).map(generateRotations)(collection.breakOut)

  /** Shift a piece by `shift` steps to the right. */
  def xshift(piece:Set[(Int,Int)],shift: Int): Set[(Int,Int)] = translate(piece,shift,0)
  def translate(piece: Set[(Int,Int)], tx: Int, ty: Int): Set[(Int, Int)] = piece.map{case (px,py) => (px+tx,py+ty)}

  /** Translates, such that minimum x-coordinate is at `x` and minimum y-coordinate is at `y`. */
  def align(piece: Set[(Int,Int)], x: Int, y: Int) = translate(piece, tx = -piece.map(_._1).min, ty = -piece.map(_._2).min)

  /** Get a piece from type, rotation and xshift. */
  def getPiece(pieceType: Int, rotation: Int, shift: Int): Set[(Int,Int)] =
    xshift(setPieces(pieceType)(rotation),shift)

  def tuplesToString(tuples: Set[(Int,Int)]): Seq[String] =
    for(y <- 0 to 3) yield ((0 to 3).map(x => if(tuples((x,y))) '#' else ' ').mkString)

  def printPiece(blocks: Set[(Int,Int)]): Unit = {
    val (tx,ty) = (blocks.map(_._1).min,blocks.map(_._2).min)
    val shifted = translate(blocks,-tx,-ty)
    println(f"translated by $tx/$ty:")
    println(tuplesToString(shifted).mkString("\n"))
  }

  def main(args: Array[String]) {
    for(p <- allPiecesRotatedShifted(4)) {
      printPiece(p)
    }
  }

  /** All pieces in all rotations and all shifts. y-aligned, such that the lowermost block is at `y=0` and extends upwards.
    * The rightmost block is at `x=0` and the left-most block is at `x = stackwidth - 1`.
    * @param stackwidth Width of the stack used for shifting.
    * @return */

  def allPiecesRotatedShifted(stackwidth: Int): Set[Set[(Int,Int)]] = for {
    piece: Set[(Int,Int)] <- stringPieces.map(stringToSet).toSet
    rotP <- generateRotations(piece).toSet.map((p: Set[(Int,Int)]) => align(p,0,0))
    shifted <- (0 to stackwidth).map(xshift(rotP,_)) if shifted.map(_._1).max < stackwidth
  } yield shifted
}