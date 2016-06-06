package trollish

import scala.util.{ Random => R }

object Utils {
  import R.{ nextInt, nextBoolean }

  /** Randomly select an element from given sequence. */
  def nextElem[A](seq: Seq[A]): A = seq.apply(nextInt(seq.size)) 
}
