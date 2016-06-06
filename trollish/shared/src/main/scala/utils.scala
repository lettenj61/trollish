package trollish

import scala.util.Random

object Utils {
  import Random.{ nextInt, nextBoolean }

  /** Randomly select an element from given sequence. */
  def nextElem[A](seq: Seq[A]): A = seq.apply(nextInt(seq.size)) 
}
