package trollish.shipyard
package jmdict

import scala.collection.mutable.ArrayBuffer
import ammonite.ops._

object RawData {

  def header(delim: String) = Seq("SEQ", "READ", "KANJI", "PRI1", "PRI2", "PRI3", "POS1",
    "POS2", "POS3", "POS4", "FIELD1", "FIELD2", "MISC1", "MISC2", "GLOSS1", "GLOSS2", "GLOSS3",
    "GLOSS4", "GLOSS5", "GLOSS6", "GLOSS7").mkString(delim)

  type Elems = ArrayBuffer[String]
  private[jmdict] case class Row(
      seq: Long = 0L,
      read: String = "",
      kanji: String = "",
      pri: Elems = new Elems,
      pos: Elems = new Elems,
      field: Elems = new Elems,
      misc: Elems = new Elems,
      gloss: Elems = new Elems) {
    def fork(): Row = this.copy(seq = this.seq + 1L, pos = new Elems, field = new Elems,
                                misc = new Elems, gloss = new Elems)
    def mkString(delim: String) = {
      val prints = (new ArrayBuffer[String]()
                   += seq.toString
                   += read
                   += kanji
                   ++= pri.padTo(3, "").take(3)
                   ++= pos.padTo(4, "").take(4)
                   ++= field.padTo(2, "").take(2)
                   ++= misc.padTo(2, "").take(2)
                   ++= gloss.padTo(7, "").take(7))
      prints.mkString(delim)
    }
  }
}
