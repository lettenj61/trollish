package trollish.shipyard
package jmdict

import scala.collection.mutable.ArrayBuffer
import ammonite.ops._

trait Preprocessor {
  import RawData.{ Row, header }

  val tagsRegex = """(<[/_\w]+>)(.*)""".r
  val reps = """[<]{1}[/_\w]+[>]{1}"""

  /** Extract opening tag from a xml text line, or return the original line itself. */
  def ltag(s: String) = s match {
    case tagsRegex(m, _) => m
    case _ => s
  }

  /** Remove xml control characters from a xml text line and extracts text content. */
  def text(s: String) = s.replaceAll(reps, "")
  
  def convertXml(source: Path, dest: Option[Path] = None, delim: String = "\t"): Unit = {
    val d = dest getOrElse tmp()

    // print header line.
    write.over(d, RawData.header(delim) + "\n")

    // read the corpus file.
    var row = Row()
    val reserves = new ArrayBuffer[Row]
    read.lines(source).foreach { ln =>
      ltag(ln) match {
        case "</entry>" =>
          reserves.foreach { res => write.append(d, res.mkString(delim) + "\n") }
          row = Row()
          reserves.clear
        case "<ent_seq>"  => row = row.copy(seq = (text(ln) + "0").toLong)
        case "<keb>"      => row = row.copy(kanji = text(ln))
        case "<reb>"      => row = row.copy(read = text(ln))
        case "<re_pri>"   => row.pri += text(ln)
        case "<pos>"      => row.pos += text(ln)
        case "<field>"    => row.field += text(ln)
        case "<misc>"     => row.misc += text(ln)
        case "<gloss>"    => row.gloss += text(ln)
        case "</sense>" =>
          if (reserves.size > 0 && row.pos.isEmpty) row.pos ++= reserves(0).pos
          reserves += row
        case "<sense>" => if (reserves.nonEmpty) row = row.fork()
        case _ =>
      }
    }
  }
}

object Preprocessor extends Preprocessor
