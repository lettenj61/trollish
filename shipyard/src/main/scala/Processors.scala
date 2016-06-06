package trollish.shipyard
package jmdict

import ammonite.ops._

trait KanaFilter {

  val katakanas = ((0x30A0: Char) to (0x30FF: Char)).toSet
  def dropKanaWords(src: Path, dest: Option[Path] = None, delim: String = "\t"): Path = {
    val target = dest getOrElse tmp()
    write.over(target, RawData.header(delim) + "\n")

    read.lines(src).tail.foreach { ln =>
      val elems = ln.split(delim, -1)
      if (!elems(1).forall(katakanas)) write.append(target, ln + "\n")
    }

    target
  }
}

trait ReverseDictionary extends CsvProcess {
  import scala.collection.mutable.HashMap

  val specialChars = """\W\s_-""".r
  case class FakeEnglish(word: String, pos: Seq[String], gloss: Seq[String])

  def reverseIndex(src: Path, dest: Option[Path] = None): Path = {

    def isSimpleEnough(s: String): Boolean = {
      val dquot = '"'.toString
      val restricts = dquot + " -+()0123456789"
      !s.exists(restricts.toSet) && specialChars.findFirstIn(s).isEmpty
    }

    def kanjiOpt(s: String): String = if (s.isEmpty) "no_kanji" else s

    process(src, dest){ case (in, out) =>
      val cache = new HashMap[String, FakeEnglish]
      read.lines(in).tail.foreach { ln =>

        val elems = ln.split(delim, -1)
        val reads = elems(1) + ":" + kanjiOpt(elems(2))
        val pos = elems.drop(6).dropRight(11)
        val glosses = elems.takeRight(7)

        glosses.filterNot(_.isEmpty).foreach { word =>
          if (isSimpleEnough(word) && cache.contains(word)) {
            val eng = cache(word)
            val newPos = (eng.pos ++ pos).distinct.sorted
            cache.update(word, eng.copy(pos = newPos, gloss = eng.gloss ++ Seq(reads)))
          } else if (isSimpleEnough(word)) {
            val eng = FakeEnglish(word, pos, Seq(reads))
            cache += (word -> eng)
          }
        }
      }

      cache.values.toList.sortBy(_.word).foreach { row =>
        val pos = row.pos.withFilter(_.nonEmpty).map(_.tail.dropRight(1)).mkString(":")
        val glosses = row.gloss.sorted.padTo(7, "").take(7).mkString("\t")
        val prints = s"${row.word}\t$pos\t$glosses"
        write.append(out, prints + "\n")
      }

      out
    }
  }
}

trait EnglishFilter extends CsvProcess {

  def collectPopularWords(src: Path, dest: Option[Path] = None): Path = {
    process(src, dest){ case (in, out) =>

      read.lines(in).foreach { ln =>
        val elems = ln.split(delim, -1)
        val glosses = elems.takeRight(7)
        if (glosses.filterNot(_.isEmpty).size > 3) write.append(out, ln + "\n")
      }
      out
    }
  }

  def noAcronym(src: Path, dest: Option[Path] = None): Path = {
    process(src, dest){ case (in, out) =>
      read.lines(in).foreach { ln =>
        val word = ln.split(delim, -1)(0)
        if (!word.forall(_.isUpper)) write.append(out, ln + "\n")
      }
      out
    }
  }

  def nouns(src: Path, dest: Option[Path] = None, delim: String = "\t"): Path = {
    def hasNoun(elems: Seq[String]) = elems.exists(_ == "n")
    process(src, dest){ case (in, out) =>
      read.lines(in).foreach { ln =>
        if (hasNoun(ln.split(":"))) write.append(out, ln + "\n")
      }
      out
    }
  }
}

object Processors extends KanaFilter with EnglishFilter with ReverseDictionary
