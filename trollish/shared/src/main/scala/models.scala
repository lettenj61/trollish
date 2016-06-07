package trollish

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import scala.annotation.tailrec

/** Type of a tone divided by its sound.
  */
object Tastes {

  sealed trait Taste
  case object Vowel extends Taste
  case object Consonant extends Taste
  case object Unknown extends Taste
}

/** String representation of a tone within a word.
  */
case class Tone(expr: String, taste: Tastes.Taste, appeared: Int, started: Int, closed: Int) {
  import Tastes._

  def isVowel: Boolean = taste match {
    case Vowel  => true
    case _      => false
  }

  def compare(that: Tone): Int = this.appeared - that.appeared
  def canReplace(that: Tone): Boolean = {
    this.isVowel == that.isVowel &&
    this.canStart == that.canStart &&
    this.canClose == that.canClose
  }

  def isAlone: Boolean = appeared == 1
  def length: Int = expr.length

  def canStart: Boolean = started > 0
  def canClose: Boolean = closed > 0
}
object Tone {
  val vowels = Set("a", "e", "i", "o", "u")
  val unknown = Tone("_unknown_", Tastes.Unknown, 0, 0, 0)

  /** Simplified regex parsing. */
  val parseRegex = "[aeiouy]+|[^aeiouy]+".r
  def parse(word: String): Seq[String] = {
    parseRegex.findAllIn(word.toLowerCase).matchData.map(_.toString).toList
  }
  def parseSentence(source: String): Seq[Seq[String]] = {
    val buf = new ArrayBuffer[Seq[String]]
    source.split("\\s").foreach { word => buf += parse(word) }
    buf.toList
  }
}

/** A tone's replacement. */
case class Rep(onHead: String, onBody: String, onTail: String) {
  def this(expr: String) = this(expr, expr, expr)
}
object Rep {
  lazy val constant = Map(
    "a" -> new Rep("a"),
    "e" -> new Rep("e"),
    "i" -> new Rep("i"),
    "o" -> new Rep("o"),
    "u" -> new Rep("u")
  )
}

/** Mapping a tone with replacements. */
class Mapper(val replacements: Map[String, Rep], private var singleVowels: Map[String, String]) {
  import Mapper._

  def newSingleVowels(): Unit = {
    singleVowels = newSingleVowelMap()
  }
  def get(expr: String): Rep = {
    val t = expr.trim
    if (Tone.vowels(t)) Rep.constant(singleVowels(t))
    else replacements getOrElse(t, new Rep(t))
  }

  def singleVowelsRef = singleVowels.toMap

  def updated(expr: String, rep: Rep): Mapper =
    new Mapper(replacements.updated(expr, rep), singleVowels)
  def updated(expr: String, onHead: String, onBody: String, onTail: String): Mapper = {
    val rep = new Rep(onHead, onBody, onTail)
    updated(expr, rep)
  }
  def updated(expr: String, anywhere: String): Mapper = updated(expr, new Rep(anywhere))
  def updated(reps: (String, Rep)*): Mapper = new Mapper(replacements ++ reps, singleVowels)
}
object Mapper {
  import Random.nextInt
  import Utils.nextElem

  /** Create translations for special vowels which represent themselves as a single character. */
  def newSingleVowelMap(): Map[String, String] = {

    // We omit `y`.
    val v = "aeiou"

    // Slide the sequence backwards randomly.
    val (h, t) = v.splitAt(nextInt(v.length - 1) + 1).swap
    v.zip(h + t).map { case (from, to) =>
      (from.toString, to.toString)
    }.toMap
  }

  /** Create a new mapper. */
  def apply(tones: Seq[Tone], retry: Int = 1, deduplicate: Boolean = false,
            threshold: Int = Int.MaxValue): Mapper = {

    def resembles(t1: Tone, t2: Tone, threshold: Option[Int] = None): Boolean = {
      if (t1.isAlone || t2.isAlone) t1.isAlone && t2.isAlone
      else threshold match {
        case None => true
        case Some(i) => (t1 compare t2).abs < i
      }
    }

    def candidate(tone: Tone, source: Seq[Tone]): Tone = {
      import Tone._

      val candidates = source.filter(t => tone.canReplace(t) && t != tone)

      if (candidates.isEmpty) tone
      else if (!deduplicate) nextElem(candidates)
      else {
        val buf = candidates.toBuffer
        @tailrec def go(tone: Tone, next: Tone, thres: Int): Tone = {
          buf -= next
          if (buf.isEmpty || resembles(tone, next, Some(thres))) next
          else go(tone, buf(nextInt(buf.size)), thres + thres)
        }

        go(tone, buf.head, threshold)
      }
    }

    val spent = new ArrayBuffer[String]
    val singles = newSingleVowelMap()
    val mapping = tones.map { t =>
      val expr = t.expr
      var tried = 0
      var alt = {
        if (t.isVowel && t.length == 1 && expr != "y") {
          tried = retry
          singles getOrElse(expr, expr)
        } else {
          candidate(t, tones).expr
        }
      }
      while (spent.contains(alt) && tried < retry) {
        alt = candidate(t, tones).expr
        tried += 1
      }
      spent += alt
      (expr, new Rep(alt))
    }.toMap
    new Mapper(mapping, singles)
  }
}
