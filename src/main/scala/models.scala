package trollish
package models

import scala.collection.mutable.Buffer
import scala.util.Random

import natives.Corpus

/** String representation of a tone within a word.
  */
sealed trait Tone {

  /** Actural string expression. */
  def expr: String

  def stats: Stats
  def isVowel: Boolean

  def length: Int = expr.length

  /** Frequencies of appearance. */
  def appeared: Int = stats.appeared
  def started: Int = stats.started
  def ended: Int = stats.ended

  /** Does this tone appear at head of a word? */
  def canStart: Boolean = started > 0
  /** Can this tone be the last element of a word? */
  def canEnd: Boolean = ended > 0
}

case class Vowel(expr: String, stats: Stats) extends Tone {
  def isVowel = true
}

case class Consonant(expr: String, stats: Stats) extends Tone {
  def isVowel = false
}

final case class Unknown(expr: String, isVowel: Boolean = false) extends Tone {
  val stats = Stats.empty
}

object Tone {
  val vowels = "aeiouy".toSet
  def apply(corpus: Corpus): Tone = {
    val expr = corpus.expr
    val stats = Stats(corpus.appeared, corpus.started, corpus.ended)
    expr match {
      case v if expr.forall(vowels(_)) => Vowel(expr, stats)
      case _ => Consonant(expr, stats)
    }
  }
}

/** Statistics of the tone at the given corpus. */
case class Stats(appeared: Int = 0, started: Int = 0, ended: Int = 0)
object Stats {
  val empty = new Stats()
}

/** A tone's replacement. */
case class Rep(onHead: String, onBody: String, onTail: String) {
  def this(expr: String) = this(expr, expr, expr)
}

/** Simplified regex parser. */
object Parser {
  val regex = "[aeiouy]+|[^aeiouy]+".r
  def parse(word: String): Seq[String] = {
    regex.findAllIn(word.toLowerCase).matchData.map(_.toString).toList
  }
}

class Mapper(val replacements: Map[String, Rep], var singleVowels: Map[String, String]) {
  import Mapper._

  def get(expr: String): Rep = replacements getOrElse(expr, new Rep(expr))
}
object Mapper {

  /** Create translations for special vowels which represent themselves as a single character. */
  def newSingleVowelMap(): Map[String, String] = {

    // We omit `y`.
    val v = "aeiou"

    // Slide the sequence backwords at-random.
    val (h, t) = v.splitAt(Random.nextInt(v.length - 1) + 1).swap
    v.zip(h + t) map { case (from, to) =>
      (from.toString, to.toString)
    }.toMap
  }

  /** Create a new mapper. */
  def create(tones: Seq[Tone], retryLimit: Int = 9): Mapper = {

    // Judges that we can treat given two tones in same group or not.
    def isSameGroup(t1: Tone, t2: Tone) = {
      t1.isVowel == t2.isVowel &&
      t1.canStart == t2.canStart &&
      t1.canEnd == t2.canEnd
    }

    // Fetch a list of candidates which can replace given tone.
    def getCandidate(tone: Tone, index: Seq[Tone]): Tone = {
      val cand = index.filter(t => t.appeared > tone.appeared && isSameGroup(tone, t))
      if (cand.isEmpty) tone else cand(Random.nextInt(cand.length))
    }

    val spent = new Buffer[String]
    val singles = newSingleVowelMap()
    val mapping = tones.map { t =>
      var tried = 0
      val expr = t.expr
      var alt = {
        if (t.isVowel && t.length == 1 && expr != "y") {
          tried = retryLimit
          singles getOrElse(expr, expr)
        } else {
          getCandidate(t, tones).expr
        }
      }
      while (spent.contains(alt) && tried < retryLimit) {
        alt = getCandidate(t, tones).expr
        tried += 1
      }
      spent += alt
      (expr, new Rep(alt))
    }.toMap
    new Mapper(mapping, singles)
  }
}
