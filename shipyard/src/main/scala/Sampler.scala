package trollish.shipyard

import ammonite.ops._
import upickle.default

import trollish._

trait Sampler {

  case class Stats(appeared: Int, started: Int, closed: Int) {
    def toVowel(expr: String) = Tone(expr, Tastes.Vowel, appeared, started, closed)
    def toConsonant(expr: String) = Tone(expr, Tastes.Consonant, appeared, started, closed)

    def merge(that: Stats): Stats = copy(
      this.appeared + that.appeared,
      this.started + that.started,
      this.closed + that.closed
    )
  }

  def createSample(words: String, delim: String = ","): Map[String, Tone] = {
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'y')
    val result = collection.mutable.Map.empty[String, (Boolean, Stats)]

    words.split(delim).foreach { word =>
      val elements = Tone.parse(word)
      // intermediate result
      elements.foreach { elem =>
        val stats = Stats(
          1,
          if (word.startsWith(elem)) 1 else 0,
          if (word.endsWith(elem)) 1 else 0
        )

        val adding = result.get(elem).map(_._2).getOrElse(Stats(0, 0, 0))
        result.put(elem, (elem.forall(vowels), stats.merge(adding)))
      }
    }

    result.map { case (k, (isVowel, s)) =>
      (k, if (isVowel) s.toVowel(k) else s.toConsonant(k))
    }.toMap
  }
}

object Sampler extends Sampler
