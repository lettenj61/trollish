package trollish

import scala.collection.mutable.ArrayBuffer
import scala.util.Random.{ nextInt, nextBoolean }

class Fabric(val tones: Seq[Tone], val mapper: Mapper) {

  val vowel = (_: Tone).isVowel
  val consonant = !(_: Tone).isVowel
  val alone = (_: Tone).isAlone
  val startable = (_:Tone).canStart
  val closeable = (_:Tone).canClose
  val frequent = (t: Tone, freq: Int) => t.appeared >= freq
  val opposite = (t1:Tone, t2:Tone) => t1.isVowel != t2.isVowel
  val average = tones.map(_.appeared).sum / tones.size

  def vowels = tones filter vowel
  def consonants = tones filter consonant
  def startables = tones filter startable
  def closeables = tones filter closeable
  def aboveAverage = tones.filter(frequent(_, average))

  def nextTone: Tone = nextElem(tones)
  def nextVowel: Tone = nextElem(vowels)
  def nextConso: Tone = nextElem(consonants)
  def nextPopularTone: Tone = nextElem(aboveAverage)
  def nextPopularVowel: Tone = nextElem(aboveAverage filter vowel)
  def nextPopularConso: Tone = nextElem(aboveAverage filter consonant)

  private def nextElem[A](list: Seq[A]): A = list.apply(nextInt(list.size))
  def kindOf(tone: Tone)(kinds: Seq[Tone]): Seq[Tone] = {
    kinds.filter(t => t.canReplace(tone))
  }
  /** Returns a series of vowels if given tone is a consonant, and vise versa. */
  def oppositesTo(tone: Tone)(candidates: Seq[Tone]): Seq[Tone] =
    candidates.filter(opposite(_, tone))

  val fpp = {
    val first = nextElem(aboveAverage.filter(_.canStart))
    val src = if (first.isVowel) first.expr else first.expr + nextPopularVowel.expr
    mapper.translate(src)
  }

  def nextDictionary = {
    val word = nextElem(Vocabulary.words)
    (word, mapper.translate(word))
  }
  def nextWord = mapper.translate(nextElem(Vocabulary.words))
  def newWord(size: Option[Int] = None) = {
    val head = nextElem(startables)
    def conclude(before: Tone): Tone = {
      val candidates = oppositesTo(head)(tones).filter(_.canClose)
      nextElem(candidates)
    }
    val buf = new StringBuilder(head.expr)
    var before = head
    var count = 0
    while (count < size.getOrElse(nextInt(3) + 1)) {
      val rep = before match {
        case _ if count == size => conclude(before)
        case _ => nextElem(oppositesTo(before)(tones))
      }
      count += 1; before = rep
      buf ++= rep.expr
    }

    buf.toString
  }

  def nextTranslation(words: Int = 4) = {
    val buf = new ArrayBuffer[String]
    var count = 0
    while (count < words) {
      buf += nextElem(Vocabulary.words)
      count += 1
    }
    val translated = buf.map(mapper translate _)
    (buf.mkString(" "), translated.mkString(" "))
  }

  def nextPhrase(words: Int = 4) = nextTranslation(words)._2
}

object Fabric {
  import Tones._

  def withDeduplication(): Fabric = {
    val mapper = Mapper(defaultValues, 10, true, average)
    new Fabric(defaultValues, mapper)
  }
}