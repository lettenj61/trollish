package trollish

import scala.collection.mutable.ArrayBuffer
import scala.util.Random.{ nextInt, nextBoolean }

case class Fabric(tones: Seq[Tone], mapper: Mapper) {
  import Utils.nextElem

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

  def nextTranslation(words: Int = nextInt(5) + 2) = {
    val buf = new ArrayBuffer[String]
    var count = 0
    while (count < words) {
      buf += nextElem(Vocabulary.words)
      count += 1
    }
    val translated = buf.map(mapper translate _)
    (buf.mkString(" "), translated.mkString(" "))
  }

  def nextPhrase(words: Int = nextInt(5) + 2) = nextTranslation(words)._2

  def updateMapper(mapper: Mapper): Fabric = copy(mapper = mapper)

  override def toString = {
    val welcome = "welcome to your language"
    (welcome, mapper.translateSentence(welcome)).toString
  }
}

object Fabric {
  import Tones._

  def deduplicated(tones: Seq[Tone] = defaultValues,
                   retry: Int = 10, threshold: Int = average): Fabric = {
    val mapper = Mapper(tones, retry, true, threshold)
    Fabric(tones, mapper)
  }
}
