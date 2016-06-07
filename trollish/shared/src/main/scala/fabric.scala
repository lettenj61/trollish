package trollish

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.util.Random.{ nextInt, nextBoolean }

case class Fabric(tones: Seq[Tone], mapper: Mapper) {
  import Utils.nextElem

  val reps = new HashMap[String, Rep]
  def get(expr: String) = reps.getOrElse(expr, mapper.get(expr))

  def tone(expr: String) = tones.find(_.expr == expr)
  def near(expr: String, threshold: Int = Int.MaxValue): Seq[Tone] = tone(expr) match {
    case None     => Nil
    case Some(t)  => candidates { u =>
      u.canReplace(t) && (u compare t).abs < threshold && u != t
    }.sortBy(_.compare(t).abs)
  }

  val frequent = (t: Tone, freq: Int) => t.appeared >= freq
  val opposite = (t1:Tone, t2:Tone) => t1.isVowel != t2.isVowel
  val average = tones.map(_.appeared).sum / tones.size

  def vowels = tones.filter(_.isVowel)
  def consonants = tones.filterNot(_.isVowel)
  def aboveAverage = tones.filter(frequent(_, average))

  def random(appearance: Int = 0): Tone = nextElem(tones.filter(frequent(_, appearance)))
  def randomPopular = random(average)
  def randomVowel(appearance: Int = 0): Tone = nextElem(vowels.filter(frequent(_, appearance)))
  def randomConso(appearance: Int = 0): Tone = nextElem(consonants.filter(frequent(_, appearance)))

  def translate(word: String): String = {
    val elems = Tone.parse(word)
    def head = get(elems.head).onHead
    def last = get(elems.last).onTail
    if (elems.isEmpty) ""
    else if (elems.size == 1) head
    else if (elems.size == 2) head + last
    else {
      val body = elems.tail.dropRight(1).map { t => get(t).onBody }
      head + body.mkString + last
    }
  }

  def sentence(s: String) = s.split("\\s").map(translate).mkString(" ")
  def display(word: String) = (word, translate(word))
  def showSentence(s: String) = (s, sentence(s))

  def randomWord = display(nextElem(Vocabulary.words))

  def randomSentence(length: Int = nextInt(5) + 2) = {
    val buf = new ArrayBuffer[String]
    var count = 0
    while (count < length) {
      buf += nextElem(Vocabulary.words)
      count += 1
    }
    val translated = buf.map(translate)
    (buf.mkString(" "), translated.mkString(" "))
  }

  /** Collect element from corpus `tones` that satisfies given predicate `p`. */
  def candidates(p: Tone => Boolean) = tones.filter(p)

  /** First person pronoun. */
  val fpp = {
    val first = nextElem(aboveAverage.filter(_.canStart))
    val src = if (first.isVowel) first.expr else first.expr + randomVowel(average).expr
    translate(src)
  }

  def newSingleVowels = mapper.newSingleVowels()

  def withMapper(mapper: Mapper): Fabric = copy(mapper = mapper)
  def mergeReps(): Fabric = withMapper(mapper.updated(reps.toList: _*))

  def rep(expr: String, alt: String): Unit = reps += (expr -> new Rep(alt))
  def rep(pair: (String, String)): Unit = rep(pair._1, pair._2)

  override def toString = {
    val (from, to) = showSentence("welcome to your language")
    s"Fabric($from => $to)"
  }

  def stringify = prettyPrint(-1)

  def prettyPrint(indent: Int = 2) = {
    import upickle.default._

    val values = tones.map { t =>
      val expr = t.expr
      (expr, Seq(get(expr).onHead, get(expr).onBody, get(expr).onTail))
    }.toMap

    if (indent > 0) write(values, indent = indent) else write(values)
  }
}

object Fabric {
  import Tones._

  def deduplicated(tones: Seq[Tone] = defaultValues,
                   retry: Int = 10, threshold: Int = average): Fabric = {
    val mapper = Mapper(tones, retry, true, threshold)
    Fabric(tones, mapper)
  }

  def withDefault(retry: Int = 10): Fabric = {
    val mapper = Mapper(tones = defaultValues, retry = retry)
    Fabric(defaultValues, mapper)
  }

  def parse(input: String): Fabric = {
    import upickle.default._
    val raw = read[Map[String, Seq[String]]](input)
    val singles = raw.filter { case (k, _) => Tone.vowels(k) }.map(ent => (ent._1, ent._2(0)))
    val reps = raw.map { case (k, vs) => (k, new Rep(vs.head)) }

    val mapper = new Mapper(reps, singles)
    Fabric(defaultValues, mapper)
  }
}
