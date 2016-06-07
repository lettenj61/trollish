package trollish

import scala.language.implicitConversions

object Implicits {

  implicit def string2DefaultTone(s: String) = Tones.default.getOrElse(s, Tone.unknown)

  object Printer {
    import scala.collection.mutable

    val kanaAlters: mutable.Buffer[(String, String)] = mutable.ArrayBuffer()
    def kanaTable = KanaConstants.default ++ kanaAlters

    def translate(s: String)(implicit fab: Fabric) = println(fab.showSentence(s))
    def display(s: String)(implicit fab: Fabric) = println(fab.display(s))
    def random(implicit fab: Fabric) = fab.randomSentence()
    def showNext(implicit fab: Fabric) = println(fab.randomSentence())

    def kanarize(s: String) = {
      var trans = s
      kanaTable.foreach { rel =>
        trans = trans.replaceAll(rel._1, rel._2)
      }
      trans
    }

    def kanarizeAll(s: String) = s.split("\\s").map(kanarize).mkString(" ")

    def randomKana(implicit fab: Fabric) = {
      val (eng, fiction) = random
      Seq(eng, fiction, kanarizeAll(fiction)).mkString("\n")
    }
  }
}