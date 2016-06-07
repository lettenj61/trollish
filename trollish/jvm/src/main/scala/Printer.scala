package trollish

import ammonite.ops._

trait Printer {

  var kanaTable: KanaLayer.Table = KanaConstants.default
  def loadConverter(src: Path): Unit = {
    kanaTable = KanaLayer.fromFile(src)
  }

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

  def kanarizeTo(s: String, depth: Int = kanaTable.size) = {
    var trans = s
    for (i <- 0 to depth) {
      trans = trans.replaceAll(kanaTable(i)._1, kanaTable(i)._2)
    }
    trans
  }

  def kanarizeAll(s: String) = s.split("\\s").map(kanarize).mkString(" ")

  def randomKana(implicit fab: Fabric) = {
    val (eng, fiction) = random
    Seq(eng, fiction, kanarizeAll(fiction)).mkString("\n")
  }
}