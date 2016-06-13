package trollish

trait KanaPrinter {

  type Entry = (String, String)
  type Table = Seq[Entry]

  var kanaTable: Table = KanaConstants.default

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
    (eng, fiction, kanarizeAll(fiction))
  }

  def printKana(implicit fab: Fabric) = {
    val (eng, fiction, kana) = randomKana(fab)
    println(eng); println(fiction); println(kana)
  }
}