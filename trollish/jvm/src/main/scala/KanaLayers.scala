package trollish

import ammonite.ops

object KanaLayer {

  type Table = Seq[(String, String)]
  def parse(source: String): Table = {
    import upickle.default._
    read[Table](source)
  }

  def writeFile(table: Table, path: ops.Path): Unit = {
    ops.write.over(path, upickle.default.write(table))
  }
}
