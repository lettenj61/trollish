package trollish

import ammonite.ops
import upickle.{ default => ud }

object KanaLayer {

  type Table = Seq[(String, String)]
  def parse(source: String): Table = ud.read[Table](source)

  def writeFile(table: Table, path: ops.Path): Unit = {
    ops.write.over(path, ud.write(table, indent = 2))
  }

  def fromFile(path: ops.Path): Table = parse(ops.read(path))
}
