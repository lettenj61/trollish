package trollish.shipyard
package jmdict

import ammonite.ops._

trait CsvProcess {
  type Channel = (Path, Path)
  def delim = "\t"

  def process[A](src: Path, dest: Option[Path] = None)(f: Channel => A): A = {

    val target = dest getOrElse tmp()
    f(src, target)
  }
}