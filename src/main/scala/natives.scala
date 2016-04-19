package trollish
package natives

import scala.scalajs.js
import js.annotation.ScalaJSDefined

@ScalaJSDefined
class Corpus(val expr: String, val appeared: Int, val started: Int,
             val ended: Int) extends js.Object
