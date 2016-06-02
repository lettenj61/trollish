package trollish

import scala.language.implicitConversions

object Implicits {

  def string2DefaultTone(s: String) = Tones.default.getOrElse(s, Tone.unknown)
}