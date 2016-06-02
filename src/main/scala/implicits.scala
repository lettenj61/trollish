package trollish

import scala.language.implicitConversions

object Implicits {

  implicit def string2DefaultTone(s: String) = Tones.default.getOrElse(s, Tone.unknown)
}