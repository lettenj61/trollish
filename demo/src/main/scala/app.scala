package trollish.demo

import scala.scalajs.js
import js.annotation._
import js.JSApp

import org.scalajs.dom
import scalatags.JsDom.all._

import trollish._

/** Demo appilication for trollish library.
  */
object DemoPage extends JSApp {

  implicit val fabric = Fabric.deduplicated()
  object K extends KanaPrinter

  val fromBox = input(id := "trollish-rep-from", `type` := "text", cls := "pure-u-2-5", maxlength := 12).render
  val toBox = input(id := "trollish-rep-to", `type` := "text", cls := "pure-u-2-5", maxlength := 12).render

  val adder = button(`type` := "button", cls := "pure-button")("Add").render
  adder.onclick = (_: dom.Event) => fabric.rep(fromBox.value, toBox.value)

  val screen = textarea(
    id := "trollish-app-screen",
    cls := "pure-input-1",
    cols := 80,
    rows := 24,
    readonly := true
  ).render

  val translation = input(
    id := "trollish-app-translation",
    `type` := "text",
    maxlength := 48
  ).render
  val translator = button(`type` := "button", cls := "pure-button pure-button-primary")("Translate").render
  translator.onclick = (_: dom.Event) => {
    val (eng, fiction) = fabric.showSentence(translation.value)
    screen.value = Seq(eng, fiction, K.kanarizeAll(fiction)).mkString("\n")
  }

  val randomizer = button(`type` := "button", cls := "pure-button")("Random").render
  randomizer.onclick = (_: dom.Event) => {
    val (e, f, k) = K.randomKana
    screen.value = Seq(e, f, k).mkString("\n")
  }

  def main(): Unit = {

    val wrapper = dom.document.getElementById("wrapper").asInstanceOf[dom.html.Div]
    val top = div(margin := "12px", cls := "pure-u-1")(fromBox, toBox, adder).render

    val translationSection = p(margin := "12px")(translation, translator).render
    val bottom = div(screen, hr.render, translationSection, randomizer).render
    wrapper.appendChild(
      form(cls := "pure-form", top, bottom).render
    )
  }
}
