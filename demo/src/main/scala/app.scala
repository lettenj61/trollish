package trollish.demo

import scala.scalajs.js
import js.annotation._
import js.JSApp

import org.scalajs.dom
import scalatags.JsDom.all._

import trollish._

object DemoPage extends JSApp {

  implicit val fabric = Fabric.deduplicated()
  object K extends KanaPrinter

  val fromBox = input(id := "trollish-rep-from", `type` := "text", maxlength := 12).render
  val toBox = input(id := "trollish-rep-to", `type` := "text", maxlength := 12).render

  val adder = input(`type` := "button", value := "Add").render
  adder.onclick = (_: dom.Event) => fabric.rep(fromBox.value, toBox.value)

  val screen = textarea(id := "trollish-app-screen", cols := 80, rows := 24, readonly := true).render
  val randomizer = input(`type` := "button", value := "Random").render
  randomizer.onclick = (_: dom.Event) => {
    val (e, f, k) = K.randomKana
    screen.value = Seq(e, f, k).mkString("\n")
  }

  def main(): Unit = {

    val wrapper = dom.document.getElementById("wrapper").asInstanceOf[dom.html.Div]
    val top = div(margin := "8px")(fromBox, toBox, adder).render
    val bottom = div(screen, hr.render, randomizer).render
    wrapper.appendChild(
      div(top, bottom).render
    )
  }
}