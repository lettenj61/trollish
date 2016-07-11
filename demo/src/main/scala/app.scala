package trollish.demo

import scala.scalajs.js
import js.annotation._
import js.JSApp

import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.style

import org.querki.jquery.$

import trollish._

/** Demo appilication for trollish library.
  */
object DemoPage extends JSApp {

  implicit val fabric = Fabric.deduplicated()
  object K extends KanaPrinter

  def displayNear(expr: String): String = {
    def show(tone: Tone) = {
      val soundType = if (tone.isVowel) "vowel" else "consonant"
      Seq(tone.expr, soundType, tone.appeared.toString).mkString(", ")
    }
    fabric.near(expr).map(show).mkString("\n")
  }

  val forElem = scalatags.JsDom.all.`for`

  val fromBox = input(
    id := "trollish-rep-from", tpe := "text", cls := "pure-u-2-5", maxlength := 12).render
  val toBox = input(
    id := "trollish-rep-to", tpe := "text", cls := "pure-u-2-5", maxlength := 12).render

  val candidates = textarea(id := "trollish-help-view", readonly, cols := 48, rows := 4).render
  val candidatesLabel = label(forElem := "trollish-help-view").render

  private lazy val history = {
    val fabricBase = fabric.mapper.replacements.toList.map(r => (r._1, r._2.onBody))
    collection.mutable.Buffer.empty[(String, String)] ++= fabricBase.sortBy(_._1)
  }
  val showHistory = div(
    width := "100%",
    cls := "pure-u-1-2",
    table(
      id := "trollish-rep-history",
      width := "100%",
      cls := "pure-table",
      tr(
        th("From"),
        th("To")
      ),
      for (i <- history.indices) yield {
        val (tone, rep) = history(i)
        tr(
          id := s"history-elem-$i",
          td(
            id := s"hist-tone-$tone",
            onclick := {(_: dom.Event) =>
              candidatesLabel.textContent =
                s"Candidates for [$tone] : appeared ${fabric.tone(tone).map(_.appeared).getOrElse(0)}"
              candidates.value = displayNear(tone)
            },
            tone
          ),
          td(id := s"hist-rep-$tone", rep)
        )
      }
    )
  ).render

  val adder = button(tpe := "button", cls := "pure-button",
    onclick := { (_: dom.Event) =>
      fabric.rep(fromBox.value, toBox.value)
      val index = history.indexWhere(_._1 == fromBox.value)
      if (index != -1) {
        $(s"td#hist-rep-${fromBox.value}")
          .text(toBox.value)
          .css("background-color", "lightblue")
      }
    })("Add").render

  val screen = textarea(
    id := "trollish-app-screen",
    cls := "pure-input-1",
    cols := 80,
    rows := 16,
    readonly
  ).render

  val displayKana = input(
    tpe := "checkbox",
    id := "trollish-demo-kana-flag",
    marginTop := 8.px
  ).render
  val translation = input(
    id := "trollish-app-translation",
    tpe := "text",
    maxlength := 48
  ).render

  val translator = button(
    tpe := "button",
    cls := "pure-button pure-button-primary",
    onclick := { (_: dom.Event) =>
      val (eng, fiction) = fabric.showSentence(translation.value)
      val answer =
        if (displayKana.checked) Seq(eng, fiction, K.kanarizeAll(fiction)).mkString("\n")
        else s"$eng\n$fiction"

      screen.value = answer
    })("Translate").render

  val randomizer = button(tpe := "button", cls := "pure-button")("Random").render
  randomizer.onclick = (_: dom.Event) => {
    val (e, f, k) = K.randomKana
    screen.value =
      if (displayKana.checked) Seq(e, f, k).mkString("\n")
      else Seq(e, f).mkString("\n")
  }

  def main(): Unit = {

    val wrapper = dom.document.getElementById("wrapper").asInstanceOf[dom.html.Div]
    val leftPage = div(fromBox, toBox, adder).render
    val translationSection = div(
      margin := "12px",
      translation,
      translator,
      br,
      label(
        forElem := "trollish-demo-kana-flag",
        displayKana,
        "Display katakanas"
      )
    ).render
    val leftBottom = div(
      screen,
      hr.render,
      translationSection,
      randomizer,
      p(candidatesLabel, br, candidates)
    ).render

    wrapper.appendChild(
      div(
        width := "100%",
        div(
          id := "content-left",
          cls := "pure-u-2-3",
          form(cls := "pure-form", leftPage, leftBottom)
        ),
        div(
          id := "content-right",
          cls := "pure-u-1-3",
          overflow := "scroll",
          showHistory
        )
      ).render
    )

    $(dom.document).ready { () =>
      $("div#content-right").css("height", $(dom.window).height.toString + "px")
    }
  }
}
