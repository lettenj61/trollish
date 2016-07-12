package trollish.demo

import scala.scalajs.js
import js.annotation._
import js.JSApp
import js.Dynamic.literal

import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.style

import org.querki.jquery.$

import trollish._

/** Demo appilication for trollish library.
  */
object DemoPage extends JSApp {

  /** Special reserve to replace specified word before executing any translation based on tones.
    */
  val wordRep = new collection.mutable.HashMap[String, String]

  implicit val fabric = Fabric.deduplicated(retry = 1024, threshold = Tones.average * 4)
  object K extends KanaPrinter

  def translateWithEscape(words: String) = {
    val translated = words.split("\\s").map { w =>
      wordRep.getOrElse(w, fabric.translate(w))
    }.mkString(" ")

    (words, translated, K.kanarizeAll(translated))
  }

  def displayNear(expr: String): String = {
    def show(tone: Tone) = {
      val soundType = if (tone.isVowel) "vowel" else "consonant"
      Seq(tone.expr, soundType, tone.appeared.toString).mkString(", ")
    }
    fabric.near(expr).map(show).mkString("\n")
  }

  val forElem = scalatags.JsDom.all.`for`

  val fromBox = input(
    id := "trollish-rep-from",
    tpe := "text",
    cls := "pure-u-2-5",
    maxlength := 12
  ).render

  val toBox = input(
    id := "trollish-rep-to", tpe := "text", cls := "pure-u-2-5", maxlength := 12).render

  // FIXME too rough
  val wordBoxSrc = input(
    id := "trollish-words-from",
    tpe := "text",
    cls := "pure-u-2-5",
    maxlength := 32
  ).render
  val wordBoxTo = input(
    id := "trollish-words-to",
    tpe := "text",
    cls := "pure-u-2-5",
    maxlength := 32
  ).render
  wordBoxSrc.ondblclick = { (_: dom.Event) =>
    val currentWordReps = wordRep.toVector.sortBy(_._1).map(x => x._1 + ", " + x._2)
    dom.window.alert(currentWordReps.mkString("\n"))
  }

  val helpView = textarea(id := "trollish-help-view", readonly, cols := 48, rows := 4).render
  val candidatesLabel = label(forElem := "trollish-help-view").render

  val updateHelpView = { (s: String) =>
    val message = s"Candidates for [$s] : appeared ${fabric.tone(s).map(_.appeared).getOrElse(0)}"
    val currentRep = s"(current replacement ${fabric.get(s).onBody})"
    candidatesLabel.textContent = message + " " + currentRep
    helpView.value = displayNear(s)
  }

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
            ondblclick := { (_: dom.Event) => updateHelpView(tone) },
            tone
          ),
          td(id := s"hist-rep-$tone", rep)
        )
      }
    )
  ).render

  fromBox.onkeyup = { (e: dom.Event) => updateHelpView(fromBox.value) }

  val adder = button(tpe := "button", cls := "pure-button",
    onclick := { (_: dom.Event) =>
      fabric.rep(fromBox.value, toBox.value)
      val index = history.indexWhere(_._1 == fromBox.value)
      if (index != -1) {
        $(s"td#hist-rep-${fromBox.value}")
          .text(toBox.value)
          .css("background-color", "lightblue")
      }
      updateHelpView(fromBox.value)
    })("Add").render

  val wordAdder = button(tpe := "button", cls := "pure-button",
    onclick := { (_: dom.Event) =>
      wordRep.put(wordBoxSrc.value, wordBoxTo.value)
    })("Add Word").render

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
    width := 418.px,
    maxlength := 48
  ).render

  val translator = button(
    tpe := "button",
    cls := "pure-button pure-button-primary",
    onclick := { (_: dom.Event) =>
      val (eng, fiction, kana) = translateWithEscape(translation.value)
      val answer =
        if (displayKana.checked) Seq(eng, fiction, kana).mkString("\n")
        else s"$eng\n$fiction"

      screen.value = answer
    })("Translate").render

  val randomizer = button(tpe := "button", cls := "pure-button")("Random").render
  randomizer.onclick = (_: dom.Event) => {
    val (e, _) = fabric.randomSentence()
    val (_, f, k) = translateWithEscape(e)
    screen.value =
      if (displayKana.checked) Seq(e, f, k).mkString("\n")
      else Seq(e, f).mkString("\n")
  }

  val downloadLink = a(href := "#", "Download").render
  downloadLink.onclick = { (_: dom.Event) =>
    val blob = new dom.Blob(js.Array(fabric.prettyPrint(4)), dom.raw.BlobPropertyBag("text/plain"))
    val url = js.Dynamic.global.URL.createObjectURL(blob).asInstanceOf[String]
    dom.window.open(url)
  }

  def main(): Unit = {

    val wrapper = dom.document.getElementById("wrapper").asInstanceOf[dom.html.Div]
    val leftPage = div(fromBox, toBox, adder).render
    val leftMiddle = div(wordBoxSrc, wordBoxTo, wordAdder).render
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
      p(paddingLeft := 12.px)(randomizer, downloadLink),
      p(candidatesLabel, br, helpView)
    ).render

    wrapper.appendChild(
      div(
        width := "100%",
        div(
          id := "content-left",
          cls := "pure-u-2-3",
          form(cls := "pure-form", leftPage, leftMiddle, leftBottom)
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
