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
  var _fabric = Fabric.deduplicated(retry = 144 * 4, threshold = Tones.average * 4)

  implicit def fabric = _fabric
  object K extends KanaPrinter

  def translateWithEscape(words: String) = {
    val translated = words.split("\\s").map { w =>
      wordRep.getOrElse(w, fabric.translate(w))
    }.mkString(" ")

    (translated, K.kanarizeAll(translated))
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
    maxlength := 12,
    placeholder := "Old tone"
  ).render

  val toHeadBox = input(id := "trollish-rep-to-head",
    tpe := "text",
    maxlength := 12,
    placeholder := "Put new tone here!").render
  val toBodyBox = input(id := "trollish-rep-to-body", tpe := "text", maxlength := 12).render
  val toTailBox = input(id := "trollish-rep-to-tail", tpe := "text", maxlength := 12).render
  val repBoxes = span(toHeadBox, toBodyBox, toTailBox).render
  toHeadBox.onkeyup = { (_: dom.Event) =>
    if (toHeadBox.value.nonEmpty) {
      toBodyBox.placeholder = toHeadBox.value
      toTailBox.placeholder = toHeadBox.value
    } else {
      toHeadBox.placeholder = "Put new tone here!"
      toBodyBox.placeholder = ""
      toTailBox.placeholder = ""
    }
  }

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

  def updateHelpView(s: String): Unit = {
    val message = s"Candidates for [$s] : appeared ${fabric.tone(s).map(_.appeared).getOrElse(0)}"
    val currentRep = s"(current replacement ${fabric.get(s)})"
    candidatesLabel.textContent = message + " " + currentRep
    helpView.value = displayNear(s)
  }

  def history: Seq[(String, String)] = {
    fabric.mapper.replacements.map { case (k, r) => (k, r.toString) }.toBuffer.sortBy( (x: (String, String)) => x._1)
  }

  def createRepViewRow(tone: String, rep: String) = tr(
    id := s"history-elem-$i",
    td(
      id := s"hist-tone-$tone",
      ondblclick := { () => updateHelpView(tone) },
      tone
    ),
    td(id := s"hist-rep-$tone", rep)
  )

  def createRepViewFrag = for (i <- history.indices) yield {
    val (tone, rep) = history(i)
    createRepViewRow(tone, rep)
  }

  val replacementsView = div(
    width := "100%",
    cls := "pure-u-1-2",
    table(
      id := "trollish-rep-history",
      width := "100%",
      cls := "pure-table",
      tr(
        id := "trollish-rep-header",
        th("From"),
        th("To")
      ),
      createRepViewFrag
    )
  ).render

  fromBox.onkeyup = { (e: dom.Event) =>
    if (fromBox.value.isEmpty && toHeadBox.value.isEmpty) {
      toHeadBox.placeholder = "Put new tone here!"
      toBodyBox.placeholder = ""
      toTailBox.placeholder = ""
      
    } else if (toHeadBox.value.isEmpty) {
      toHeadBox.placeholder = fabric.get(fromBox.value).onHead
      toBodyBox.placeholder = fabric.get(fromBox.value).onBody
      toTailBox.placeholder = fabric.get(fromBox.value).onTail
    }
    updateHelpView(fromBox.value)
  }

  val adder = button(tpe := "button", cls := "pure-button",
    onclick := { (_: dom.Event) =>
      val repOnBody = if (toBodyBox.value.isEmpty) toHeadBox.value else toBodyBox.value
      val repOnTail = if (toTailBox.value.isEmpty) toHeadBox.value else toTailBox.value
      if (toHeadBox.value.nonEmpty) {
        fabric.rep(fromBox.value, toHeadBox.value, repOnBody, repOnTail)
        val index = history.indexWhere(_._1 == fromBox.value)
        if (index != -1) {
          $(s"td#hist-rep-${fromBox.value}")
            .text(fabric.get(fromBox.value).toString)
            .css("background-color", "lightblue")
        } else {
          $(createRepViewRow(fromBox.value, fabric.get(fromBox.value).toString).render)
            .insertAfter("tr#trollish-rep-header")
        }
      } else dom.window.alert(s"Tone replacement not specified!")
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
    rows := 9,
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
      val (fiction, kana) = translateWithEscape(translation.value)
      screen.value =
        if (displayKana.checked) Seq(translation.value, fiction, kana).mkString("\n")
        else s"${translation.value}\n$fiction"
    })("Translate").render

  val randomizer = button(tpe := "button", cls := "pure-button")("Random").render
  randomizer.onclick = (_: dom.Event) => {
    val (e, _) = fabric.randomSentence()
    val (f, k) = translateWithEscape(e)
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

  def updateRepView() = {
    $("table#trollish-rep-history").empty()
    $("table#trollish-rep-history").append($(createRepViewFrag.render))
  }

  val resetButton = button(
    cls := "pure-button",
    onclick := { () =>
      _fabric = _fabric.shuffle(retry = 99)
      updateRepView()
    },
    "Reset"
  ).render

  val mapOnlyVowelsButton = button(
    cls := "pure-button",
    onclick := { () =>
      _fabric = Fabric.deduplicated(tones = Tones.vowels)
      updateRepView()
    },
    "Vowels Only"
  ).render

  def main(): Unit = {

    val wrapper = dom.document.getElementById("wrapper").asInstanceOf[dom.html.Div]
    val leftPage = div(fromBox, p(repBoxes, adder)).render
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
      p(candidatesLabel, br, helpView),
      p(resetButton, mapOnlyVowelsButton)
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
          replacementsView
        )
      ).render
    )

    $(dom.document).ready { () =>
      $("div#content-right").css("height", $(dom.window).height.toString + "px")
    }
  }
}
