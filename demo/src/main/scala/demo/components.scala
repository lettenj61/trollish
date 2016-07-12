package trollish.demo

import scala.scalajs.js

import org.scalajs.dom
import scalatags.JsDom.all._

object Components {

  /** Simple 2-column table with functions to search containments. */
  def SearchTable(idString: String, className: Option[String] = None)
                 (header: (String, String))
                 (data: Map[String, String]): HtmlTag = {
    tmpl(idString, className)(
      table(
        id := s"table-$idString",
        tr(th(header._1), th(header._2)),
        for (kv <- data.toVector) yield {
          tr(
            td(id := s"table-elem-key-$idString")(kv._1),
            td(id := s"table-elem-value-$idString")(kv._2)
          )
        }
      )
    )
  }

  def tmpl(idString: String, className: Option[String]) = className match {
    case None     => div(id := idString)
    case Some(x)  => div(id := idString, cls := x)
  }
}
