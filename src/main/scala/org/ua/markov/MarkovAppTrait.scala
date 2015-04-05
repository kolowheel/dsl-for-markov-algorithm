package org.ua.markov

import org.apache.commons.lang3.{StringUtils, StringEscapeUtils}

import scala.collection.mutable.ListBuffer

/**
 * @author yaroslav.gryniuk
 */
trait MarkovAppTrait /*extends App*/ {

  case class MarkovToken(left: String, right: String, end: Boolean = false)

  implicit class MarkovString(string: String)(implicit buf: ListBuffer[MarkovToken]) {
    def ->(that: String) = substitutions += MarkovToken(StringEscapeUtils.escapeJava(string), StringEscapeUtils.escapeJava(that))

    def ->(that: EndStr) = substitutions += MarkovToken(StringEscapeUtils.escapeJava(string), StringEscapeUtils.escapeJava(that.string), true)
  }

  case class EndStr(string: String)

  implicit class StrToEndStr(string: String) {
    def unary_!() = EndStr(string)
  }

  object alphabet {
    def set(alphabet: String) = alphabetStr = alphabet

    def >>(s: String => Unit) = s(alphabetStr)
  }

  var alphabetStr = ""

  implicit val substitutions = ListBuffer.empty[MarkovToken]

  def solve(input: String): String = {
    val takeSubs = (input: String, markovs: List[MarkovToken]) => markovs find (x => input.contains(x.left))
    val markovList = substitutions.toList
    val token = takeSubs(input, markovList)
    val replace = (where: String, what: String, on: String) => StringUtils.replaceOnce(where, what, on)
    val isEnd = (token: Option[MarkovToken]) => token.isEmpty || token.get.end

    lazy val calculate: Stream[Option[String]] = Some(replace(input, token.get.left, token.get.right)) #:: calculate.map({
      case Some(str) =>
        val token = takeSubs(str, markovList)
        if (isEnd(token))
          Option.empty[String]
        else
          Some(replace(str, token.get.left, token.get.right))
      case None =>
        None
    })

    calculate.takeWhile(_.isDefined).last.get
  }

  def refresh: Unit = substitutions.clear()

}
