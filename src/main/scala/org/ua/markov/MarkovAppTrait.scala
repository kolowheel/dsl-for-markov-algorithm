package org.ua.markov

import org.apache.commons.lang3.{StringUtils, StringEscapeUtils}

import scala.collection.mutable.ListBuffer

/**
 * @author yaroslav.gryniuk
 */
trait MarkovAppTrait /*extends App*/ {

  case class MarkovToken(left: String, right: String)

  implicit class MarkovString(string: String)(implicit buf: ListBuffer[MarkovToken]) {
    def ->(that: String) = markovs += MarkovToken(StringEscapeUtils.escapeJava(string), StringEscapeUtils.escapeJava(that))
  }

  implicit class endStr(string: String) {
    def unary_!() = {
      end set string
      string
    }
  }


  object end {
    def set(end: String) = endStr = end

    def apply(s: String => Unit) = s(endStr)
  }

  object alphabet {
    def set(alphabet: String) = alphabetStr = alphabet

    def >>(s: String => Unit) = s(alphabetStr)
  }


  val Token_Stub = MarkovToken("", "")
  var alphabetStr = ""

  var endStr = "$"
  val empty = " "
  implicit val markovs = ListBuffer.empty[MarkovToken]

  def solve(input: String): String = {
    def findSubstitution(input: String, markovs: List[MarkovToken]): Option[MarkovToken] = {
      if (markovs.nonEmpty) {
        if (input.contains(markovs.head.left))
          Some(markovs.head)
        else
          findSubstitution(input, markovs.tail)
      } else {
        None
      }
    }
    val markovList = markovs.toList

    val firstToken = findSubstitution(input, markovs.toList)
    if (firstToken.isEmpty) {
      input
    } else {
      var buffer = input.replaceFirst(firstToken.get.left, firstToken.get.right)
      var token = firstToken
      var anySubs = true
      while (token.isDefined && !(token.get.right contains endStr) && anySubs) {
        val nextSubs = findSubstitution(buffer, markovList)
        anySubs = nextSubs.isDefined
        val subs = nextSubs getOrElse Token_Stub
        buffer = StringUtils.replaceOnce(buffer, subs.left, subs.right)
        token = nextSubs
      }
      buffer
    }
  }

  def refresh: Unit = markovs.clear()

}
