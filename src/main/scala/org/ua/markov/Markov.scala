package org.ua.markov


/**
 * @author yaroslav.gryniuk
 */
object Markov extends  App with MarkovAppTrait {


  "A" -> "apple"
  "B" -> "bag"
  "S" -> "shop"
  "T" -> "the"
  "the shop" -> "my brother"
  "a never used" -> !"terminating rule"


  println(solve("I bought a B of As from T S."))


}
