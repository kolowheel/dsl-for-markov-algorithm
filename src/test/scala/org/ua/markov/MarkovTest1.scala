package org.ua.markov

import org.scalatest.FlatSpec

/**
 * @author yaroslav.gryniuk
 */
class MarkovTest1 extends FlatSpec {
  "" should "calculate properly" in {
    var test:String = ""
    object Test extends MarkovAppTrait {

      "A" -> "apple"
      "B" -> "bag"
      "S" -> "shop"
      "T" -> "the"
      "the shop" -> "my brother"
      "a never used" -> !"terminating rule"


      test = solve("I bought a B of As from T S.")

    }
    assertResult(test)("I bought a bag of apples from my brother.")

  }
}
