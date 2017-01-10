/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}


class PrefixSetSpec extends WordSpec with Matchers {
  /**
    * этот тест декоративный,
    * сделан для повышения test coverage,
    * основную массу тестов см PrefixMap
    **/
  "Tests for PrefixSet " when {

    "add" in {
      val set = PrefixSet("a", "b", "c")
      set += "aa"
      set.size shouldBe 4
    }

    "remove" in {
      val set = PrefixSet("a", "b", "c")
      set -= "a"
      set.size shouldBe 2
    }

    "iterator" in {
      PrefixSet("a", "b", "c").iterator.toList.sorted shouldBe List("a", "b", "c")
    }

    "foreach" in {
      var sum = 0
      PrefixSet("a", "bb", "ccc").foreach(x => sum += x.length)
      sum shouldBe 6
    }

    "contains" in {
      val set = PrefixSet("a", "bb", "ccc")
      set.contains("bb") shouldBe true
    }

    "has prefix" in {
      val set = PrefixSet("ab", "bb", "ccc")
      set.hasPrefix("a") shouldBe true
    }

    "find for prefix" in {
      val set = PrefixSet("ab", "aaa", "bb", "ccc")
      set.findForPrefix("a").toList.sorted shouldBe List("aaa", "ab")
    }

    "clear" in {
      val set = PrefixSet("ab", "aaa", "bb", "ccc")
      set.clear()
      set.size shouldBe 0
    }
  }

}
