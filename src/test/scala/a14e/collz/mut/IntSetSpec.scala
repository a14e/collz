package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}


class IntSetSpec extends WordSpec with Matchers {

  /**
    * этот тест декоративный,
  * сделан для повышения test coverage,
  * основную массу тестов см IntMap
  * */

  "Tests for IntSet " when {

    "add" in {
      val set = IntSet(1, 2, 3)
      set += 4
      set.size shouldBe 4
    }

    "remove" in {
      val set = IntSet(1, 2, 3)
      set -= 2
      set.size shouldBe 2
    }

    "iterator" in {
      IntSet(1, 2, 3).iterator.toList.sorted shouldBe List(1, 2, 3)
    }

    "foreach" in {
      var sum = 0
      IntSet(1, 2, 3).foreach(sum += _)
      sum shouldBe 6
    }

    "contains" in {
      val set = IntSet(1, 2, 3)
      set.contains(1) shouldBe true
    }
  }

}
