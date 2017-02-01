package a14e.collz.mut

/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class IntMapSpec extends WordSpec with Matchers {
  "Tests for IntMap " when {

    "valid size" should {
      "empty" in {
        IntMap[Int]().isEmpty shouldBe true
        IntMap[Int](1 -> 1).isEmpty shouldBe false
        IntMap[Int](1 -> 2, 2 -> 2).isEmpty shouldBe false


      }

      "single element" in {

        IntMap[Int]().size shouldBe 0
        IntMap[Int](1 -> 1).size shouldBe 1
        IntMap[Int](2 -> 2).size shouldBe 1
        IntMap[Int](0xFF -> 2).size shouldBe 1
        IntMap[Int](0xFFF -> 2).size shouldBe 1
      }

      "some element" in {

        IntMap[Int]().size shouldBe 0
        IntMap[Int](1 -> 1, 2 -> 3).size shouldBe 2
        IntMap[Int](2 -> 2, 3 -> 3).size shouldBe 2
        IntMap[Int](0xFF -> 2, 0xFF1 -> 2).size shouldBe 2
        IntMap[Int](0xFFF -> 2, 0xFF2 -> 2).size shouldBe 2
      }

      "nested elements" in {
        IntMap[Int](0x1F -> 2, 0x2F -> 2).size shouldBe 2
        IntMap[Int](0x1F -> 2, 0x2F -> 2, 0x3F -> 2).size shouldBe 3
        IntMap[Int](0xFFF -> 2, 0x2F -> 2, 0x3F -> 2, 0x1FFF -> 2).size shouldBe 4
        IntMap[Int](0xFFF -> 2, 0x2F -> 2, 0x3F -> 2, 0x1FFF -> 2, 0xF1FFF -> 2).size shouldBe 5
      }
    }

    def randWithout(invalid: Int): Int = {
      var rand = Random.nextInt()
      while (rand == invalid)
        rand = Random.nextInt()
      rand
    }

    def randWithoutSet(invalid: Set[Int]): Int = {
      var rand = Random.nextInt()
      while (invalid.contains(rand))
        rand = Random.nextInt()
      rand
    }

    "contains" should {
      "empty" in {
        IntMap[Int]().contains(1) shouldBe false
      }

      "single element" in {

        IntMap[Int](1 -> 1).contains(1) shouldBe true
        IntMap[Int](1 -> 1).contains(randWithout(1)) shouldBe false


        IntMap[Int](2 -> 1).contains(2) shouldBe true
        IntMap[Int](2 -> 1).contains(randWithout(2)) shouldBe false


        IntMap[Int](0xFF -> 2).contains(0xFF) shouldBe true
        IntMap[Int](0xFF -> 1).contains(randWithout(0xFF)) shouldBe false


        IntMap[Int](0xFFF -> 2).contains(0xFFF) shouldBe true
        IntMap[Int](0xFFF -> 1).contains(randWithout(0xFFF)) shouldBe false
      }

      "some element" in {
        val values = List(1 -> 1, 2 -> 3, 4 -> 2)
        val map = IntMap[Int](values: _*)
        for ((k, _) <- values)
          map.contains(k) shouldBe true

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }

      }

      "nested elements" in {
        val values = List(0xFFF -> 2, 0x2F -> 2, 0x3F -> 2, 0x1FFF -> 2, 0xF1FFF -> 2)
        val map = IntMap[Int](values: _*)
        for ((k, _) <- values) {
          map.contains(k) shouldBe true
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }

      "a lot of random numbers" in {
        val values = List.fill(100)(Random.nextInt() -> 1)
        val map = IntMap[Int](values: _*)
        for ((k, _) <- values) {
          map.contains(k) shouldBe true
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }
    }

    "update" when {
      "single value" in {
        (IntMap[Int]() += (1 -> 1)).contains(1) shouldBe true
        (IntMap[Int]() += (1 -> 1)).contains(randWithout(1)) shouldBe false


        (IntMap[Int]() += (2 -> 1)).contains(2) shouldBe true
        (IntMap[Int]() += (2 -> 1)).contains(randWithout(2)) shouldBe false


        (IntMap[Int]() += (0xFF -> 1)).contains(0xFF) shouldBe true
        (IntMap[Int]() += (0xFF -> 1)).contains(randWithout(0xFF)) shouldBe false


        (IntMap[Int]() += (0xFFF -> 1)).contains(0xFFF) shouldBe true
        (IntMap[Int]() += (0xFFF -> 1)).contains(randWithout(0xFFF)) shouldBe false
      }

      "duplicates" in {
        val values = List(1 -> 1, 2 -> 3, 4 -> 2, 1 -> 2)
        val map = IntMap[Int]() ++= values
        map.size shouldBe 3
        for ((k, _) <- values.tail)
          map.contains(k) shouldBe true

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }

      "some values" in {
        val values = List(1 -> 1, 2 -> 3, 4 -> 2)
        val map = IntMap[Int]() ++= values
        for ((k, _) <- values)
          map.contains(k) shouldBe true

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }
      "nested elements" in {
        val values = List(0xFFF -> 2, 0x2F -> 2, 0x3F -> 2, 0x1FFF -> 2, 0xF1FFF -> 2)
        val map = IntMap[Int]() ++= values
        for ((k, _) <- values) {
          map.contains(k) shouldBe true
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }
      "a lot of random elements" in {
        val values = List.fill(100)(Random.nextInt() -> 1)
        val map = IntMap[Int]() ++= values
        for ((k, _) <- values) {
          map.contains(k) shouldBe true
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.contains(k) shouldBe false
        }
      }

      "contains elements while update" in {
        val values = List.fill(100)(Random.nextInt() -> 1)
        val keys = values.map(_._1).toSet
        val map = IntMap[Int]()
        for (el@(k, _) <- values) {
          map += el
          map.contains(k) shouldBe true

          val randKey = Random.nextInt()
          if (!keys(randKey))
            map.contains(randKey) shouldBe false
        }
      }
    }

    "get" when {
      "empty" in {
        IntMap[Int]().get(1) shouldBe None
      }

      "single element" in {

        IntMap[Int](1 -> 1).get(1) shouldBe Some(1)
        IntMap[Int](1 -> 1).get(randWithout(1)) shouldBe None


        IntMap[Int](2 -> 3).get(2) shouldBe Some(3)
        IntMap[Int](2 -> 3).get(randWithout(2)) shouldBe None


        IntMap[Int](0xFF -> 5).get(0xFF) shouldBe Some(5)
        IntMap[Int](0xFF -> 5).get(randWithout(0xFF)) shouldBe None


        IntMap[Int](0xFFF -> 0xF).get(0xFFF) shouldBe Some(0xF)
        IntMap[Int](0xFFF -> 0xF).get(randWithout(0xFFF)) shouldBe None
      }

      "some element" in {
        val values = List(1 -> 1, 2 -> 3, 4 -> 2)
        val map = IntMap[Int](values: _*)
        for ((k, v) <- values)
          map.get(k) shouldBe Some(v)

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.get(k) shouldBe None
        }

      }

      "nested elements" in {
        val values = List(0xFFF -> 2, 0x2F -> 3, 0x3F -> 4, 0x1FFF -> 5, 0xF1FFF -> 6)
        val map = IntMap[Int](values: _*)
        for ((k, v) <- values) {
          map.get(k) shouldBe Some(v)
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.get(k) shouldBe None
        }
      }

      "a lot of random numbers" in {
        val values = List.fill(100)(Random.nextInt() -> 1)
        val map = IntMap[Int](values: _*)
        for ((k, v) <- values) {
          map.get(k) shouldBe Some(v)
        }

        val keys = values.map(_._1).toSet

        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            map.get(k) shouldBe None
        }
      }
    }

    "remove" when {
      "empty" in {
        val map = IntMap[Int]()
        map -= 1
        map.isEmpty shouldBe true
        map.size shouldBe 0
      }

      "single element" in {
        val map = IntMap[Int](1 -> 1)
        map -= 1
        map.isEmpty shouldBe true
        map.size shouldBe 0
      }

      "invalid remove of single element" in {
        val map = IntMap[Int](1 -> 1)
        map -= 2
        map.isEmpty shouldBe false
        map.size shouldBe 1
        map(1) shouldBe 1
      }

      "nested elements" in {
        val values = List(0xFFF -> 2, 0x2F -> 3, 0x3F -> 4, 0x1FFF -> 5, 0xF1FFF -> 6)
        val map = IntMap[Int](values: _*)
        var tail = values
        for ((_, _) <- values) {
          map.remove(tail.head._1)
          map.contains(tail.head._1) shouldBe false
          tail = tail.tail
          for ((key, value) <- tail) {
            map.contains(key) shouldBe true
            map(key) shouldBe value
          }
        }

        val keys = values.map(_._1).toSet
        val removeMap = IntMap[Int](values: _*)
        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            removeMap.remove(k) shouldBe None
          for (key <- keys)
            removeMap.contains(key) shouldBe true
        }
      }

      "random elements" in {
        val values = List.fill(20)(Random.nextInt() -> 1)
        val map = IntMap[Int](values: _*)
        var tail = values
        for ((_, _) <- values) {
          map.remove(tail.head._1)
          map.contains(tail.head._1) shouldBe false
          tail = tail.tail
          for ((key, value) <- tail) {
            map.contains(key) shouldBe true
            map(key) shouldBe value
          }
        }

        val keys = values.map(_._1).toSet
        val removeMap = IntMap[Int](values: _*)
        for (_ <- 0 to 100) {
          val k = Random.nextInt()
          if (!keys(k))
            removeMap.remove(k) shouldBe None
          for (key <- keys)
            removeMap.contains(key) shouldBe true
        }
      }

      "mixed remove and update" in {
        val testedMap = mutable.HashMap[Int, Int]()
        val map = IntMap[Int]()

        for (_ <- 0 to 50) {
          Random.nextInt(3) match {
            case 0 => // update
              val key = Random.nextInt(100)
              val value = Random.nextInt(100)
              testedMap(key) = value
              map(key) = value
              testedMap.size shouldBe map.size
              testedMap(key) shouldBe map(key)
            case 1 => // remove random int
              val key = Random.nextInt(100)
              testedMap -= key
              map -= key
              testedMap.contains(key) shouldBe false
              map.contains(key) shouldBe false
            case 2 if testedMap.nonEmpty => // remove first
              val keysArray = testedMap.keysIterator.toArray
              val keyIndex = Random.nextInt(keysArray.length)
              val key = keysArray(keyIndex)
              testedMap -= key
              map -= key
              testedMap.contains(key) shouldBe false
              map.contains(key) shouldBe false
            case _ =>
          }

          for ((k, v) <- testedMap)
            map(k) shouldBe v
        }

      }
    }

    "foreach" in {
      val elems = List.fill(100)(Random.nextInt() -> Random.nextInt())
      val elemsSet = elems.toSet

      val buffer = new ListBuffer[(Int, Int)]
      val map = IntMap[Int]() ++= elems
      map.foreach(buffer += _)

      buffer.toSet shouldBe elemsSet
    }

    "iterator" when {
      "single element" in {
        val map = IntMap[Int](1 -> 2)
        map.iterator.toList shouldBe List(1 -> 2)
      }

      "some elements" in {

        val elems = List(1 -> 2, 3 -> 4)

        val map = IntMap[Int](elems: _ *)
        map.iterator.toList.sorted shouldBe elems.sorted
      }

      "nested elements" in {
        val elems = List(0xFFF -> 2, 0x2F -> 3, 0x3F -> 4, 0x1FFF -> 5, 0xF1FFF -> 6)
        val map = IntMap[Int](elems: _ *)
        map.iterator.toList.sorted shouldBe elems.sorted
      }

      "random elements" in {

        val elems = List.fill(100)(Random.nextInt() -> Random.nextInt())

        val map = IntMap[Int](elems: _ *)
        map.iterator.toList.sorted shouldBe elems.sorted

      }
    }


    "map" in {
      import IntMap._
      val res = IntMap[Int](1 -> 1, 2 -> 2).map { case (k, v) => (k + 1) -> (v + 2) }
      res shouldBe IntMap[Int](2 -> 3, 3 -> 4)
    }


  }

}
