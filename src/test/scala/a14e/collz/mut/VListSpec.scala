package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Andrew on 14.05.2016.
  */

class VListSpec extends WordSpec with Matchers {
  "Tests for VList " when {

    "to list" in {
      VList(1, 2, 3).toList shouldBe List(1, 2, 3)
      VList().toList shouldBe Nil
      VList(1 to 10: _*).toList shouldBe (1 to 10).toList
    }

    //TODO сделать тесты презентабельнее
    "test for constructing, size, removing and indexes " in {

      VList[Unit]() shouldBe empty
      VList(1) should not be empty

      VList(1, 2, 3, 4, 5).length shouldEqual 5

      VList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) shouldEqual (1 to 10)

      val list = new VList(Array(1, 2, 3, 4, 5))
      for ((x, i) <- Array(1, 2, 3, 4, 5).zipWithIndex)
        list(i) shouldEqual x

      list.prepend(0)
      list.prepend(-1)
      -2 +=: list
      -3 +=: list

      for ((x, i) <- Array(-3, -2, -1, 0, 1, 2, 3, 4, 5).zipWithIndex)
        list(i) shouldEqual x


      val list2 = VList(1, 2, 3, 4)
      for (i <- 0 until 4)
        list2(i) = 0

      for (i <- 0 until 4)
        list2(i) shouldEqual 0

      list2 ++= List(1, 2, 3)

      list2 shouldEqual List(0, 0, 0, 0, 1, 2, 3)

      List(1, 2, 3) ++=: list2

      list2 shouldEqual List(1, 2, 3, 0, 0, 0, 0, 1, 2, 3)

      list2.append(2, 2)
      list2 += 3

      list2 shouldEqual List(1, 2, 3, 0, 0, 0, 0, 1, 2, 3, 2, 2, 3)


      list2.prepend(2, 2)

      list2 shouldEqual List(2, 2, 1, 2, 3, 0, 0, 0, 0, 1, 2, 3, 2, 2, 3)

      VList(1, 2, 3, 4, 5).toArray shouldEqual Array(1, 2, 3, 4, 5)
      //      VList(5, 4, 3, 2, 1).toReverseArray shouldEqual Array(1, 2, 3, 4, 5)


      VList(1, 2, 3).toString() shouldEqual "VList(1, 2, 3)"
      VList(1, 2, 3).map(identity).toString() shouldEqual "VList(1, 2, 3)"

      val builder = VList.newBuilder[Int]
      builder ++= VList(1, 2, 3)
      builder.result() shouldEqual VList(1, 2, 3)
      builder.clear()
      builder.result() shouldBe empty


      VList(1, 2, 3) should not be Array(2, 3, 4)
    }

    "tests for insert all and remove" in {
      val list = VList(-3, -2, -1, 0, 1, 2, 3, 4, 5)
      list.append(6)
      list.append(7, 8)
      for ((x, i) <- Array(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8).zipWithIndex)
        list(i) shouldEqual x

      list.remove(9) shouldEqual 6
      list should have length 11
      list shouldEqual Array(-3, -2, -1, 0, 1, 2, 3, 4, 5, 7, 8)

      list.remove(9) shouldEqual 7
      list should have length 10
      list.remove(9) shouldEqual 8
      list should have length 9
      list.remove(0) shouldEqual -3
      list should have length 8

      for ((x, i) <- Array(-2, -1, 0, 1, 2, 3, 4, 5).zipWithIndex)
        list(i) shouldEqual x

      list.remove(3) shouldEqual 1
      list should have length 7
      list shouldEqual Array(-2, -1, 0, 2, 3, 4, 5)

      list.clear()
      list.length shouldEqual 0
      list shouldBe empty


      val toInsertList = VList(1, 2, 3, 6, 7, 8)
      toInsertList.insertAll(3, Array(4, 5))
      toInsertList shouldEqual (1 to 8)

      toInsertList.clear()
      toInsertList ++= VList(1, 2, 3, 8)
      toInsertList.insertAll(3, List(4, 5, 6, 7))
      toInsertList shouldEqual (1 to 8)

      toInsertList.clear()
      toInsertList ++= VList(1, 2, 3, 4, 9)
      toInsertList.insertAll(4, List(5, 6, 7, 8))
      toInsertList should have length 9
      toInsertList shouldEqual (1 to 9)


      toInsertList.clear()
      toInsertList += 3
      toInsertList.insertAll(0, List(4, 5, 6, 7))
      toInsertList should have length 5
      toInsertList shouldEqual List(4, 5, 6, 7, 3)

      toInsertList.clear()
      toInsertList += 3
      toInsertList.insertAll(1, List(4, 5, 6, 7))
      toInsertList should have length 5
      toInsertList shouldEqual List(3, 4, 5, 6, 7)

      toInsertList.clear()
      toInsertList.insertAll(0, List(4, 5, 6, 7))
      toInsertList should have length 4
      toInsertList shouldEqual List(4, 5, 6, 7)

      toInsertList.clear()
      toInsertList ++= List(1, 2, 3)
      toInsertList.insertAll(3, List(4, 5, 6, 7))
      toInsertList should have length 7
      toInsertList shouldEqual (1 to 7)

    }

    "test for iterators " in {

      VList(1, 2, 3).iterator should have length 3
      VList(1, 2, 3).iterator.sum shouldEqual 6
      VList(1, 2, 3).iterator.reduceLeft(_ - _) shouldEqual -4
      VList(1, 2, 3).iterator.reduceRight((x, temp) => temp - x) shouldEqual 0

      VList(1, 2, 3).reverseIterator.filter(_ % 2 == 0) should have length 1

      VList(1, 2, 3).reverseIterator should have length 3
      VList(1, 2, 3).reverseIterator.sum shouldEqual 6
      VList(1, 2, 3).reverseIterator.reduceLeft(_ - _) shouldEqual 0
      VList(1, 2, 3).reverseIterator.reduceRight((x, temp) => temp - x) shouldEqual -4

      VList(1, 2, 3).reverseIterator.filter(_ % 2 != 0) should have length 2
    }

    "test for functional api  " in {
      VList(1, 2, 3).head shouldEqual 1
      VList(6, 7, 8, 9, 1, 2, 3).head shouldEqual 6

      VList(1, 2, 3).last shouldEqual 3
      VList(1, 2).last shouldEqual 2


      VList(1, 2, 3).tail shouldEqual VList(2, 3)

      VList(1, 2).tail shouldEqual VList(2)

      VList(1, 2, 3).map(_ * 2) shouldEqual VList(2, 4, 6)

      val test = 1 to 5
      var i = 0
      for (x <- VList(1, 2, 3, 4, 5)) {
        x shouldEqual test(i)
        i += 1
      }


      VList(1, 2, 3).sum shouldEqual 6
      VList(1, 2, 3).product shouldEqual 6
      VList(1, 2, 3).reduceLeft(_ - _) shouldEqual -4
      VList(1, 2, 3).reduceRight((x, temp) => temp - x) shouldEqual 0

      VList(1, 2, 3).foldRight(3)((x, temp) => temp - x) shouldEqual -3


      VList(VList(1, 2, 3), VList(4, 5, 6), VList(7, 8, 9)).flatten shouldEqual (1 to 9)
      val lists = VList(VList(1, 2, 3), VList(4, 5, 6), VList(7, 8, 9))
      VList(0, 1, 2).flatMap(x => lists(x)) shouldEqual (1 to 9)

      new VList(1 to 10).filter(_ % 2 == 0) shouldEqual (1 to 10).filter(_ % 2 == 0)

      VList.range(1, 10).filter(_ % 2 == 0) shouldEqual (1 to 10).filter(_ % 2 == 0)

      VList[Int](5, 4, 3, 2, 1).reverseMap(_ - 1) shouldEqual (0 to 4)

      VList(1, 2, 3, 4, 5).reverseFilter(_ % 2 != 0) shouldEqual List(5, 3, 1)

      VList(5, 4, 3, 2, 1).reverse shouldEqual (1 to 5)
    }

    "testing for errors" in {
      intercept[NoSuchElementException] {
        new VList[Int]().iterator.next()
      }

      intercept[NoSuchElementException] {
        new VList[Int]().reverseIterator.next()
      }

      val it = VList[Int](1).iterator
      it.next()
      intercept[NoSuchElementException] {
        it.next()
      }

      val reverseIt = VList[Int](1).reverseIterator
      reverseIt.next()
      intercept[NoSuchElementException] {
        reverseIt.next()
      }

      intercept[IndexOutOfBoundsException] {
        VList(1, 2, 3)(3)
      }

      intercept[IndexOutOfBoundsException] {
        VList(1, 2, 3)(-1)
      }
    }

  }
}