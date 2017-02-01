/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

class VListSpec extends WordSpec with Matchers {
  "Tests for VList " should {

    "valid transform to list" when {
      "empty" in {
        VList().toList shouldBe Nil
      }
      "single element" in {
        VList(1).toList shouldBe List(1)
      }

      "multiple elements" in {

        VList(1, 2, 3).toList shouldBe List(1, 2, 3)
      }

      "a lot of elements" in {

        VList(1 to 100: _*).toList shouldBe (1 to 100).toList
      }
    }

    "valid transform to array" when {
      "empty" in {
        VList[Int]().toArray shouldBe Array[Int]()
      }
      "single element" in {
        VList(1).toArray shouldBe Array(1)
      }

      "multiple elements" in {

        VList(1, 2, 3).toArray shouldBe Array(1, 2, 3)
      }

      "a lot of elements" in {

        VList(1 to 100: _*).toArray shouldBe (1 to 100).toArray
      }
    }

    "reverse" when {
      "empty" in {
        VList().reverse shouldBe Nil
      }
      "single element" in {
        VList(1).reverse shouldBe List(1)
      }

      "multiple elements" in {

        VList(1, 2, 3).reverse shouldBe List(1, 2, 3).reverse
      }

      "a lot of elements" in {

        VList(1 to 100: _*).reverse shouldBe (1 to 100).toList.reverse
      }
    }


    "valid iterator" when {
      "empty" in {
        VList().iterator.toList shouldBe Nil
      }
      "single element" in {
        VList(1).iterator.toList shouldBe List(1)
      }

      "multiple elements" in {

        VList(1, 2, 3).iterator.toList shouldBe List(1, 2, 3)
      }

      "a lot of elements" in {

        VList(1 to 100: _*).iterator.toList shouldBe (1 to 100).toList
      }
    }


    "valid reverse iterator" when {
      "empty" in {
        VList().reverseIterator.toList shouldBe Nil.reverse
      }
      "single element" in {
        VList(1).reverseIterator.toList shouldBe List(1).reverse
      }

      "multiple elements" in {
        VList(1, 2, 3).reverseIterator.toList shouldBe List(1, 2, 3).reverse
      }

      "a lot of elements" in {
        VList(1 to 100: _*).reverseIterator.toList shouldBe (1 to 100).toList.reverse
      }
    }

    "valid size after create" when {
      "empty" in {
        VList().isEmpty shouldBe true
        VList().size shouldBe 0
      }
      "single element" in {
        VList(1).isEmpty shouldBe false
        VList(1).size shouldBe 1
      }

      "multiple elements" in {
        VList(1, 2, 3).isEmpty shouldBe false
        VList(1, 2, 3).size shouldBe 3
      }

      "a lot of elements" in {
        VList(1 to 100: _*).isEmpty shouldBe false
        VList(1 to 100: _*).size shouldBe 100
      }
    }

    "foreach" when {
      def testForeach[T](list: VList[T], test: List[T]): Unit = {
        val res = new ListBuffer[T]()
        list.foreach(res += _)
        res.result() shouldBe test
      }

      "empty" in {
        testForeach(VList(), Nil)
      }
      "single element" in {
        testForeach(VList(1), List(1))
      }

      "multiple elements" in {

        testForeach(VList(1, 2, 3), List(1, 2, 3))
      }

      "a lot of elements" in {
        testForeach(VList(1 to 100: _*), (1 to 100).toList)
      }
    }

    "reverse foreach" when {
      def testReverseForeach[T](list: VList[T], test: List[T]): Unit = {
        val res = new ListBuffer[T]()
        list.reverseForeach(_ +=: res)
        res.result() shouldBe test
      }

      "empty" in {
        testReverseForeach(VList(), Nil)
      }
      "single element" in {
        testReverseForeach(VList(1), List(1))
      }

      "multiple elements" in {

        testReverseForeach(VList(1, 2, 3), List(1, 2, 3))
      }

      "a lot of elements" in {
        testReverseForeach(VList(1 to 100: _*), (1 to 100).toList)
      }
    }

    "fold right" when {
      "empty" in {
        VList[Int]().foldRight(0)(_ + _) shouldBe 0
      }

      "single element" in {
        VList(1).foldRight(1)(_ + _) shouldBe 2
      }

      "multiple elements" in {

        VList(1, 2, 3).foldRight("")((x, acc) => acc + x) shouldBe "321"
      }
    }

    "equals" when {

      "empty" in {
        VList[Int]() shouldBe Nil
        VList[Int]() shouldBe Array[Int]()
        VList[Int]() shouldBe VList[Int]()
        VList[Int]() should not be VList(1)
      }
      "single element" in {
        VList(1) shouldBe 1 :: Nil
        VList(1) shouldBe Array(1)
        VList(1) shouldBe VList(1)
        VList[Int]() should not be VList(2)
      }

      "multiple elements" in {
        VList(1, 2, 3) shouldBe List(1, 2, 3)
        VList(1, 2, 3) shouldBe Array(1, 2, 3)
        VList(1, 2, 3) shouldBe VList(1, 2, 3)
        VList[Int](1, 2, 3) should not be VList(1, 2, 4)
      }

      "a lot of elements" in {
        VList(1 to 100: _*) shouldBe (1 to 100).toList
        VList(1 to 100: _*) shouldBe (1 to 100).toArray
        VList(1 to 100: _*) should not be VList(1 to 101: _*)
      }
    }

    "append" when {

      "empty" in {

        (VList[Int]() += 1) shouldBe List(1)
      }
      "single element" in {
        (VList[Int](1) += 1) shouldBe List(1, 1)
        (VList[Int](1) += 1 += 1) shouldBe List(1, 1, 1)
      }

      "multiple elements" in {
        (VList[Int](1, 2, 3) += 1) shouldBe List(1, 2, 3, 1)
        (VList[Int](1, 2, 3, 4) += 1) shouldBe List(1, 2, 3, 4, 1)
        (VList[Int](1, 2, 3, 4) += 1 += 1) shouldBe List(1, 2, 3, 4, 1, 1)
      }

      "a lot of elements" in {
        (VList[Int](1 to 100: _*) += 1) shouldBe (List(1 to 100: _*) :+ 1)
        (VList[Int](1 to 100: _*) += 1 += 2) shouldBe (List(1 to 100: _*) :+ 1 :+ 2)
      }
    }

    "append all" when {
      "empty" in {

        (VList[Int]() ++= List(1)) shouldBe List(1)
      }
      "single element" in {
        (VList[Int](1) ++= List(1)) shouldBe List(1, 1)
        (VList[Int](1) ++= List(1, 1)) shouldBe List(1, 1, 1)
      }

      "multiple elements" in {
        (VList[Int](1, 2, 3) ++= List(1, 1)) shouldBe List(1, 2, 3, 1, 1)
        (VList[Int](1, 2, 3, 4) ++= List(1, 1, 1)) shouldBe List(1, 2, 3, 4, 1, 1, 1)
        (VList[Int](1, 2, 3, 4) ++=
          List(1, 1, 1)
          ++= List(1, 1, 1)) shouldBe List(1, 2, 3, 4) ++ List(1, 1, 1) ++ List(1, 1, 1)
      }

      "a lot of elements" in {
        (VList[Int](1 to 100: _*) ++= (1 to 100)) shouldBe (List(1 to 100: _*) ++ (1 to 100))

        (VList[Int](1 to 100: _*) ++=
          (1 to 100) ++=
          (1 to 100)) shouldBe ((1 to 100) ++ (1 to 100) ++ (1 to 100))
      }
    }

    "prepend" when {

      "empty" in {

        (1 +=: VList[Int]()) shouldBe List(1)
      }
      "single element" in {
        (1 +=: VList[Int](1)) shouldBe List(1, 1)
        (1 +=: 1 +=: VList[Int](1)) shouldBe List(1, 1, 1)
      }

      "multiple elements" in {
        (1 +=: VList[Int](0, 1, 2, 3)) shouldBe List(1, 0, 1, 2, 3)
        (1 +=: VList[Int](0, 1, 2, 3, 4)) shouldBe List(1, 0, 1, 2, 3, 4)
        (1 +=: 1 +=: VList[Int](0, 1, 2, 3, 4)) shouldBe List(1, 1, 0, 1, 2, 3, 4)
      }

      "a lot of elements" in {
        (1 +=: VList[Int](1 to 5: _*)) shouldBe (1 +: List(1 to 5: _*))
        (1 +=: 2 +=: VList[Int](1 to 100: _*)) shouldBe (1 +: 2 +: List(1 to 100: _*))
      }
    }

    "prepend all" when {
      "empty" in {

        (List(1) ++=: VList[Int]()) shouldBe List(1)
      }
      "single element" in {
        (List(1) ++=: VList[Int](1)) shouldBe List(1, 1)
        (List(1, 1) ++=: VList[Int](1)) shouldBe List(1, 1, 1)
      }

      "multiple elements" in {
        (List(1, 1) ++=: VList[Int](1, 2, 3)) shouldBe List(1, 1) ::: List(1, 2, 3)
        (List(1, 1, 1) ++=: VList[Int](1, 2, 3, 4)) shouldBe List(1, 1, 1) ::: List(1, 2, 3, 4)
        (List(1, 1, 1) ++=:
          List(1, 1, 1) ++=:
          VList[Int](1, 2, 3, 4)) shouldBe List(1, 1, 1) ::: List(1, 1, 1) ::: List(1, 2, 3, 4)
      }

      "a lot of elements" in {
        ((1 to 100) ++=: VList[Int](1 to 100: _*)) shouldBe ((1 to 100) ++ (1 to 100))

        ((1 to 100) ++=:
          (1 to 100) ++=:
          VList[Int](1 to 100: _*)) shouldBe ((1 to 100) ++ (1 to 100) ++ (1 to 100))
      }
    }

    "remove" when {
      "empty" in {
        intercept[IndexOutOfBoundsException] {
          VList[Int]().remove(0)
        }
      }
      "single element" in {
        intercept[IndexOutOfBoundsException] {
          VList[Int](1).remove(1)
        }
        val list = VList[Int](1)
        list.remove(0)
        list shouldBe Nil
      }

      "multiple elements remove first" in {
        val list = VList[Int](1, 2, 3, 4)
        list.remove(0)
        list shouldBe List(2, 3, 4)
      }

      "multiple elements remove last" in {
        val list = VList[Int](1, 2, 3, 4)
        list.remove(3)
        list shouldBe List(1, 2, 3)
      }

      "multiple elements remove in middle" in {
        val list = VList[Int](1, 2, 3, 4)
        list.remove(2)
        list shouldBe List(1, 2, 4)


        val list2 = VList[Int](1, 2, 3, 4)
        list2.remove(1)
        list2 shouldBe List(1, 3, 4)
      }

      "remove multiple times" in {
        val list = VList[Int](1, 2, 3, 4)
        list.remove(1)
        list shouldBe List(1, 3, 4)

        list.remove(2)
        list shouldBe List(1, 3)

        list.remove(1)
        list shouldBe List(1)

        list.remove(0)
        list shouldBe Nil
      }

      "remove and append" in {
        val list = VList[Int](1, 2, 3, 4)
        list.remove(1)
        list shouldBe List(1, 3, 4)

        list += 1
        list shouldBe List(1, 3, 4, 1)

        list.remove(1)
        list shouldBe List(1, 4, 1)

        list += 2
        list shouldBe List(1, 4, 1, 2)


        list.remove(1)
        list.remove(1)
        list.remove(1)
        list shouldBe List(1)

        list += 2 += 2 += 2
        list shouldBe List(1, 2, 2, 2)
      }
    }

    "head" when {
      "empty" in {
        intercept[UnsupportedOperationException] {
          VList().head
        }
      }
      "single element" in {
        VList(1).head shouldBe 1
      }

      "multiple elements" in {

        VList(1, 2, 3).head shouldBe 1
      }

      "a lot of elements" in {

        VList(0 to 100: _*).head shouldBe 0
      }
    }

    "last" when {
      "empty" in {
        intercept[UnsupportedOperationException] {
          VList().last
        }
      }
      "single element" in {
        VList(1).last shouldBe 1
      }

      "multiple elements" in {

        VList(1, 2, 3).last shouldBe 3
      }

      "a lot of elements" in {

        VList(0 to 100: _*).last shouldBe 100
      }
    }

    "tail" when {
      "empty" in {
        intercept[UnsupportedOperationException] {
          VList().tail
        }
      }
      "single element" in {
        VList(1).tail shouldBe Nil
      }

      "multiple elements" in {

        VList(1, 2, 3).tail shouldBe VList(2, 3)
      }

      "a lot of elements" in {

        VList(0 to 100: _*).tail shouldBe VList(1 to 100: _*)
      }
    }


    "filter" when {
      "empty" in {
        VList[Int]().filter(_ != 0) shouldBe Nil
      }
      "single element" in {
        VList(1).filter(_ != 0) shouldBe VList(1)
        VList(1).filter(_ != 1) shouldBe VList[Int]()
      }

      "multiple elements" in {
        VList(1, 2, 3).filter(_ % 2 == 0) shouldBe VList(2)
        VList(1, 2, 3).filter(_ % 2 != 0) shouldBe VList(1, 3)
      }
    }


    "insert all" when {
      "empty" in {
        intercept[IndexOutOfBoundsException] {
          VList[Int]().insertAll(-1, List(1, 2, 3))
        }
        val list = VList[Int]()
        list.insertAll(0, List(1, 2, 3))
        list shouldBe List(1, 2, 3)
      }
      "single element" in {
        intercept[IndexOutOfBoundsException] {
          VList(2).insertAll(2, List(1, 2, 3))
        }
        val list = VList(1)
        list.insertAll(0, List(1, 2, 3))
        list shouldBe VList(1, 2, 3, 1)


        val list1 = VList(1)
        list1.insertAll(1, List(2, 3, 4))
        list1 shouldBe VList(1, 2, 3, 4)
      }

      "multiple elements" in {
        val list = VList(1, 2, 3)
        list.insertAll(0, List(1, 2, 3))
        list shouldBe VList(1, 2, 3, 1, 2, 3)


        val list1 = VList(1, 2, 3)
        list1.insertAll(1, List(1, 2, 3))
        list1 shouldBe VList(1, 1, 2, 3, 2, 3)


        val list2 = VList(1, 2, 3)
        list2.insertAll(2, List(1, 2, 3))
        list2 shouldBe VList(1, 2, 1, 2, 3, 3)


        val list3 = VList(1, 2, 3)
        list3.insertAll(3, List(4, 5, 6))
        list3 shouldBe (1 to 6)
      }

    }
    "toString" when {
      "empty" in {
        VList().toString() shouldBe "VList()"
      }
      "single element" in {
        VList(1).toString() shouldBe "VList(1)"
      }

      "multiple elements" in {

        VList(1, 2, 3).toString() shouldBe "VList(1, 2, 3)"

      }
    }
  }
}