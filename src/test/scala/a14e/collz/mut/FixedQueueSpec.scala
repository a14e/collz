package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

/**
  * Created by m0hct on 01.01.2017.
  */
class FixedQueueSpec extends WordSpec with Matchers {
  "Tests for mutable Fixed Queue " when {
    "size" when {
      "single element" in {
        FixedQueue[Int](1).size shouldBe 0
        FixedQueue.fill(1)(1).size shouldBe 1
        FixedQueue.of(1).size shouldBe 1
        (FixedQueue[Int](1) += 1).size shouldBe 1
        (FixedQueue[Int](3) += 1).size shouldBe 1
      }
      "multiple elements" in {
        FixedQueue.fill(2)(1).size shouldBe 2
        FixedQueue.of(1, 2, 3).size shouldBe 3
        (FixedQueue[Int](3) += 1 += 2).size shouldBe 2
        (FixedQueue[Int](3) += 1 += 2 += 3).size shouldBe 3
      }
      "elements more then size" in {
        (FixedQueue.of(1) += 1 += 1).size shouldBe 1
        (FixedQueue[Int](2) += 1 += 2 += 4).size shouldBe 2
        (FixedQueue[Int](3) += 1 += 2 += 3 += 4).size shouldBe 3
      }

      "single element and removed" in {
        val q1 = FixedQueue.fill(1)(1)
        q1.pull()
        q1.size shouldBe 0

        val q2 = FixedQueue[Int](1) += 1
        q2.pull()
        q2.size shouldBe 0

        val q3 = FixedQueue[Int](3) += 1
        q3.pull()
        q3.size shouldBe 0
      }

      def removeTest(q: Queue[Int]): Unit = {
        for (i <- (0 until q.size).reverse) {
          q.pull()
          q.size shouldBe i
        }
        q.isEmpty shouldBe true
      }

      "multiple elements and removed" in {


        removeTest(FixedQueue.fill(2)(1))
        removeTest(FixedQueue.of(1, 2, 3))
        removeTest(FixedQueue[Int](3) += 1 += 2)
        removeTest(FixedQueue[Int](3) += 1 += 2 += 3)
      }

      "elements more then size and removed" in {

        removeTest(FixedQueue.of(1) += 1 += 1)
        removeTest(FixedQueue[Int](2) += 1 += 2 += 4)
        removeTest(FixedQueue[Int](3) += 1 += 2 += 3 += 4)
      }
    }


    "at index" when {
      "single element" in {

        FixedQueue.fill(1)(1).apply(0) shouldBe 1
        FixedQueue.of(1).apply(0) shouldBe 1
        (FixedQueue[Int](1) += 1).apply(0) shouldBe 1
        (FixedQueue[Int](3) += 1).apply(0) shouldBe 1
      }

      def testElementsAtIndex(q: Queue[Int],
                              traversableOnce: TraversableOnce[Int]): Unit = {
        for ((el, i) <- traversableOnce.toIterator.zipWithIndex)
          q(i) shouldBe el
      }

      "multiple elements" in {

        testElementsAtIndex(FixedQueue.fill(2)(1), List(1, 1))
        testElementsAtIndex(FixedQueue.of(1, 2, 3), 1 to 3)
        testElementsAtIndex(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsAtIndex(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {


        testElementsAtIndex(FixedQueue.of(1) += 1 += 1, List(1))
        testElementsAtIndex(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsAtIndex(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsAtIndex(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))

      }

      def testElementsAtIndexWithRemove(q: Queue[Int],
                                        traversable: Traversable[Int]): Unit = {
        var list = traversable.toList
        testElementsAtIndex(q, list)

        for (_ <- list.indices) {
          list = list.tail
          q.pull()
          testElementsAtIndex(q, list)
        }
      }

      "single element and removed" in {
        testElementsAtIndexWithRemove(FixedQueue.fill(1)(1), List(1))
        testElementsAtIndexWithRemove(FixedQueue.of(1), List(1))
        testElementsAtIndexWithRemove(FixedQueue[Int](1) += 1, List(1))
        testElementsAtIndexWithRemove(FixedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElementsAtIndexWithRemove(FixedQueue.fill(2)(1), List(1, 1))
        testElementsAtIndexWithRemove(FixedQueue.of(1, 2, 3), 1 to 3)
        testElementsAtIndexWithRemove(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsAtIndexWithRemove(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElementsAtIndexWithRemove(FixedQueue.of(1) += 1 += 1, List(1))
        testElementsAtIndexWithRemove(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsAtIndexWithRemove(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsAtIndexWithRemove(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
    }

    "iterator" when {
      "single element" in {
        FixedQueue.fill(1)(1).toIterator.toList shouldBe List(1)
        FixedQueue.of(1).toIterator.toList shouldBe List(1)
        (FixedQueue[Int](1) += 1).toIterator.toList shouldBe List(1)
        (FixedQueue[Int](3) += 1).toIterator.toList shouldBe List(1)
      }
      "multiple elements" in {
        FixedQueue.fill(2)(1).toIterator.toList shouldBe List(1, 1)
        FixedQueue.of(1, 2, 3).toIterator.toList shouldBe (1 to 3).toList
        (FixedQueue[Int](3) += 1 += 2).toIterator.toList shouldBe (1 to 2).toList
        (FixedQueue[Int](3) += 2 += 3 += 4).toIterator.toList shouldBe (2 to 4).toList
      }
      "elements more then size" in {
        (FixedQueue.of(1) += 1 += 1).toIterator.toList shouldBe List(1)
        (FixedQueue[Int](2) += 1 += 2 += 4).toIterator.toList shouldBe List(2, 4)
        (FixedQueue[Int](2) += 1 += 2 += 3 += 4).toIterator.toList shouldBe List(3, 4)
        (FixedQueue[Int](3) += 1 += 2 += 3 += 4).toIterator.toList shouldBe List(2, 3, 4)
      }

      def testElementsIteratorWithRemove(q: Queue[Int],
                                         traversable: Traversable[Int]): Unit = {
        var list = traversable.toList
        q.toIterator.toList shouldBe list

        for (_ <- list.indices) {
          list = list.tail
          q.pull()
          q.toIterator.toList shouldBe list
        }
      }

      "single element and removed" in {
        testElementsIteratorWithRemove(FixedQueue.fill(1)(1), List(1))
        testElementsIteratorWithRemove(FixedQueue.of(1), List(1))
        testElementsIteratorWithRemove(FixedQueue[Int](1) += 1, List(1))
        testElementsIteratorWithRemove(FixedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElementsIteratorWithRemove(FixedQueue.fill(2)(1), List(1, 1))
        testElementsIteratorWithRemove(FixedQueue.of(1, 2, 3), 1 to 3)
        testElementsIteratorWithRemove(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsIteratorWithRemove(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElementsIteratorWithRemove(FixedQueue.of(1) += 1 += 1, List(1))
        testElementsIteratorWithRemove(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsIteratorWithRemove(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsIteratorWithRemove(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
    }

    "pull" when {
      def testElements(q: Queue[Int],
                       traversableOnce: TraversableOnce[Int]): Unit = {
        for (el <- traversableOnce)
          q.pull() shouldBe el
      }

      "single element" in {
        testElements(FixedQueue.fill(1)(1), List(1))
        testElements(FixedQueue.of(1), List(1))
        testElements(FixedQueue[Int](1) += 1, List(1))
        testElements(FixedQueue[Int](3) += 1, List(1))
      }
      "multiple elements" in {
        testElements(FixedQueue.fill(2)(1), List(1, 1))
        testElements(FixedQueue.of(1, 2, 3), 1 to 3)
        testElements(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {
        testElements(FixedQueue.of(1) += 1 += 1, List(1))
        testElements(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
      "single element and removed" in {
        testElements(FixedQueue.fill(1)(1), List(1))
        testElements(FixedQueue.of(1), List(1))
        testElements(FixedQueue[Int](1) += 1, List(1))
        testElements(FixedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElements(FixedQueue.fill(2)(1), List(1, 1))
        testElements(FixedQueue.of(1, 2, 3), 1 to 3)
        testElements(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElements(FixedQueue.of(1) += 1 += 1, List(1))
        testElements(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
        testElements(FixedQueue[Int](3) += 1 += 2 += 3 += 4 += 5 += 6, List(4, 5, 6))
      }
    }

    "pull all" when {
      "single element" in {}
      "multiple elements" in {}
      "elements more then size" in {}

      "single element and removed" in {}
      "multiple elements and removed" in {}

      "elements more then size and removed" in {}
    }

    "pull while" in {

    }


    "foreach" when {
      def foreachTest(q: Queue[Int],
                      traversableOnce: TraversableOnce[Int]): Unit = {
        val buff = new ListBuffer[Int]()
        q.foreach(buff += _)
        buff.result() shouldBe traversableOnce.toList
      }

      "single element" in {
        foreachTest(FixedQueue.fill(1)(1), List(1))
        foreachTest(FixedQueue.of(1), List(1))
        foreachTest(FixedQueue[Int](1) += 1, List(1))
        foreachTest(FixedQueue[Int](3) += 1, List(1))
      }
      "multiple elements" in {
        foreachTest(FixedQueue.fill(2)(1), List(1, 1))
        foreachTest(FixedQueue.of(1, 2, 3), 1 to 3)
        foreachTest(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        foreachTest(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {
        foreachTest(FixedQueue.of(1) += 1 += 1, List(1))
        foreachTest(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        foreachTest(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        foreachTest(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
      "single element and removed" in {
        foreachTest(FixedQueue.fill(1)(1), List(1))
        foreachTest(FixedQueue.of(1), List(1))
        foreachTest(FixedQueue[Int](1) += 1, List(1))
        foreachTest(FixedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        foreachTest(FixedQueue.fill(2)(1), List(1, 1))
        foreachTest(FixedQueue.of(1, 2, 3), 1 to 3)
        foreachTest(FixedQueue[Int](3) += 1 += 2, 1 to 2)
        foreachTest(FixedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        foreachTest(FixedQueue.of(1) += 1 += 1, List(1))
        foreachTest(FixedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        foreachTest(FixedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        foreachTest(FixedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
        foreachTest(FixedQueue[Int](3) += 1 += 2 += 3 += 4 += 5 += 6, List(4, 5, 6))
      }
    }
  }

}
