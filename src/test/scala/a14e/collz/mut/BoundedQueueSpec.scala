package a14e.collz.mut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

/**
  * Created by m0hct on 01.01.2017.
  */
class BoundedQueueSpec extends WordSpec with Matchers {
  "Tests for mutable BoundedQueue " when {
    "size" when {
      "single element" in {
        BoundedQueue[Int](1).size shouldBe 0
        BoundedQueue.fill(1)(1).size shouldBe 1
        BoundedQueue.of(1).size shouldBe 1
        (BoundedQueue[Int](1) += 1).size shouldBe 1
        (BoundedQueue[Int](3) += 1).size shouldBe 1
      }
      "multiple elements" in {
        BoundedQueue.fill(2)(1).size shouldBe 2
        BoundedQueue.of(1, 2, 3).size shouldBe 3
        (BoundedQueue[Int](3) += 1 += 2).size shouldBe 2
        (BoundedQueue[Int](3) += 1 += 2 += 3).size shouldBe 3
      }
      "elements more then size" in {
        (BoundedQueue.of(1) += 1 += 1).size shouldBe 1
        (BoundedQueue[Int](2) += 1 += 2 += 4).size shouldBe 2
        (BoundedQueue[Int](3) += 1 += 2 += 3 += 4).size shouldBe 3
      }

      "single element and removed" in {
        val q1 = BoundedQueue.fill(1)(1)
        q1.pull()
        q1.size shouldBe 0

        val q2 = BoundedQueue[Int](1) += 1
        q2.pull()
        q2.size shouldBe 0

        val q3 = BoundedQueue[Int](3) += 1
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


        removeTest(BoundedQueue.fill(2)(1))
        removeTest(BoundedQueue.of(1, 2, 3))
        removeTest(BoundedQueue[Int](3) += 1 += 2)
        removeTest(BoundedQueue[Int](3) += 1 += 2 += 3)
      }

      "elements more then size and removed" in {

        removeTest(BoundedQueue.of(1) += 1 += 1)
        removeTest(BoundedQueue[Int](2) += 1 += 2 += 4)
        removeTest(BoundedQueue[Int](3) += 1 += 2 += 3 += 4)
      }
    }


    "at index" when {
      "single element" in {

        BoundedQueue.fill(1)(1).apply(0) shouldBe 1
        BoundedQueue.of(1).apply(0) shouldBe 1
        (BoundedQueue[Int](1) += 1).apply(0) shouldBe 1
        (BoundedQueue[Int](3) += 1).apply(0) shouldBe 1
      }

      def testElementsAtIndex(q: Queue[Int],
                              traversableOnce: TraversableOnce[Int]): Unit = {
        for ((el, i) <- traversableOnce.toIterator.zipWithIndex)
          q(i) shouldBe el
      }

      "multiple elements" in {

        testElementsAtIndex(BoundedQueue.fill(2)(1), List(1, 1))
        testElementsAtIndex(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElementsAtIndex(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsAtIndex(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {


        testElementsAtIndex(BoundedQueue.of(1) += 1 += 1, List(1))
        testElementsAtIndex(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsAtIndex(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsAtIndex(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))

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
        testElementsAtIndexWithRemove(BoundedQueue.fill(1)(1), List(1))
        testElementsAtIndexWithRemove(BoundedQueue.of(1), List(1))
        testElementsAtIndexWithRemove(BoundedQueue[Int](1) += 1, List(1))
        testElementsAtIndexWithRemove(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElementsAtIndexWithRemove(BoundedQueue.fill(2)(1), List(1, 1))
        testElementsAtIndexWithRemove(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElementsAtIndexWithRemove(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsAtIndexWithRemove(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElementsAtIndexWithRemove(BoundedQueue.of(1) += 1 += 1, List(1))
        testElementsAtIndexWithRemove(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsAtIndexWithRemove(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsAtIndexWithRemove(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
    }

    "iterator" when {
      "single element" in {
        BoundedQueue.fill(1)(1).toIterator.toList shouldBe List(1)
        BoundedQueue.of(1).toIterator.toList shouldBe List(1)
        (BoundedQueue[Int](1) += 1).toIterator.toList shouldBe List(1)
        (BoundedQueue[Int](3) += 1).toIterator.toList shouldBe List(1)
      }
      "multiple elements" in {
        BoundedQueue.fill(2)(1).toIterator.toList shouldBe List(1, 1)
        BoundedQueue.of(1, 2, 3).toIterator.toList shouldBe (1 to 3).toList
        (BoundedQueue[Int](3) += 1 += 2).toIterator.toList shouldBe (1 to 2).toList
        (BoundedQueue[Int](3) += 2 += 3 += 4).toIterator.toList shouldBe (2 to 4).toList
      }
      "elements more then size" in {
        (BoundedQueue.of(1) += 1 += 1).toIterator.toList shouldBe List(1)
        (BoundedQueue[Int](2) += 1 += 2 += 4).toIterator.toList shouldBe List(2, 4)
        (BoundedQueue[Int](2) += 1 += 2 += 3 += 4).toIterator.toList shouldBe List(3, 4)
        (BoundedQueue[Int](3) += 1 += 2 += 3 += 4).toIterator.toList shouldBe List(2, 3, 4)
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
        testElementsIteratorWithRemove(BoundedQueue.fill(1)(1), List(1))
        testElementsIteratorWithRemove(BoundedQueue.of(1), List(1))
        testElementsIteratorWithRemove(BoundedQueue[Int](1) += 1, List(1))
        testElementsIteratorWithRemove(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElementsIteratorWithRemove(BoundedQueue.fill(2)(1), List(1, 1))
        testElementsIteratorWithRemove(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElementsIteratorWithRemove(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElementsIteratorWithRemove(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElementsIteratorWithRemove(BoundedQueue.of(1) += 1 += 1, List(1))
        testElementsIteratorWithRemove(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElementsIteratorWithRemove(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElementsIteratorWithRemove(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
    }

    "pull" when {
      def testElements(q: Queue[Int],
                       traversableOnce: TraversableOnce[Int]): Unit = {
        for (el <- traversableOnce)
          q.pull() shouldBe el
      }

      "single element" in {
        testElements(BoundedQueue.fill(1)(1), List(1))
        testElements(BoundedQueue.of(1), List(1))
        testElements(BoundedQueue[Int](1) += 1, List(1))
        testElements(BoundedQueue[Int](3) += 1, List(1))
      }
      "multiple elements" in {
        testElements(BoundedQueue.fill(2)(1), List(1, 1))
        testElements(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElements(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {
        testElements(BoundedQueue.of(1) += 1 += 1, List(1))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
      "single element and removed" in {
        testElements(BoundedQueue.fill(1)(1), List(1))
        testElements(BoundedQueue.of(1), List(1))
        testElements(BoundedQueue[Int](1) += 1, List(1))
        testElements(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElements(BoundedQueue.fill(2)(1), List(1, 1))
        testElements(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElements(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElements(BoundedQueue.of(1) += 1 += 1, List(1))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4 += 5 += 6, List(4, 5, 6))
      }
    }

    "pull all" when {
      def testElements(q: Queue[Int],
                       traversableOnce: TraversableOnce[Int]): Unit = {
        val takeSize = q.size - 1
        val pulled = q.pullAll(q.size - 1)
        traversableOnce.toIterator.take(takeSize).toList shouldBe pulled
        q.pull() shouldBe traversableOnce.toList.last
        q shouldBe empty
      }

      "single element" in {
        testElements(BoundedQueue.fill(1)(1), List(1))
        testElements(BoundedQueue.of(1), List(1))
        testElements(BoundedQueue[Int](1) += 1, List(1))
        testElements(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements" in {
        testElements(BoundedQueue.fill(2)(1), List(1, 1))
        testElements(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElements(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }

      "elements more then size" in {
        testElements(BoundedQueue.of(1) += 1 += 1, List(1))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }

      "single element and removed" in {
        testElements(BoundedQueue.fill(1)(1), List(1))
        testElements(BoundedQueue.of(1), List(1))
        testElements(BoundedQueue[Int](1) += 1, List(1))
        testElements(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        testElements(BoundedQueue.fill(2)(1), List(1, 1))
        testElements(BoundedQueue.of(1, 2, 3), 1 to 3)
        testElements(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        testElements(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        testElements(BoundedQueue.of(1) += 1 += 1, List(1))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        testElements(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
        testElements(BoundedQueue[Int](3) += 1 += 2 += 3 += 4 += 5 += 6, List(4, 5, 6))
      }
    }



    "foreach" when {
      def foreachTest(q: Queue[Int],
                      traversableOnce: TraversableOnce[Int]): Unit = {
        val buff = new ListBuffer[Int]()
        q.foreach(buff += _)
        buff.result() shouldBe traversableOnce.toList
      }

      "single element" in {
        foreachTest(BoundedQueue.fill(1)(1), List(1))
        foreachTest(BoundedQueue.of(1), List(1))
        foreachTest(BoundedQueue[Int](1) += 1, List(1))
        foreachTest(BoundedQueue[Int](3) += 1, List(1))
      }
      "multiple elements" in {
        foreachTest(BoundedQueue.fill(2)(1), List(1, 1))
        foreachTest(BoundedQueue.of(1, 2, 3), 1 to 3)
        foreachTest(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        foreachTest(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)
      }
      "elements more then size" in {
        foreachTest(BoundedQueue.of(1) += 1 += 1, List(1))
        foreachTest(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        foreachTest(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        foreachTest(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
      }
      "single element and removed" in {
        foreachTest(BoundedQueue.fill(1)(1), List(1))
        foreachTest(BoundedQueue.of(1), List(1))
        foreachTest(BoundedQueue[Int](1) += 1, List(1))
        foreachTest(BoundedQueue[Int](3) += 1, List(1))
      }

      "multiple elements and removed" in {

        foreachTest(BoundedQueue.fill(2)(1), List(1, 1))
        foreachTest(BoundedQueue.of(1, 2, 3), 1 to 3)
        foreachTest(BoundedQueue[Int](3) += 1 += 2, 1 to 2)
        foreachTest(BoundedQueue[Int](3) += 2 += 3 += 4, 2 to 4)

      }

      "elements more then size and removed" in {
        foreachTest(BoundedQueue.of(1) += 1 += 1, List(1))
        foreachTest(BoundedQueue[Int](2) += 1 += 2 += 4, List(2, 4))
        foreachTest(BoundedQueue[Int](2) += 1 += 2 += 3 += 4, List(3, 4))
        foreachTest(BoundedQueue[Int](3) += 1 += 2 += 3 += 4, List(2, 3, 4))
        foreachTest(BoundedQueue[Int](3) += 1 += 2 += 3 += 4 += 5 += 6, List(4, 5, 6))
      }
    }
  }

}
