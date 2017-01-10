/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

class BoundedQueueSpec extends WordSpec with Matchers {
  "Tests for BoundedQueue " when {

    "valid size" should {
      "empty" in {

        BoundedQueue[Int](10).size shouldBe 0

        BoundedQueue[Int](1).size shouldBe 0

        BoundedQueue[Int](10).isEmpty shouldBe true

        BoundedQueue[Int](1).isEmpty shouldBe true
      }

      "while read vector non fill" in {
        BoundedQueue[Int](2).size shouldBe 0

        (BoundedQueue[Int](2) :+ 1).size shouldBe 1

        (BoundedQueue[Int](2) :+ 1 :+ 2).size shouldBe 2

      }

      "while write vector non fill" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3).size shouldBe 2
        (BoundedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).size shouldBe 3
      }

      "after full rotate" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).size shouldBe 2
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).size shouldBe 2
      }
    }

    "push" should {
      "have right values while read vector non fill" in {
        BoundedQueue[Int](2).mkString("") shouldBe ""

        (BoundedQueue[Int](2) :+ 2).mkString("") shouldBe "2"

        (BoundedQueue[Int](2) :+ 2 :+ 3).mkString("") shouldBe "23"
      }

      "have right values while write vector non fill" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3).mkString("") shouldBe "23"
        (BoundedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).mkString("") shouldBe "233"
      }

      "after full rotate" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).mkString("") shouldBe "34"
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).mkString("") shouldBe "45"
      }
    }

    "push all" should {
      "have right values while read vector non fill" in {
        BoundedQueue[Int](2).mkString("") shouldBe ""

        BoundedQueue[Int](2).pushValues(2).mkString("") shouldBe "2"

        BoundedQueue[Int](2).pushValues(2, 3).mkString("") shouldBe "23"
      }

      "have right values while write vector non fill" in {
        BoundedQueue[Int](2).pushValues(1, 2, 3).mkString("") shouldBe "23"
        BoundedQueue[Int](3).pushValues(1, 2, 3, 3).mkString("") shouldBe "233"
      }

      "after full rotate" in {
        BoundedQueue[Int](2).pushValues(1, 2, 3, 4).mkString("") shouldBe "34"
        BoundedQueue[Int](2).pushValues(1, 2, 3, 4, 5).mkString("") shouldBe "45"
      }
    }

    "pull" should {
      "have right values while read vector non fill" in {
        val (q1, x1) = BoundedQueue[Int](2).pushValues(2).pull()
        q1.mkString("") shouldBe ""
        x1 shouldBe 2

        val (q2, x2) = BoundedQueue[Int](2).pushValues(2, 3).pull()
        q2.mkString("") shouldBe "3"
        x2 shouldBe 2
      }

      "have right values while write vector non fill" in {
        val (q1, x1) = BoundedQueue[Int](2).pushValues(1, 2, 3).pull()
        q1.mkString("") shouldBe "3"
        x1 shouldBe 2

        val (q2, x2) = BoundedQueue[Int](3).pushValues(1, 2, 3, 3).pull()
        q2.mkString("") shouldBe "33"
        x2 shouldBe 2
      }

      "after full rotate" in {
        val (q1, x1) = BoundedQueue[Int](2).pushValues(1, 2, 3, 4).pull()
        q1.mkString("") shouldBe "4"
        x1 shouldBe 3

        val (q2, x2) = BoundedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pull()
        q2.mkString("") shouldBe "45"
        x2 shouldBe 3
      }
    }

    "push and then pull" should {
      def deleteTest(queue: Queue[Int], list: List[Int]): Unit = {
        var q = queue
        for (x <- list) {
          val (newQueue, pulled) = q.pull()
          pulled shouldBe x
          q = newQueue
        }
        q.isEmpty shouldBe true

        q.pullOption() shouldBe(q, None)
      }


      def pushAndThenPullOneByOne(size: Int)(xs: Int*): Unit = {
        val list = new ListBuffer[Int]()
        var queue: Queue[Int] = BoundedQueue[Int](size)
        for (x <- xs) {
          queue = queue.push(x)
          list += x
          if (list.size > size)
            list.remove(0)

          deleteTest(queue, list.result())
        }
      }

      "have right values while read vector non fill" in {
        pushAndThenPullOneByOne(2)(2)

        pushAndThenPullOneByOne(2)(2, 3)
      }

      "have right values while write vector non fill" in {
        pushAndThenPullOneByOne(2)(1, 2, 3)

        pushAndThenPullOneByOne(3)(1, 2, 3, 3)
      }

      "have right after after full rotate" in {
        pushAndThenPullOneByOne(2)(1, 2, 3, 4)

        pushAndThenPullOneByOne(3)(1, 2, 3, 4, 5)
      }
    }

    "iterator test" should {

      def iteratorOneByOne(size: Int)(xs: Int*): Unit = {
        val list = new ListBuffer[Int]()
        var queue: Queue[Int] = BoundedQueue[Int](size)
        for (x <- xs) {
          queue = queue.push(x)
          list += x
          if (list.size > size)
            list.remove(0)

          queue.iterator.toList shouldBe list.result()
        }
      }

      "have right values while read vector non fill" in {
        iteratorOneByOne(2)(2)

        iteratorOneByOne(2)(2, 3)
      }

      "have right values while write vector non fill" in {
        iteratorOneByOne(2)(1, 2, 3)

        iteratorOneByOne(3)(1, 2, 3, 3)
      }

      "have right after after full rotate" in {
        iteratorOneByOne(2)(1, 2, 3, 4)

        iteratorOneByOne(3)(1, 2, 3, 4, 5)
      }
    }

    "pull all" should {
      "have right values while read vector non fill" in {
        val (q1, xs1) = BoundedQueue[Int](2).pushValues(2).pullAll(1)
        q1.mkString("") shouldBe ""
        xs1 shouldBe List(2)

        val (q2, xs2) = BoundedQueue[Int](2).pushValues(2).pullAll(2)
        q2.mkString("") shouldBe ""
        xs2 shouldBe List(2)

        val (q3, xs3) = BoundedQueue[Int](2).pushValues(2).pullAll(0)
        q3.mkString("") shouldBe "2"
        xs3 shouldBe Nil
      }


      "have right values while write vector non fill" in {
        val (q1, xs1) = BoundedQueue[Int](2).pushValues(1, 2, 3).pullAll(1)
        q1.mkString("") shouldBe "3"
        xs1 shouldBe List(2)

        val (q2, xs2) = BoundedQueue[Int](3).pushValues(1, 2, 3, 3).pullAll(2)
        q2.mkString("") shouldBe "3"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q1, xs1) = BoundedQueue[Int](2).pushValues(1, 2, 3, 4).pullAll(1)
        q1.mkString("") shouldBe "4"
        xs1 shouldBe List(3)

        val (q2, xs2) = BoundedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullAll(3)
        q2.mkString("") shouldBe ""
        xs2 shouldBe List(3, 4, 5)
      }
    }

    "pull while" should {
      "have right values while read vector non fill" in {
        val (q2, xs2) = BoundedQueue[Int](5).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(1, 2, 3)
      }

      "have right values while write vector non fill" in {
        val (q2, xs2) = BoundedQueue[Int](4).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q2, xs2) = BoundedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(3)
      }
    }


    "head" should {
      "have right values while read vector non fill" in {
        BoundedQueue[Int](2).headOption shouldBe None

        (BoundedQueue[Int](2) :+ 2).headOption shouldBe Some(2)

        (BoundedQueue[Int](2) :+ 2 :+ 3).headOption shouldBe Some(2)
      }

      "have right values while write vector non fill" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3).headOption shouldBe Some(2)
        (BoundedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).headOption shouldBe Some(2)
      }

      "after full rotate" in {
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).headOption shouldBe Some(3)
        (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).headOption shouldBe Some(4)
      }

      "last" should {
        "have right values while read vector non fill" in {
          BoundedQueue[Int](2).lastOption shouldBe None

          (BoundedQueue[Int](2) :+ 2).lastOption shouldBe Some(2)

          (BoundedQueue[Int](2) :+ 2 :+ 3).lastOption shouldBe Some(3)
        }

        "have right values while write vector non fill" in {
          (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3).lastOption shouldBe Some(3)
          (BoundedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).lastOption shouldBe Some(3)
        }

        "after full rotate" in {
          (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).lastOption shouldBe Some(4)
          (BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).lastOption shouldBe Some(5)
        }
      }

      "apply" should {
        "have right values while read vector non fill" in {
          val q1 = BoundedQueue[Int](2) :+ 2
          q1(0) shouldBe 2

          val q2 = BoundedQueue[Int](2) :+ 2 :+ 3
          q2(0) shouldBe 2
          q2(1) shouldBe 3
        }

        "have right values while write vector non fill" in {
          val q1 = BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3
          q1(0) shouldBe 2
          q1(1) shouldBe 3

          val q2 = BoundedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3
          q2(0) shouldBe 2
          q2(1) shouldBe 3
          q2(2) shouldBe 3
        }

        "after full rotate" in {
          val q1 = BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4
          q1(0) shouldBe 3
          q1(1) shouldBe 4

          val q2 = BoundedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5
          q2(0) shouldBe 4
          q2(1) shouldBe 5
        }
      }

    }

  }
}
