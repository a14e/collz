package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ListBuffer

/**
  * Created by m0hct on 31.01.2017.
  */
class BoundedBuckedQueueSpec extends WordSpec with Matchers {
  "Tests for BoundedBuckedQueue " when {

    "valid size" should {
      "empty" in {

        BoundedBucketQueue[Int](10).queue.size shouldBe 0

        BoundedBucketQueue[Int](1).queue.size shouldBe 0

        BoundedBucketQueue[Int](10).isEmpty shouldBe true

        BoundedBucketQueue[Int](1).isEmpty shouldBe true
      }

      "while read vector non fill" in {
        BoundedBucketQueue[Int](2).queue.size shouldBe 0
        BoundedBucketQueue[Int](2).bucket.length shouldBe 0

        (BoundedBucketQueue[Int](2) :+ 1).queue.size shouldBe 1
        (BoundedBucketQueue[Int](2) :+ 1).bucket.length shouldBe 0

        (BoundedBucketQueue[Int](2) :+ 1 :+ 2).queue.size shouldBe 2
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2).bucket.length shouldBe 0

      }

      "while write vector non fill" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).queue.size shouldBe 2
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).bucket.length shouldBe 1

        (BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).queue.size shouldBe 3
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 3).bucket.length shouldBe 2
      }

      "after full rotate" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).queue.size shouldBe 2
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).bucket.length shouldBe 2

        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).queue.size shouldBe 2
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).bucket.length shouldBe 3
      }
    }

    "push" should {
      "have right values while read vector non fill" in {
        BoundedBucketQueue[Int](2).queue.mkString("") shouldBe ""

        (BoundedBucketQueue[Int](2) :+ 2).queue.mkString("") shouldBe "2"

        (BoundedBucketQueue[Int](2) :+ 2 :+ 3).queue.mkString("") shouldBe "23"


        BoundedBucketQueue[Int](2).bucket.mkString("") shouldBe ""

        (BoundedBucketQueue[Int](2) :+ 2).bucket.mkString("") shouldBe ""

        (BoundedBucketQueue[Int](2) :+ 2 :+ 3).bucket.mkString("") shouldBe ""
      }

      "have right values while write vector non fill" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).queue.mkString("") shouldBe "23"
        (BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).queue.mkString("") shouldBe "233"


        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).bucket.mkString("") shouldBe "1"
        (BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).bucket.mkString("") shouldBe "1"
      }

      "after full rotate" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).queue.mkString("") shouldBe "34"
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).queue.mkString("") shouldBe "45"


        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).bucket.mkString("") shouldBe "21"
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).bucket.mkString("") shouldBe "321"
      }
    }

    "push all" should {
      "have right values while read vector non fill" in {
        BoundedBucketQueue[Int](2).queue.mkString("") shouldBe ""

        BoundedBucketQueue[Int](2).pushValues(2).queue.mkString("") shouldBe "2"

        BoundedBucketQueue[Int](2).pushValues(2, 3).queue.mkString("") shouldBe "23"


        BoundedBucketQueue[Int](2).bucket.mkString("") shouldBe ""

        BoundedBucketQueue[Int](2).pushValues(2).bucket.mkString("") shouldBe ""

        BoundedBucketQueue[Int](2).pushValues(2, 3).bucket.mkString("") shouldBe ""
      }

      "have right values while write vector non fill" in {
        BoundedBucketQueue[Int](2).pushValues(1, 2, 3).queue.mkString("") shouldBe "23"
        BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 3).queue.mkString("") shouldBe "233"

        BoundedBucketQueue[Int](2).pushValues(1, 2, 3).bucket.mkString("") shouldBe "1"
        BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 3).bucket.mkString("") shouldBe "1"
      }

      "after full rotate" in {
        BoundedBucketQueue[Int](2).pushValues(1, 2, 3, 4).queue.mkString("") shouldBe "34"
        BoundedBucketQueue[Int](2).pushValues(1, 2, 3, 4, 5).bucket.mkString("") shouldBe "321"
      }
    }

    "clear of bucket" should {
      "withEmptyBucket" in {
        BoundedBucketQueue[Int](2).withEmptyBucket.queue.mkString("") shouldBe ""

        BoundedBucketQueue[Int](2).pushValues(2).withEmptyBucket.queue.mkString("") shouldBe "2"

        BoundedBucketQueue[Int](2).pushValues(2, 3).withEmptyBucket.queue.mkString("") shouldBe "23"


        BoundedBucketQueue[Int](2).pushValues(1, 2, 3).withEmptyBucket.queue.mkString("") shouldBe "23"


        BoundedBucketQueue[Int](2).withEmptyBucket.bucket shouldBe Nil

        BoundedBucketQueue[Int](2).pushValues(2).withEmptyBucket.bucket shouldBe Nil

        BoundedBucketQueue[Int](2).pushValues(2, 3).withEmptyBucket.bucket shouldBe Nil


        BoundedBucketQueue[Int](2).pushValues(1, 2, 3).withEmptyBucket.bucket shouldBe Nil
      }

      "clearBucket" in {
        val (q1, bucket1) = BoundedBucketQueue[Int](2).clearBucket
        q1.queue.mkString("") shouldBe ""
        q1.bucket shouldBe Nil
        bucket1  shouldBe Nil

        val (q2, bucket2) = BoundedBucketQueue[Int](2).pushValues(1, 2).clearBucket
        q2.queue.mkString("") shouldBe "12"
        q2.bucket shouldBe Nil
        bucket2  shouldBe Nil

        val (q3, bucket3) = BoundedBucketQueue[Int](2).pushValues(1, 2 , 3).clearBucket
        q3.queue.mkString("") shouldBe "23"
        q3.bucket shouldBe Nil
        bucket3  shouldBe List(1)
      }
    }

    "pull" should {
      "have right values while read vector non fill" in {
        val (q1, x1) = BoundedBucketQueue[Int](2).pushValues(2).pull()
        q1.queue.mkString("") shouldBe ""
        q1.bucket.mkString("") shouldBe ""
        x1 shouldBe 2

        val (q2, x2) = BoundedBucketQueue[Int](2).pushValues(2, 3).pull()
        q2.queue.mkString("") shouldBe "3"
        q2.bucket.mkString("") shouldBe ""
        x2 shouldBe 2
      }

      "have right values while write vector non fill" in {
        val (q1, x1) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3).pull()
        q1.queue.mkString("") shouldBe "3"
        q1.bucket.mkString("") shouldBe "1"
        x1 shouldBe 2

        val (q2, x2) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 3).pull()
        q2.queue.mkString("") shouldBe "33"
        q1.bucket.mkString("") shouldBe "1"
        x2 shouldBe 2
      }

      "after full rotate" in {
        val (q1, x1) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3, 4).pull()
        q1.queue.mkString("") shouldBe "4"
        q1.bucket.mkString("") shouldBe "21"
        x1 shouldBe 3

        val (q2, x2) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 4, 5).pull()
        q2.queue.mkString("") shouldBe "45"
        q1.bucket.mkString("") shouldBe "21"
        x2 shouldBe 3
      }
    }

    "pullOption" should {
      "have right values while read vector non fill" in {
        val (q1, Some(x1)) = BoundedBucketQueue[Int](2).pushValues(2).pullOption()
        q1.queue.mkString("") shouldBe ""
        q1.bucket.mkString("") shouldBe ""
        x1 shouldBe 2

        val (q2, Some(x2)) = BoundedBucketQueue[Int](2).pushValues(2, 3).pullOption()
        q2.queue.mkString("") shouldBe "3"
        q2.bucket.mkString("") shouldBe ""
        x2 shouldBe 2
      }

      "have right values while write vector non fill" in {
        val (q1, Some(x1)) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3).pullOption()
        q1.queue.mkString("") shouldBe "3"
        q1.bucket.mkString("") shouldBe "1"
        x1 shouldBe 2

        val (q2, Some(x2)) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 3).pullOption()
        q2.queue.mkString("") shouldBe "33"
        q1.bucket.mkString("") shouldBe "1"
        x2 shouldBe 2
      }

      "after full rotate" in {
        val (q1, Some(x1)) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3, 4).pullOption()
        q1.queue.mkString("") shouldBe "4"
        q1.bucket.mkString("") shouldBe "21"
        x1 shouldBe 3

        val (q2, Some(x2)) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullOption()
        q2.queue.mkString("") shouldBe "45"
        q1.bucket.mkString("") shouldBe "21"
        x2 shouldBe 3
      }
    }

    "push and then pull" should {
      def deleteTest(queue: BoundedBucketQueue[Int], list: List[Int]): Unit = {
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
        var queue: BoundedBucketQueue[Int] = BoundedBucketQueue[Int](size)
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
        var queue = BoundedBucketQueue[Int](size)
        for (x <- xs) {
          queue = queue.push(x)
          list += x
          if (list.size > size)
            list.remove(0)

          queue.queue.iterator.toList shouldBe list.result()
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
        val (q1, xs1) = BoundedBucketQueue[Int](2).pushValues(2).pullAll(1)
        q1.queue.mkString("") shouldBe ""
        xs1 shouldBe List(2)

        val (q2, xs2) = BoundedBucketQueue[Int](2).pushValues(2).pullAll(2)
        q2.queue.mkString("") shouldBe ""
        xs2 shouldBe List(2)

        val (q3, xs3) = BoundedBucketQueue[Int](2).pushValues(2).pullAll(0)
        q3.queue.mkString("") shouldBe "2"
        xs3 shouldBe Nil
      }


      "have right values while write vector non fill" in {
        val (q1, xs1) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3).pullAll(1)
        q1.queue.mkString("") shouldBe "3"
        q1.bucket.mkString("") shouldBe "1"
        xs1 shouldBe List(2)

        val (q2, xs2) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 3).pullAll(2)
        q2.queue.mkString("") shouldBe "3"
        q2.bucket.mkString("") shouldBe "1"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q1, xs1) = BoundedBucketQueue[Int](2).pushValues(1, 2, 3, 4).pullAll(1)
        q1.queue.mkString("") shouldBe "4"
        q1.bucket.mkString("") shouldBe "21"
        xs1 shouldBe List(3)

        val (q2, xs2) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullAll(3)
        q2.queue.mkString("") shouldBe ""
        q2.bucket.mkString("") shouldBe "21"
        xs2 shouldBe List(3, 4, 5)
      }
    }

    "pull while" should {
      "have right values while read vector non fill" in {
        val (q2, xs2) = BoundedBucketQueue[Int](5).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.queue.mkString("") shouldBe "45"
        xs2 shouldBe List(1, 2, 3)
      }

      "have right values while write vector non fill" in {
        val (q2, xs2) = BoundedBucketQueue[Int](4).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.queue.mkString("") shouldBe "45"
        q2.bucket.mkString("") shouldBe "1"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q2, xs2) = BoundedBucketQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.queue.mkString("") shouldBe "45"
        q2.bucket.mkString("") shouldBe "21"
        xs2 shouldBe List(3)
      }
    }


    "head" should {
      "have right values while read vector non fill" in {
        BoundedBucketQueue[Int](2).queue.headOption shouldBe None

        (BoundedBucketQueue[Int](2) :+ 2).queue.headOption shouldBe Some(2)

        (BoundedBucketQueue[Int](2) :+ 2 :+ 3).queue.headOption shouldBe Some(2)
      }

      "have right values while write vector non fill" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).queue.headOption shouldBe Some(2)
        (BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).queue.headOption shouldBe Some(2)
      }

      "after full rotate" in {
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).queue.headOption shouldBe Some(3)
        (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).queue.headOption shouldBe Some(4)
      }

      "last" should {
        "have right values while read vector non fill" in {
          BoundedBucketQueue[Int](2).queue.lastOption shouldBe None

          (BoundedBucketQueue[Int](2) :+ 2).queue.lastOption shouldBe Some(2)

          (BoundedBucketQueue[Int](2) :+ 2 :+ 3).queue.lastOption shouldBe Some(3)
        }

        "have right values while write vector non fill" in {
          (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3).queue.lastOption shouldBe Some(3)
          (BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).queue.lastOption shouldBe Some(3)
        }

        "after full rotate" in {
          (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).queue.lastOption shouldBe Some(4)
          (BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).queue.lastOption shouldBe Some(5)
        }
      }

      "apply" should {
        "have right values while read vector non fill" in {
          val q1 = BoundedBucketQueue[Int](2) :+ 2
          q1.queue(0) shouldBe 2

          val q2 = BoundedBucketQueue[Int](2) :+ 2 :+ 3
          q2.queue(0) shouldBe 2
          q2.queue(1) shouldBe 3
        }

        "have right values while write vector non fill" in {
          val q1 = BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3
          q1.queue(0) shouldBe 2
          q1.queue(1) shouldBe 3

          val q2 = BoundedBucketQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3
          q2.queue(0) shouldBe 2
          q2.queue(1) shouldBe 3
          q2.queue(2) shouldBe 3
        }

        "after full rotate" in {
          val q1 = BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4
          q1.queue(0) shouldBe 3
          q1.queue(1) shouldBe 4

          val q2 = BoundedBucketQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5
          q2.queue(0) shouldBe 4
          q2.queue(1) shouldBe 5
        }
      }


    }

  }

}
