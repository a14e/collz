package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by Andrew on 18.12.2016.
  */
class FixedQueueSpec extends WordSpec with Matchers {
  "Tests for FixedQueue " when {

    "valid size" should {
      "empty" in {

        FixedQueue[Int](10).size shouldBe 0

        FixedQueue[Int](1).size shouldBe 0

        FixedQueue[Int](10).isEmpty shouldBe true

        FixedQueue[Int](1).isEmpty shouldBe true
      }

      "while read vector non fill" in {
        FixedQueue[Int](2).size shouldBe 0

        (FixedQueue[Int](2) :+ 1).size shouldBe 1

        (FixedQueue[Int](2) :+ 1 :+ 2).size shouldBe 2

      }

      "while write vector non fill" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3).size shouldBe 2
        (FixedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).size shouldBe 3
      }

      "after full rotate" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).size shouldBe 2
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).size shouldBe 2
      }
    }

    "push" should {
      "have right values while read vector non fill" in {
        FixedQueue[Int](2).mkString("") shouldBe ""

        (FixedQueue[Int](2) :+ 2).mkString("") shouldBe "2"

        (FixedQueue[Int](2) :+ 2 :+ 3).mkString("") shouldBe "23"
      }

      "have right values while write vector non fill" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3).mkString("") shouldBe "23"
        (FixedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).mkString("") shouldBe "233"
      }

      "after full rotate" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).mkString("") shouldBe "34"
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).mkString("") shouldBe "45"
      }
    }

    "push all" should {
      "have right values while read vector non fill" in {
        FixedQueue[Int](2).mkString("") shouldBe ""

        FixedQueue[Int](2).pushValues(2).mkString("") shouldBe "2"

        FixedQueue[Int](2).pushValues(2, 3).mkString("") shouldBe "23"
      }

      "have right values while write vector non fill" in {
        FixedQueue[Int](2).pushValues(1, 2, 3).mkString("") shouldBe "23"
        FixedQueue[Int](3).pushValues(1, 2, 3, 3).mkString("") shouldBe "233"
      }

      "after full rotate" in {
        FixedQueue[Int](2).pushValues(1, 2, 3, 4).mkString("") shouldBe "34"
        FixedQueue[Int](2).pushValues(1, 2, 3, 4, 5).mkString("") shouldBe "45"
      }
    }

    "pull" should {
      "have right values while read vector non fill" in {
        val (q1, x1) = FixedQueue[Int](2).pushValues(2).pull()
        q1.mkString("") shouldBe ""
        x1 shouldBe 2

        val (q2, x2) = FixedQueue[Int](2).pushValues(2, 3).pull()
        q2.mkString("") shouldBe "3"
        x2 shouldBe 2
      }

      "have right values while write vector non fill" in {
        val (q1, x1) = FixedQueue[Int](2).pushValues(1, 2, 3).pull()
        q1.mkString("") shouldBe "3"
        x1 shouldBe 2

        val (q2, x2) = FixedQueue[Int](3).pushValues(1, 2, 3, 3).pull()
        q2.mkString("") shouldBe "33"
        x2 shouldBe 2
      }

      "after full rotate" in {
        val (q1, x1) = FixedQueue[Int](2).pushValues(1, 2, 3, 4).pull()
        q1.mkString("") shouldBe "4"
        x1 shouldBe 3

        val (q2, x2) = FixedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pull()
        q2.mkString("") shouldBe "45"
        x2 shouldBe 3
      }
    }

    "pull all" should {
      "have right values while read vector non fill" in {
        val (q1, xs1) = FixedQueue[Int](2).pushValues(2).pullAll(1)
        q1.mkString("") shouldBe ""
        xs1 shouldBe List(2)

        val (q2, xs2) = FixedQueue[Int](2).pushValues(2).pullAll(2)
        q2.mkString("") shouldBe ""
        xs2 shouldBe List(2)

        val (q3, xs3) = FixedQueue[Int](2).pushValues(2).pullAll(0)
        q3.mkString("") shouldBe "2"
        xs3 shouldBe Nil
      }


      "have right values while write vector non fill" in {
        val (q1, xs1) = FixedQueue[Int](2).pushValues(1, 2, 3).pullAll(1)
        q1.mkString("") shouldBe "3"
        xs1 shouldBe List(2)

        val (q2, xs2) = FixedQueue[Int](3).pushValues(1, 2, 3, 3).pullAll(2)
        q2.mkString("") shouldBe "3"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q1, xs1) = FixedQueue[Int](2).pushValues(1, 2, 3, 4).pullAll(1)
        q1.mkString("") shouldBe "4"
        xs1 shouldBe List(3)

        val (q2, xs2) = FixedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullAll(3)
        q2.mkString("") shouldBe ""
        xs2 shouldBe List(3, 4, 5)
      }
    }

    "pull while" should {
      "have right values while read vector non fill" in {
        val (q2, xs2) = FixedQueue[Int](5).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(1, 2, 3)
      }

      "have right values while write vector non fill" in {
        val (q2, xs2) = FixedQueue[Int](4).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(2, 3)
      }

      "after full rotate" in {
        val (q2, xs2) = FixedQueue[Int](3).pushValues(1, 2, 3, 4, 5).pullWhile(_ < 4)
        q2.mkString("") shouldBe "45"
        xs2 shouldBe List(3)
      }
    }


    "head" should {
      "have right values while read vector non fill" in {
        FixedQueue[Int](2).headOption shouldBe None

        (FixedQueue[Int](2) :+ 2).headOption shouldBe Some(2)

        (FixedQueue[Int](2) :+ 2 :+ 3).headOption shouldBe Some(2)
      }

      "have right values while write vector non fill" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3).headOption shouldBe Some(2)
        (FixedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).headOption shouldBe Some(2)
      }

      "after full rotate" in {
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).headOption shouldBe Some(3)
        (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).headOption shouldBe Some(4)
      }

      "last" should {
        "have right values while read vector non fill" in {
          FixedQueue[Int](2).lastOption shouldBe None

          (FixedQueue[Int](2) :+ 2).lastOption shouldBe Some(2)

          (FixedQueue[Int](2) :+ 2 :+ 3).lastOption shouldBe Some(3)
        }

        "have right values while write vector non fill" in {
          (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3).lastOption shouldBe Some(3)
          (FixedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3).lastOption shouldBe Some(3)
        }

        "after full rotate" in {
          (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4).lastOption shouldBe Some(4)
          (FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5).lastOption shouldBe Some(5)
        }
      }

      "apply" should {
        "have right values while read vector non fill" in {
          val q1 = FixedQueue[Int](2) :+ 2
          q1(0) shouldBe 2

          val q2 = FixedQueue[Int](2) :+ 2 :+ 3
          q2(0) shouldBe 2
          q2(1) shouldBe 3
        }

        "have right values while write vector non fill" in {
          val q1 = FixedQueue[Int](2) :+ 1 :+ 2 :+ 3
          q1(0) shouldBe 2
          q1(1) shouldBe 3

          val q2 = FixedQueue[Int](3) :+ 1 :+ 2 :+ 3 :+ 3
          q2(0) shouldBe 2
          q2(1) shouldBe 3
          q2(2) shouldBe 3
        }

        "after full rotate" in {
          val q1 = FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4
          q1(0) shouldBe 3
          q1(1) shouldBe 4

          val q2 = FixedQueue[Int](2) :+ 1 :+ 2 :+ 3 :+ 4 :+ 5
          q2(0) shouldBe 4
          q2(1) shouldBe 5
        }
      }

    }

  }
}
