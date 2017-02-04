package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.TreeSet

/**
  * Created by m0hct on 04.02.2017.
  */
class BucketRouterSpec extends WordSpec with Matchers {

  "Tests for BucketRouter " when {

    "Bucket" should {
      "is empty" in {

        Bucket.empty[Int].isEmpty shouldBe true

        Bucket(1 -> List(1)).withoutNode(1).isEmpty shouldBe true
      }

      "route by key" in {
        val b = Bucket(1 -> List(2, 3))

        (1 to 100).map(b.routeByKey).toSet shouldBe Set(1, 2, 3)
      }

      "with node" in {
        Bucket(1 -> List(2, 3)).withNode(4) shouldBe Bucket(Some(1), TreeSet(1 to 4: _*))

        Bucket.empty[Int].withNode(1) shouldBe Bucket(Some(1), TreeSet(1))
      }

      "+" in {
        (Bucket(1 -> List(2, 3)) + 4) shouldBe Bucket(Some(1), TreeSet(1 to 4: _*))


        (Bucket.empty[Int] + 4) shouldBe Bucket(Some(4), TreeSet(4))
      }

      "with Master" in {
        Bucket(1 -> List(2, 3)).withMaster(4) shouldBe Bucket(Some(4), TreeSet(1 to 4: _*))
      }

      "without node" in {
        Bucket(1 -> List(2, 3)).withoutNode(2) shouldBe Bucket(Some(1), TreeSet(1, 3))

        Bucket(1 -> List(2, 3)).withoutNode(1) shouldBe Bucket(Some(2), TreeSet(2, 3))


        (Bucket(1 -> List(1)) withoutNode 1) shouldBe Bucket.empty[Int]

        (Bucket.empty[Int] withoutNode 1) shouldBe Bucket.empty[Int]

      }

      "-" in {
        (Bucket(1 -> List(2, 3)) - 2) shouldBe Bucket(Some(1), TreeSet(1, 3))

        (Bucket(1 -> List(2, 3)) - 1) shouldBe Bucket(Some(2), TreeSet(2, 3))

        (Bucket(1 -> List(1)) - 1) shouldBe Bucket.empty[Int]

        (Bucket.empty[Int] - 1) shouldBe Bucket.empty[Int]
      }

      "empty" in {
        Bucket.empty[Int].isEmpty shouldBe true
        Bucket.empty[Int].nodes.isEmpty shouldBe true
        Bucket.empty[Int].master.isEmpty shouldBe true
      }

      "apply" in {
        val b = Bucket(1 -> List(2, 3))
        b.nodes.toList shouldBe List(1, 2, 3)
        b.master shouldBe Some(1)

        b.isEmpty shouldBe false
      }
    }

    "router test" should {
      "apply" in {
        val created = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )
        val expectedBucket1 = Bucket(Some(1), TreeSet(1, 2, 3))
        val expectedBucket2 = Bucket(Some(5), TreeSet(5, 6, 7, 8))
        created shouldBe BucketRouter(Vector(expectedBucket1, expectedBucket2))


        BucketRouter[Int](2).bucketNumber shouldBe 2
        BucketRouter[Int](2).buckets.forall(_.isEmpty) shouldBe true
      }

      "empty" in {
        BucketRouter[Int]().buckets.isEmpty shouldBe true
        BucketRouter.empty[Int].buckets.isEmpty shouldBe true
      }


      "bucket at" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )

        router.bucketAt(0) shouldBe Bucket(1 -> List(1, 2, 3))
        router.bucketAt(1) shouldBe Bucket(5 -> List(6, 7, 8))
      }

      "route" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )

        (1 to 10).flatMap(router.routeOption).toSet shouldBe router.buckets.flatMap(_.master).toSet
      }

      "index by hash" in {
        val router = BucketRouter[Int](2)
        (1 to 10).map(x => router.indexByHash(x)).toSet shouldBe Set(0, 1)
      }

      "bucket for key" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )

        (1 to 10).map(router.bucketForKey(_)).toSet shouldBe router.buckets.toSet
      }


      "index by elem" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )
        (1 to 3).flatMap(router.bucketIndexByElem).toSet shouldBe Set(0)


        (5 to 8).flatMap(router.bucketIndexByElem).toSet shouldBe Set(1)


        (9 to 12).flatMap(router.bucketIndexByElem).toSet shouldBe Set()
      }

      "bucket by elem" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          5 -> List(6, 7, 8)
        )
        (1 to 3).flatMap(router.bucketByElem).toSet shouldBe Set(Bucket(1 -> List(1, 2, 3)))


        (5 to 8).flatMap(router.bucketByElem).toSet shouldBe Set(Bucket(5 -> List(6, 7, 8)))


        (9 to 12).flatMap(router.bucketByElem).toSet shouldBe Set()
      }

      "empty bucket index" in {
        val router = BucketRouter[Int](
          1 -> List(1, 2, 3),

          4 -> List(4)
        )

        router.emptyBucketIndex() shouldBe None
        router.withoutEll(1, 4).emptyBucketIndex() shouldBe Some(1)
      }

      "with new bucket" in {
        BucketRouter[Int](
          1 -> List(1, 2, 3),

          4 -> List(4)
        ).withNewBucket(0, Bucket(7 -> List(7))) shouldBe BucketRouter(7 -> List(7), 4 -> List(4))
      }

      "with el" in {
        BucketRouter[Int](
          7 -> List(7),

          4 -> List(4)
        ).withEll(0, 10) shouldBe BucketRouter(7 -> List(7, 10), 4 -> List(4))
      }

      "without el" in {
        BucketRouter[Int](
          7 -> List(7, 10),

          4 -> List(4)
        ).withoutEll(0, 10) shouldBe BucketRouter(7 -> List(7), 4 -> List(4))
      }

      "with master" in {
        BucketRouter[Int](
          7 -> List(7),

          4 -> List(4)
        ).withMaster(0, 10) shouldBe BucketRouter(10 -> List(7, 10), 4 -> List(4))
      }
    }
  }

}
