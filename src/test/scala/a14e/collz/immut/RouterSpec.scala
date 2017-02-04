package a14e.collz.immut

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

/**
  * Created by m0hct on 28.01.2017.
  */
class RouterSpec extends WordSpec with Matchers {


  "Router" should {
    "create" in {
      val values = 1 to 3
      val created = Router(values: _*)
      created.size shouldBe 3
      created should contain theSameElementsAs values
    }

    "add" when {
      "new elems" in {
        val values = 1 to 3
        val created = Router(values: _*) + 4 + 5
        created.size shouldBe 5
        created should contain theSameElementsAs (1 to 5)
      }

      "old elements" in {
        val values = 1 to 3
        val created = Router(values: _*) + 2 + 3
        created.size shouldBe 3
        created should contain theSameElementsAs (1 to 3)
      }

      "new and old elements" in {
        val values = 1 to 3
        val created = Router(values: _*) + 3 + 4
        created.size shouldBe 4
        created should contain theSameElementsAs (1 to 4)
      }

    }

    "add All" when {
      "new elems" in {
        val values = 1 to 3
        val created = Router(values: _*) ++ (4 to 7)
        created.size shouldBe 7
        created should contain theSameElementsAs (1 to 7)
      }

      "old elements" in {
        val values = 1 to 3
        val created = Router(values: _*) ++ (1 to 3)
        created.size shouldBe 3
        created should contain theSameElementsAs (1 to 3)
      }

      "new and old elements" in {
        val values = 1 to 3
        val created = Router(values: _*) ++ (2 to 6)
        created.size shouldBe 6
        created should contain theSameElementsAs (1 to 6)
      }
    }

    "add and then delete shoudl contain the same elements" in {
      val router1 = Router[Int]() + 1 + 2 + 4 - 2 + 8 + 7 - 1
      val router2 = Router[Int]() + 1 + 2 + 7 - 2 + 8 + 4 - 1

      router1.toList should contain theSameElementsInOrderAs router2.toList
    }

    "route" when {
      "should route to all" in {
        val router = Router(1 to 5: _*)

        val data = 1 to 1000
        val found = data.map(router.route)
        found.toSet.size shouldBe router.size
      }

      "should route to all by rand numb" in {
        val router = Router(1 to 10: _*)

        val data = (1 to 200).map(_ => Random.nextInt())
        val found = data.map(router.route)
        found.toSet.size shouldBe router.size
      }

      "should find each time same values" in {
        val router = Router(1 to 5: _*)

        val data = 1 to 1000
        val found1 = data.map(router.route)
        val found2 = data.map(router.route)
        found1 should contain theSameElementsInOrderAs found2
      }

      "should find same values for different order of add" in {
        val router1 = Router[Int]() + 1 + 2 + 4 - 2 + 8 + 7 - 1
        val router2 = Router[Int]() + 1 + 2 + 7 - 2 + 8 + 4 - 1
        for (_ <- 0 to 100) {
          val key = Random.nextInt()
          router1.route(key) shouldBe router2.route(key)
        }

      }

      "route on empty" in {
        intercept[UnsupportedOperationException] {
          Router[Int]().route(0)
        }
      }
    }

    "can build from test" in {
      import Router.canBuildFrom
      Router(1 to 5: _*).map(_ + 1).toList shouldBe (2 to 6).toList
    }

    "builder" in {
      val builder = Router.newBuilder[Int]
      builder += 1
      builder += 2
      builder ++= List(3, 4)
      builder.result().toList shouldBe List(1, 2, 3, 4)

      builder.clear()
      builder.result().toList shouldBe Nil
    }

    "remove" when {
      "remove if exists" in {

        val router = Router(1 to 5: _*) - 2

        router should contain theSameElementsAs List(1, 3, 4, 5)
      }

      "dont remove if not exists" in {
        val router = Router(1 to 5: _*) - 6

        router should contain theSameElementsAs (1 to 5)

      }

      "remove to empty" in {
        val router = Router(1 to 3: _*) - 1 - 2 - 3
        router.size shouldBe 0
        router.isEmpty shouldBe true
      }
    }
  }
}
