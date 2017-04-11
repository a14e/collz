package a14e.collz.concurrent

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps


class AsyncListSpec extends WordSpec with Matchers {
  "AsyncList" when {
    "represents internaly as cunstructed" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "in single batch" in new CommonWiring {
        val data = 1 to 10
        (data :: ANil).batches shouldBe Seq(data)
      }

      "in multiple batches" in new CommonWiring {
        val data1 = 1 to 10
        val data2 = 1 to 12
        (data1 :: data2 :: ANil).batches shouldBe Seq(data1, data2)
      }
    }

    "serially" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work on empty lists" in new CommonWiring {
        (Seq.empty[Int] :: Seq.empty[Int] :: ANil).serially.batches shouldBe Seq(Nil)
      }

      "work if empty lists in middle" in new CommonWiring {
        ((1 to 2) :: Seq.empty[Int] :: Seq.empty[Int] :: (3 to 4) :: ANil)
          .serially.batches shouldBe Seq(Seq(1), Seq(2), Seq(3), Seq(4))
      }

      "work on single batch lists" in new CommonWiring {
        ((1 to 5) :: ANil).serially.batches shouldBe Seq(Seq(1), Seq(2), Seq(3), Seq(4), Seq(5))
      }


      "work on multiple batches lists" in new CommonWiring {
        ((1 to 2) :: (3 to 4) :: ANil).serially.batches shouldBe Seq(Seq(1), Seq(2), Seq(3), Seq(4))
      }

      "work on infinity lists" in new CommonWiring {
        (Stream.from(1) :: ANil).serially.take(4).batches shouldBe Seq(Seq(1), Seq(2), Seq(3), Seq(4))
      }
    }

    "batched" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work on empty lists" in new CommonWiring {
        (Seq.empty[Int] :: Seq.empty[Int] :: ANil).batched(2).batches shouldBe Seq(Nil)
      }

      "create same batched if data fits to batched" in new CommonWiring {
        ((1 to 2) :: (3 to 4) :: ANil).batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3, 4))
      }

      "create same batched if data almost fits to batched" in new CommonWiring {
        ((1 to 2) :: (3 to 4) :: Seq(1) :: ANil).batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3, 4), Seq(1))
      }

      "split single collection to batches" in new CommonWiring {
        (Seq(1) :: Seq(2) :: Seq(3) :: ANil).batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3))
      }

      "collect single collection to batches" in new CommonWiring {
        ((1 to 4) :: ANil).batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3, 4))
      }
      "collect single collection to batches if not fit to size" in new CommonWiring {
        ((1 to 5) :: ANil).batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3, 4), Seq(5))
      }

      "collect multiple collections different size data to batches" in new CommonWiring {
        ((1 to 3) :: Seq(4) :: Seq(1, 5, 6, 7) :: ANil)
          .batched(2).batches shouldBe Seq(Seq(1, 2), Seq(3, 4), Seq(1, 5), Seq(6, 7))
      }


      "collect serrialy different size data to batches if data does not fit to size" in new CommonWiring {
        ((1 to 3) :: Seq(4) :: Seq(1, 5, 6, 7) :: ANil)
          .batched(3).batches shouldBe Seq(Seq(1, 2, 3), Seq(4, 1, 5), Seq(6, 7))

      }

      "work on infinity lists" in new CommonWiring {
        (Stream.from(1) :: ANil)
          .batched(2)
          .take(5)
          .batches shouldBe Seq(Seq(1, 2), Seq(3, 4), Seq(5))
      }

      "work on by one" in new CommonWiring {
        (Stream.from(1) :: ANil)
          .batched(4)
          .batched(1)
          .batched(2)
          .take(5)
          .batches shouldBe Seq(Seq(1, 2), Seq(3, 4), Seq(5))
      }
    }


    "map" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work correctly" in new CommonWiring {
        ((1 to 4) :: ANil).map(_ + 1).run.sync() shouldBe (2 to 5)
      }

      "work on infinity lists" in new CommonWiring {
        (Stream.from(1) :: ANil).map(_ + 1).take(10).run.sync() shouldBe (2 to 11)
      }
    }


    "filter" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work correctly" in new CommonWiring {
        ((1 to 20) :: ANil).filter(_ % 2 == 0).run.sync() shouldBe (1 to 20).filter(_ % 2 == 0)
      }

      "work correctly on empty res" in new CommonWiring {
        ((1 to 20) :: ANil).filter(_ => false).run.sync() shouldBe Nil
      }
    }

    "flatMap" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work correctly" in new CommonWiring {
        ((1 to 20) :: ANil).flatMap(x => Seq(x, x)).run.sync() shouldBe (1 to 20).flatMap(x => Seq(x, x))
      }

      "work correctly on empty res" in new CommonWiring {
        ((1 to 20) :: ANil).flatMap(_ => Nil).run.sync() shouldBe Nil
      }
    }

    "flatten" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work correctly" in new CommonWiring {
        (Seq(1 to 20) :: ANil).flatten.run.sync() shouldBe (1 to 20)
      }


      "work correctly on empty res" in new CommonWiring {
        (Seq(Seq.empty[Int]) :: ANil).flatten.run.sync() shouldBe Nil
      }
    }

    "collect" should {
      import scala.concurrent.ExecutionContext.Implicits.global
      "work correctly without filtering" in new CommonWiring {
        ((1 to 20) :: (1 to 20) :: ANil)
          .collect { case x => x + 1 }.run.sync() shouldBe ((1 to 20) ++ (1 to 20)).map(_ + 1)
      }


      "work correctly with filtering" in new CommonWiring {
        ((1 to 20) :: (1 to 20) :: ANil)
          .collect { case x if x % 2 == 0 => x + 1 }
          .run
          .sync() shouldBe ((1 to 20) ++ (1 to 20)).collect { case x if x % 2 == 0 => x + 1 }
      }
    }
  }


  abstract class CommonWiring {

    implicit class RichTestingFuture[T](future: Future[T]) {
      def sync(): T = Await.result[T](future, awaitTimeout)
    }

    implicit class RichAsyncList[T](alist: AsyncList[T]) {
      def batches(implicit context: ExecutionContext): Seq[Seq[T]] = alist.batchMap(_ :: Nil).run.sync()
    }

    implicit val awaitTimeout: FiniteDuration = 10 seconds
  }

}
