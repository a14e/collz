/*
* This source code is licensed under the MIT license found in the
* LICENSE.txt file in the root directory of this source tree
*/
package a14e.collz.concurrent

import a14e.collz.concurrent.AsyncList.{Computation, ComputationResult, TailBuilder}

import scala.collection.{GenTraversableOnce, TraversableOnce}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.{ExecutionContext, Future}


trait AsyncList[+T] extends ResultHandler[Seq[T]] {
  self =>

  def ::[B >: T](newBachHead: Seq[B]): AsyncList[B] = {
    def newComputation(context: ExecutionContext): Future[(Future[Seq[B]], TailBuilder[B])] = {
      val tailBuilder: AsyncList.TailBuilder[B] = _ => Future.successful(self)
      val futureHead = Future.successful(newBachHead)
      Future.successful((futureHead, tailBuilder))
    }

    new AsyncListImpl[B](newComputation)
  }

  def map[B](f: T => B): AsyncList[B] = batchMap[B](_.map(f))

  def filter(f: T => Boolean): AsyncList[T] = batchMap[T](_.filter(f))

  def flatMap[B](f: T => GenTraversableOnce[B]): AsyncList[B] = batchMap[B](_.flatMap(f))

  def flatten[B](implicit conv: T => GenTraversableOnce[B]): AsyncList[B] = batchMap[B](_.flatten)

  def collect[B](f: PartialFunction[T, B]): AsyncList[B] = batchMap[B](_.collect(f))


  def mapAsync[B](f: T => Future[B]): AsyncList[B] =
    batchMapAsync[B] { (seq, ex) =>
      implicit val context = ex
      Future.traverse(seq)(f)
    }

  def filterAsync(test: T => Future[Boolean]): AsyncList[T] =
    batchMapAsync[T] { (seq, ex) =>
      implicit val context = ex
      seq.foldLeft(Future.successful(new ListBuffer[T]())) {
        (futureBuf, current) =>
          val testFuture = test(current)
          for {
            buffer <- futureBuf
            testRes <- testFuture
          } yield {
            if (testRes) buffer += current
            buffer
          }
      }.map(_.result())
    }

  def flatMapAsync[B](f: T => Future[GenTraversableOnce[B]]): AsyncList[B] =
    batchMapAsync[B] { (seq, ex) =>
      implicit val context = ex
      Future.traverse(seq)(f).map(_.flatten)
    }

  def collectAsync[B](f: PartialFunction[T, Future[B]]): AsyncList[B] =
    batchMapAsync[B] { (seq, ex) =>
      implicit val context = ex
      Future.sequence(seq.collect(f))
    }

  def find(cond: T => Boolean): ResultHandler[Option[T]] = {
    foldLeftImpl(Option.empty[T]) { (prev, batch) =>
      val found = prev.orElse(batch.find(cond))
      (found, found.isEmpty)
    }
  }

  def isEmpty: ResultHandler[Boolean] = {
    foldLeftImpl(false) { (_, batch) =>
      val res = batch.isEmpty
      (res, !res)
    }
  }

  def nonEmpty: ResultHandler[Boolean] = {
    FunctionResultHandler[Boolean] { implicit ctx => isEmpty.run.map(x => !x) }
  }

  def length: ResultHandler[Long] = {
    foldLeftImpl(0L) { (prevLen, batch) =>
      (prevLen + batch.length, true)
    }
  }

  def exists(cond: T => Boolean): ResultHandler[Boolean] = {
    val found = find(cond)
    FunctionResultHandler[Boolean](ctx => found.run(ctx).map(_.isDefined)(ctx))
  }

  def forall(cond: T => Boolean): ResultHandler[Boolean] =
    FunctionResultHandler[Boolean] { implicit ctx =>
      exists(x => !cond(x)).run.map(x => !x)
    }

  def foldLeft[B](init: B)(cond: (B, T) => B): ResultHandler[B] = {
    foldLeftImpl[B](init) { (prev, batch) =>
      val res = batch.foldLeft(prev)(cond)
      (res, true)
    }
  }

  def foreach(f: T => Unit): ResultHandler[Unit] = {
    foldLeftImpl((): Unit) { (_, current) =>
      current.foreach(f)
      (Unit, true)
    }
  }

  def count(cond: T => Boolean): ResultHandler[Long] = {
    foldLeft(0L)((prev, current) => if (cond(current)) prev + 1 else prev)
  }

  def headOption: ResultHandler[Option[T]] = find(_ => true)

  def findAsync(cond: T => Future[Boolean]): ResultHandler[Option[T]] = {
    foldLeftAsyncImpl(Option.empty[T]) { (_, batch, context) =>
      implicit val ctx = context
      for {
        found <- AsyncList.findAsync(batch)(cond)
      } yield (found, found.isEmpty)
    }
  }

  def existsAsync(cond: T => Future[Boolean]): ResultHandler[Boolean] = {
    val found = findAsync(cond)
    new FunctionResultHandler[Boolean](ctx => found.run(ctx).map(_.isDefined)(ctx))
  }

  def forallAsync(cond: T => Future[Boolean]): ResultHandler[Boolean] = {
    FunctionResultHandler[Boolean] { implicit ctx =>
      existsAsync(x => cond(x).map(res => !res)).run.map(x => !x)
    }
  }

  def foldLeftAsync[B](init: B)(cond: (B, T) => Future[B]): ResultHandler[B] = {
    foldLeftAsyncImpl[B](init) { (prev, batch, context) =>
      implicit val ctx = context
      for (res <- AsyncList.foldLeftAsync(batch, prev)(cond))
        yield (res, true)
    }
  }

  def countAsync(cond: T => Future[Boolean]): ResultHandler[Long] = {
    FunctionResultHandler[Long] { implicit ctx =>
      foldLeftAsync(0L)((prev, current) => cond(current).map(res => if (res) prev + 1 else prev)).run
    }
  }


  def force(implicit context: ExecutionContext): AsyncList[T] = {
    AsyncList.async[T](run)
  }

  override def run(implicit context: ExecutionContext): Future[Seq[T]] = {
    foldLeftImpl(new ListBuffer[T]()) { (buffer, batch) =>
      buffer ++= batch
      (buffer, true)
    }.run.map(_.result())
  }

  def serially: AsyncList[T]

  def batched(batchSize: Int): AsyncList[T]

  def takeWhile(cond: T => Boolean): AsyncList[T]

  def dropWhile(cond: T => Boolean): AsyncList[T]

  def takeWhileAsync(cond: T => Future[Boolean]): AsyncList[T]

  def dropWhileAsync(cond: T => Future[Boolean]): AsyncList[T]

  def sequence: AsyncList[T] = AsyncList.asyncComputation(run(_))

  def take(count: Int): AsyncList[T]

  def drop(count: Int): AsyncList[T]

  def batchMap[B](f: Seq[T] => Seq[B]): AsyncList[B]

  def batchMapAsync[B](f: (Seq[T], ExecutionContext) => Future[Seq[B]]): AsyncList[B]

  protected[concurrent] def computation[B >: T](context: ExecutionContext): ComputationResult[B]


  protected[concurrent] def foldLeftImpl[B](init: B)(calc: (B, Seq[T]) => (B, Boolean)): ResultHandler[B]

  protected[concurrent] def foldLeftAsyncImpl[B](init: B)
                                                (calc: (B, Seq[T], ExecutionContext) => Future[(B, Boolean)]): ResultHandler[B]
}

object AsyncList {
  type TailBuilder[T] = ExecutionContext => Future[AsyncList[T]]
  type ComputationResult[T] = Future[(Future[Seq[T]], TailBuilder[T])]
  type Computation[T] = ExecutionContext => ComputationResult[T]

  def apply[T](seq: Seq[T]): AsyncList[T] = seq :: ANil

  def async[T](futureSeq: Future[Seq[T]]): AsyncList[T] = {
    asyncComputation[T](_ => futureSeq)
  }

  def serially[T](seq: Seq[T]): AsyncList[T] = (seq :: ANil).serially

  def batched[T](seq: Seq[T], batchSize: Int): AsyncList[T] = (seq :: ANil).batched(batchSize)

  def asyncComputation[T](futureSeq: ExecutionContext => Future[Seq[T]]): AsyncList[T] = {
    def newComputation(context: ExecutionContext): Future[(Future[Seq[T]], TailBuilder[T])] = {
      val tailBuilder: AsyncList.TailBuilder[T] = _ => Future.successful(ANil)
      Future.successful((futureSeq(context), tailBuilder))
    }

    new AsyncListImpl[T](newComputation)
  }

  def findAsync[T](seq: Seq[T])
                  (cond: T => Future[Boolean])
                  (implicit ctx: ExecutionContext): Future[Option[T]] = {

    def recursiveSearch(iter: Iterator[T]): Future[Option[T]] = {
      if (!iter.hasNext) Future.successful(Option.empty[T])
      else {
        val current = iter.next()
        for {
          currentRes <- cond(current)
          totalRes <- {
            if (currentRes) Future.successful(Some(current))
            else recursiveSearch(iter)
          }
        } yield totalRes
      }
    }

    recursiveSearch(seq.toIterator)
  }

  def spanAsync[T](seq: Seq[T],
                   cond: T => Future[Boolean])
                  (implicit ctx: ExecutionContext): Future[(Seq[T], Seq[T])] = {

    def recSpanAsync(buffer: ListBuffer[T],
                     prev: Stream[T],
                     cond: T => Future[Boolean])
                    (implicit ctx: ExecutionContext): Future[(Seq[T], Seq[T])] = prev match {
      case Stream.Empty => Future.successful((buffer.result(), Seq.empty[T]))
      case head #:: tail =>
        for {
          testRes <- cond(head)
          res <- {
            if (testRes) {
              buffer += head
              recSpanAsync(buffer, prev, cond)
            } else
              Future.successful((buffer.result(), tail))
          }
        } yield res
    }

    recSpanAsync(new ListBuffer[T](), seq.toStream, cond)
  }

  def existsAsync[T](seq: Seq[T])
                    (cond: T => Future[Boolean])
                    (implicit ctx: ExecutionContext): Future[Boolean] = {
    findAsync(seq)(cond).map(_.isDefined)
  }


  def forallAsync[T](seq: Seq[T])
                    (cond: T => Future[Boolean])
                    (implicit ctx: ExecutionContext): Future[Boolean] = {
    existsAsync(seq)(x => cond(x).map(res => !res)).map(x => !x)
  }


  def foldLeftAsync[T, B](seq: Seq[T], init: B)
                         (cond: (B, T) => Future[B])
                         (implicit ctx: ExecutionContext): Future[B] = {

    def recursiveSearch(prev: B, iter: Iterator[T]): Future[B] = {
      if (!iter.hasNext) Future.successful(prev)
      else {
        val current = iter.next()
        cond(prev, current).flatMap(res => recursiveSearch(res, iter))
      }
    }

    recursiveSearch(init, seq.toIterator)
  }


  protected[concurrent] def batchedImpl[T](batchSize: Int,
                                           leftSeq: Seq[T],
                                           tailBuilder: TailBuilder[T],
                                           batchBuffer: ArrayBuffer[T],
                                           context: ExecutionContext): Future[AsyncList[T]] = {

    def resultWithSeq(seq: Seq[T], tail: Seq[T]): Future[AsyncList[T]] = {
      def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
        val newHeadBatch = Future.successful(seq)

        def newTailBuilder(ctx: ExecutionContext): Future[AsyncList[T]] = {
          batchedImpl(batchSize, tail, tailBuilder, new ArrayBuffer[T](batchSize), ctx)
        }

        Future.successful((newHeadBatch, newTailBuilder))
      }

      Future.successful(new AsyncListImpl[T](newComputation))
    }

    leftSeq.toStream match {
      case Stream.Empty if batchBuffer.isEmpty =>
        tailBuilder(context).map(_.batched(batchSize))(context)
      case Stream.Empty if batchBuffer.nonEmpty =>
        if (batchBuffer.size == batchSize) resultWithSeq(batchBuffer, Nil)
        else {
          implicit val ctx = context
          tailBuilder(context).flatMap {
            case ANil => Future.successful(batchBuffer :: ANil)
            case alist: AsyncList[T] =>
              alist.computation[T](context).flatMap {
                case (headBatchFuture, nestedTailBuilder) =>
                  for {
                    headBatch <- headBatchFuture
                    resAlist <- batchedImpl(batchSize, headBatch, nestedTailBuilder, batchBuffer, context)
                  } yield resAlist
              }
          }
        }
      case stream =>
        val leftSize = batchSize - batchBuffer.size
        val (headBatch, tail) = stream.splitAt(leftSize)
        batchBuffer ++= headBatch
        if (batchBuffer.size == batchSize) resultWithSeq(batchBuffer, tail)
        else batchedImpl[T](batchSize, tail, tailBuilder, batchBuffer, context)
    }
  }
}

class AsyncListImpl[+T](_computation: Computation[T]) extends AsyncList[T] {

  override def takeWhile(cond: T => Boolean): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            (prev, end) = headBatch.span(cond)
            res <- {
              if (prev.isEmpty) {
                ANil.computation[T](ctx)
              } else if (end.isEmpty) {
                val nextBatch = Future.successful(prev)

                def nextBuilder(c: ExecutionContext): Future[AsyncList[T]] = tailBuilder(c).map(_.takeWhile(cond))

                Future.successful((nextBatch, nextBuilder)): ComputationResult[T]
              } else {
                (prev :: ANil).computation[T](ctx)
              }
            }
          } yield res
      }
    }

    new AsyncListImpl[T](newComputation)
  }

  override def dropWhile(cond: T => Boolean): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            (_, end) = headBatch.span(cond)
            res <- {
              if (end.isEmpty) {
                tailBuilder(ctx).map(_.dropWhile(cond)).flatMap(_.computation[T](ctx))
              } else {
                val nextHead = Future.successful(end)
                Future.successful((nextHead, tailBuilder))
              }
            }
          } yield res
      }
    }

    new AsyncListImpl[T](newComputation)
  }

  override def take(count: Int): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation[T](ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            taken = headBatch.take(count)
            res <- {
              val takenLen = taken.length
              if (takenLen == count) {
                (taken :: ANil).computation[T](ctx)
              } else {
                tailBuilder(ctx).flatMap(x => (taken :: x.take(count - takenLen)).computation[T](ctx))
              }
            }
          } yield res
      }
    }

    if (count <= 0) ANil
    else new AsyncListImpl[T](newComputation)
  }

  override def drop(count: Int): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            dropped = headBatch.drop(count)
            res <- {
              if (dropped.nonEmpty) {
                val droppedFuture = Future.successful(dropped)
                Future.successful((droppedFuture, tailBuilder))
              } else {
                val currentBatchLen = headBatch.size
                tailBuilder(ctx).flatMap(_.drop(count - currentBatchLen).computation[T](ctx))
              }
            }
          } yield res
      }
    }

    if (count <= 0) this
    else new AsyncListImpl[T](newComputation)
  }


  override def takeWhileAsync(cond: T => Future[Boolean]): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation[T](ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            (prev, end) <- AsyncList.spanAsync(headBatch, cond)
            res <- {
              if (prev.isEmpty) ANil.computation[T](ctx)
              else if (end.isEmpty) {
                val nextBatch = Future.successful(prev)

                def nextBuilder(c: ExecutionContext): Future[AsyncList[T]] = tailBuilder(c).map(_.takeWhileAsync(cond))

                Future.successful((nextBatch, nextBuilder)): ComputationResult[T]
              } else (prev :: ANil).computation[T](ctx)
            }
          } yield res
      }
    }

    new AsyncListImpl[T](newComputation)
  }


  override def dropWhileAsync(cond: T => Future[Boolean]): AsyncList[T] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[T] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            (_, end) <- AsyncList.spanAsync(headBatch, cond)
            res <- {
              if (end.isEmpty) {
                tailBuilder(ctx).map(_.dropWhileAsync(cond)).flatMap(_.computation[T](ctx))
              } else {
                val nextHead = Future.successful(end)
                Future.successful((nextHead, tailBuilder))
              }
            }
          } yield res
      }
    }

    new AsyncListImpl[T](newComputation)
  }


  override def serially: AsyncList[T] = {

    def recursiveBuilder(ctx: ExecutionContext,
                         currentStream: Stream[T],
                         tailBuilder: TailBuilder[T]): Future[AsyncList[T]] = {
      currentStream match {
        case Stream.Empty => tailBuilder(ctx).map(_.serially)(ctx)
        case head #:: tail =>
          val nextCol = Future.successful(head :: Nil)

          def newTailBuilder(c: ExecutionContext): Future[AsyncList[T]] =
            recursiveBuilder(c, tail: Stream[T], tailBuilder)

          def nextComputation(c: ExecutionContext): ComputationResult[T] = {
            Future.successful((nextCol, newTailBuilder))
          }

          Future.successful(new AsyncListImpl[T](nextComputation))
      }
    }

    def newComputation[B >: T](ctx: ExecutionContext): ComputationResult[B] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            resultList <- recursiveBuilder(ctx, headBatch.toStream, tailBuilder)
            computed <- resultList.computation(ctx)
          } yield computed
      }
    }

    new AsyncListImpl[T](newComputation)
  }


  override def batched(batchSize: Int): AsyncList[T] = {

    def newComputation[B >: T](ctx: ExecutionContext): ComputationResult[B] = {
      implicit val context = ctx
      computation(ctx).flatMap {
        case (headBatchFuture, tailBuilder) =>
          for {
            headBatch <- headBatchFuture
            alist <- AsyncList.batchedImpl(batchSize, headBatch, tailBuilder, new ArrayBuffer[T](batchSize), ctx)
            computed <- alist.computation(ctx)
          } yield computed
      }
    }

    if (batchSize <= 0)
      throw new RuntimeException(s"Unsupported batch size $batchSize")

    if (batchSize == 1) serially
    else new AsyncListImpl[T](newComputation)
  }

  override def batchMap[B](f: (Seq[T]) => Seq[B]): AsyncList[B] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[B] = {
      implicit val context = ctx
      computation(ctx).map {
        case (headBatch, tailBuilder) =>
          def newTailBuilder(ctx: ExecutionContext): Future[AsyncList[B]] = {
            tailBuilder(ctx).map { alist => alist.batchMap[B](f) }
          }

          val newHead = headBatch.map(f)
          (newHead, newTailBuilder)
      }
    }

    new AsyncListImpl[B](newComputation)
  }

  override def batchMapAsync[B](f: (Seq[T], ExecutionContext) => Future[Seq[B]]): AsyncListImpl[B] = {
    def newComputation(ctx: ExecutionContext): ComputationResult[B] = {
      implicit val context = ctx
      computation(ctx).map {
        case (headBatch, tailBuilder) =>
          def newTailBuilder(ctx: ExecutionContext): Future[AsyncList[B]] = {
            tailBuilder(ctx).map { alist => alist.batchMapAsync[B](f) }
          }

          val newHead = headBatch.flatMap(f(_, ctx))
          (newHead, newTailBuilder)
      }
    }

    new AsyncListImpl[B](newComputation)
  }

  protected[concurrent] override def computation[B >: T](context: ExecutionContext): ComputationResult[B] = {
    _computation(context)
  }

  protected[concurrent] override def foldLeftImpl[B](init: B)
                                                    (calc: (B, Seq[T]) => (B, Boolean)): ResultHandler[B] = {

    FunctionResultHandler[B] { implicit context =>
      for {
        (batchFuture, tailBuilder) <- computation(context)
        currentBatch <- batchFuture
        (foldRes, continue) = calc(init, currentBatch)
        result <- {
          if (!continue) Future.successful(foldRes)
          else {
            for {
              tail <- tailBuilder(context)
              reduced <- tail.foldLeftImpl(foldRes)(calc).run(context)
            } yield reduced
          }
        }
      } yield result
    }
  }


  protected[concurrent] override def foldLeftAsyncImpl[B](init: B)
                                                         (calc: (B, Seq[T], ExecutionContext) => Future[(B, Boolean)]): ResultHandler[B] = {
    FunctionResultHandler[B] { implicit context =>
      for {
        (batchFuture, tailBuilder) <- computation(context)
        currentBatch <- batchFuture
        (foldRes, continue) <- calc(init, currentBatch, context)
        result <- {
          if (!continue) Future.successful(foldRes)
          else {
            for {
              tail <- tailBuilder(context)
              reduced <- tail.foldLeftAsyncImpl(foldRes)(calc(_, _, _)).run
            } yield reduced
          }
        }
      } yield result
    }
  }

}

object ANil extends AsyncList[Nothing] {

  override def takeWhile(cond: Nothing => Boolean): AsyncList[Nothing] = ANil

  override def dropWhile(cond: Nothing => Boolean): AsyncList[Nothing] = ANil

  override def takeWhileAsync(cond: Nothing => Future[Boolean]): AsyncList[Nothing] = ANil

  override def dropWhileAsync(cond: Nothing => Future[Boolean]): AsyncList[Nothing] = ANil

  override def take(count: Int): AsyncList[Nothing] = ANil

  override def drop(count: Int): AsyncList[Nothing] = ANil

  override def sequence: AsyncList[Nothing] = ANil

  override def serially: AsyncList[Nothing] = ANil

  override def batched(batchSize: Int): AsyncList[Nothing] = ANil

  override def batchMap[B](f: (Seq[Nothing]) => Seq[B]): AsyncList[B] = ANil

  override def batchMapAsync[B](f: (Seq[Nothing], ExecutionContext) => Future[Seq[B]]): AsyncList[B] = ANil

  protected[concurrent] override def foldLeftImpl[B](init: B)
                                                    (calc: (B, Seq[Nothing]) => (B, Boolean)): ResultHandler[B] = {
    new FunctionResultHandler[B](_ => Future.successful(init))
  }

  protected[concurrent] override def foldLeftAsyncImpl[B](init: B)
                                                         (calc: (B, Seq[Nothing], ExecutionContext) => Future[(B, Boolean)]): ResultHandler[B] = {
    new FunctionResultHandler[B](_ => Future.successful(init))
  }

  protected[concurrent] override def computation[B >: Nothing](context: ExecutionContext): ComputationResult[B] = {
    Future.successful((Future.successful(Nil), { _: ExecutionContext => Future.successful(ANil) }))
  }
}

trait ResultHandler[+T] {
  def run(implicit context: ExecutionContext): Future[T]
}

case class FunctionResultHandler[+T](private val _run: ExecutionContext => Future[T]) extends ResultHandler[T] {
  def run(implicit context: ExecutionContext): Future[T] = _run(context)
}
