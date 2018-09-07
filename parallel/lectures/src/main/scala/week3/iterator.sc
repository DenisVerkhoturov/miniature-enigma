import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable

object common {

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}

trait Iterator[T] {
  def next: T
  def hasNext: Boolean
  def foldLeft[R](initial: R)(function: (R, T) => R): R = {
    def iterate(accumulator: R): R = if (hasNext) iterate(function(accumulator, next)) else accumulator

    if (hasNext) iterate(initial) else initial
  }
}
import common._
val threshold = 1

trait Splitter[T] extends Iterator[T] {
  def split: Seq[Splitter[T]]
  def remaining: Int
  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[ForkJoinTask[T]] = split.map(splitter => task { splitter.fold(z)(f) })
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

trait Builder[T, Repr] {
  def +=(element: T): Builder[T, Repr]
  def result: Repr
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach(element => if (p(element)) b += element)
    b.result
  }
}

trait Combiner[T, Repr] extends Builder[T, Repr] {
  def combine(that: Combiner[T, Repr]): Combiner[T, Repr]
}
