import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

trait Task[T] {
  def join: T
}

object Task {
  def apply[T](expression: => T): Task[T] = {
    val future: Future[T] = Future(expression)
    new Task[T] {
      override def join = Await.result(future, Duration.Inf)
    }
  }
}

implicit def getJoin[T](task: Task[T]): T = task.join

def parallel[A,B](left: => A, right: => B): (A, B) = {
  val task = Task { right }
  (left, task.join)
}

def power(number: Double, exponent: Double): Double = math.exp(exponent * math.log(math.abs(number)))

def sumSegment(array: Array[Int], exponent: Double, start: Int, end: Int): Double = {
  require(0 <= start && start <= end, "Start of the boundaries required to be in the range from zero up to the end.")
  require(end <= array.length, "End of the boundaries required to be less than or equal to the length of the array.")

  def iterate(accumulator: Double, counter: Int): Double =
    if (counter < end) iterate(accumulator + power(array(counter), exponent), counter + 1) else accumulator

  iterate(0, start)
}

def pNorm(array: Array[Int], exponent: Double): Double = {
  val t1 = Task { sumSegment(array, exponent, 0, array.length / 4) }
  val t2 = Task { sumSegment(array, exponent, array.length / 4, array.length / 2) }
  val t3 = Task { sumSegment(array, exponent, array.length / 2, array.length / 2 + array.length / 4) }
  val t4 = Task { sumSegment(array, exponent, array.length / 2 + array.length / 4, array.length) }
  power(t1 + t2 + t3 + t4, 1 / exponent)
}

pNorm(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2)


