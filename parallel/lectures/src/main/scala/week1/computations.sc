import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

def parallel[T, U](left: => T, right: => U): (T, U) = Await.result(
  for (left <- Future(left); right <- Future(right)) yield (left, right),
  Duration.Inf
)

def power(number: Double, exponent: Double): Double = math.exp(exponent * math.log(math.abs(number)))

def sumSegment(array: Array[Int], exponent: Double, start: Int, end: Int): Double = {
  require(0 <= start && start <= end, "Start of the boundaries required to be in the range from zero up to the end.")
  require(end <= array.length, "End of the boundaries required to be less than or equal to the length of the array.")

  def iterate(accumulator: Double, counter: Int): Double =
    if (counter < end) iterate(accumulator + power(array(counter), exponent), counter + 1) else accumulator

  iterate(0, start)
}

/**
  * Sequential computation
  */
def pNorm(array: Array[Int], exponent: Double): Double = power(sumSegment(array, exponent, 0, array.length), 1 / exponent)

/**
  * Parallel computation using two threads
  */
def pNormTwoPart(array: Array[Int], exponent: Double): Double = {
  val middle = array.length / 2
  val (left, right) = parallel(sumSegment(array, exponent, 0, middle), sumSegment(array, exponent, middle, array.length))
  power(left + right, 1 / exponent)
}

/**
  * Parallel computation using dynamic number of threads depending on size of the problem
  */
def pNormRec(array: Array[Int], exponent: Double): Double = {
  val threshold = 2

  def fork(start: Int, end: Int): Double =
    if (end - start < threshold) sumSegment(array, exponent, start, end)
    else {
      val middle = start + (end - start) / 2
      val (left, right) = parallel(sumSegment(array, exponent, 0, middle), sumSegment(array, exponent, middle, array.length))
      left + right
    }

  power(fork(0, array.length), 1 / exponent)
}

pNormRec(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2)
