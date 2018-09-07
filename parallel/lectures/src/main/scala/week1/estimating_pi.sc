import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

import scala.concurrent.ExecutionContext.Implicits.global

def parallel[T, U](left: => T, right: => U): (T, U) = Await.result(
  for (left <- Future(left); right <- Future(right)) yield (left, right),
  Duration.Inf
)

def monteCarloCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble
    val y = randomX.nextDouble
    if (x * x + y * y < 1) hits += 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * monteCarloCount(iter) / iter

def monteCarloPiPar(iter: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(monteCarloCount(iter / 4), monteCarloCount(iter / 4)),
    parallel(monteCarloCount(iter / 4), monteCarloCount(iter - 3 * (iter / 4)))
  )
  4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}

monteCarloPiPar(100)
