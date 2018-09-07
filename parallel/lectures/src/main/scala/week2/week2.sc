import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

val numbers = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
val sum = numbers reduce (_ + _)
val length = numbers map (_ => 1) reduce (_ + _)
val result = numbers map (x => (x, 1)) reduce ((left, right) => (left._1 + right._1, left._2 + right._2 ))

def parallel[T](left: => T, right: => T): (T, T) = {
  val task = Future { right }
  val result = left
  (left, Await.result(task, Duration.Inf))
}

object sequential {
  def scanLeft[T](array: Array[T], initial: T, function: (T, T) => T, result: Array[T]): Unit = {
    result(0) = initial
    for {
      (element, index) <- array.zipWithIndex
    } result(index + 1) = function(result(index), element)
  }
}

val source = Array(1, 2, 3)
val destination = Array.ofDim[Int](source.length + 1)
sequential.scanLeft[Int](source, 100, _ + _, destination)
destination

object parallel {
  var threshold = 3

  def reduce[T](array: Array[T], left: Int, right: Int, initial: T, function: (T, T) => T): T = ???

  def map[T, R](array: Array[T], left: Int, right: Int, function: (Int, T) => R, result: Array[R]): Unit = ???

  def scanLeft[T](array: Array[T], initial: T, function: (T, T) => T, result: Array[T]): Unit = {
    val reducer: (Int, T) => T = (index, element) => reduce(array, 0, index, initial, function)
    map(array, 0, array.length, reducer, result)
    val last = array.length - 1
    result(last + 1) = function(result(last), array(last))
  }
}

sealed trait Tree[T]
case class Leaf[T](data: T) extends Tree[T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

sealed trait TreeRes[T] { val result: T }
case class LeafRes[T](override val result: T) extends TreeRes[T]
case class NodeRes[T](override val result: T, left: TreeRes[T], right: TreeRes[T]) extends TreeRes[T]

def reduceRes[A](tree: Tree[A], func: (A,A) => A): TreeRes[A] = tree match {
  case Leaf(data) => LeafRes(data)
  case Node(left, right) =>
    val (resultLeft, resultRight) = (reduceRes(left, func), reduceRes(right, func))
    NodeRes(func(resultLeft, resultRight), resultLeft, resultRight)
}

def upsweep[T](tree: Tree[T], func: (T, T) => T): TreeRes[T] = tree match {
  case Leaf(result) => LeafRes(result)
  case Node(left, right) =>
    val (tl, tr) = parallel(upsweep(left, func), upsweep(right, func))
    NodeRes(func(tl.result, tr.result), tl, tr)
}

def downsweep[T](tree: TreeRes[T], initial: T, func: (T, T) => T): Tree[T] = tree match {
  case LeafRes(result) => Leaf(func(initial, result))
  case NodeRes(_, left, right) =>
    val (tl, tr) = parallel(downsweep(left, initial, func), downsweep(right, func(initial, left.result), func))
    Node(tl, tr)
}

def prepend[T](element: T, tree: Tree[T]): Tree[T] = tree match {
  case Leaf(data) => Node(Leaf(element), Leaf(data))
  case Node(left, right) => Node(prepend(element, left), right)
}

def scanLeft[T](tree: Tree[T], initial: T, func: (T, T) => T): Tree[T] = {
  val result = upsweep(tree, func)
  val scan = downsweep(result, initial, func)
  prepend(initial, scan)
}
