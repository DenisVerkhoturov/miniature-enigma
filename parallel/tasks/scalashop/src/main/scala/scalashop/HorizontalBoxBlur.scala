package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      column <- 0 until src.width
      row <- from until end
    } dst(column, row) = boxBlurKernel(src, column, row, radius)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val starts = 0 to src.height by src.height / numTasks
    (starts zip starts.tail) map { case (start, end) => task { blur(src, dst, start, end, radius) } } foreach (_.join)
  }
}

/*
measurements: 8281.634977, 8223.806613, 8496.316781, 8467.044397, 8727.439745, 8271.363922, 8480.493493, 8329.335978, 8369.223866, 8413.726298
sequential blur time: 8406.038607 ms

measurements: 4627.021685, 4917.797483, 5362.763094, 4389.726564, 4340.670349, 4110.366836, 4333.724311, 4172.652094, 6084.692482, 5882.645251
fork/join blur time: 4822.2060149 ms

speedup: 1.743193588375614
 */