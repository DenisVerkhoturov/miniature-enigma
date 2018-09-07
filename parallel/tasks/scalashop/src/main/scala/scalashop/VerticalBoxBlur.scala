package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

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
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      column <- from until end
      row <- 0 until src.height
    } dst(column, row) = boxBlurKernel(src, column, row, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val starts = 0 to src.width by src.width / numTasks
    (starts zip starts.tail) map { case (start, end) => task { blur(src, dst, start, end, radius) } } foreach (_.join)
  }
}
/*
measurements: 8138.242951, 8505.975301, 7971.478096, 9385.532419, 10069.039161, 9011.006344, 8320.056393, 7968.662156, 8031.812436, 8350.011105
sequential blur time: 8575.181636199999 ms

measurements: 3881.177546, 3907.455404, 3954.313141, 4260.401507, 3889.919726, 3784.871521, 3776.708212, 3756.357417, 3848.461907, 4221.810946
fork/join blur time: 3928.1477327000002 ms

speedup: 2.1830089446014482
 */
