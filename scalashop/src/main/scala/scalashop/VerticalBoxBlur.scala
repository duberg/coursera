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
     i <- clamp(from, 0, src.width - 1) to clamp(end, 0, src.width - 1)
     j <- 0 until src.height
   } dst(i,j) = boxBlurKernel(src, i, j, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
    *
    *  Use Scala ranges to create a list of splitting points
    *  (hint: use the by method on ranges).
    *  Then use collection combinators on the list of splitting
    *  points to create a list of start and end tuples, one for
    *  each strip (hint: use the zip and tail methods). Finally,
    *  use the task construct to start a parallel task for each strip,
    *  and then call join on each task to wait for its completion.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val w = src.width / numTasks
    val start = 0 until src.width by w
    val end = start.toList map {
      case elem if elem == start.end => elem + src.width - 1 - w * (start.length - 1)
      case elem => elem + w - 1
    }
    (for (i <- start zip end) yield task(blur(src, dst, i._1, i._2, radius))).foreach(_.join())
  }
}
