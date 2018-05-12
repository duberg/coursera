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

object HorizontalBoxBlur {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      j <- clamp(from, 0, src.height - 1) to clamp(end, 0, src.height - 1)
      i <- 0 until src.width
    } dst(i,j) = boxBlurKernel(src, i, j, radius)
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val h = src.height / numTasks
    val start = 0 until src.height by h
    val end = start.toList map {
      case elem if elem == start.end => elem + src.height - 1 - h * (start.length - 1)
      case elem => elem + h - 1
    }
    (for (i <- start zip end) yield task(blur(src, dst, i._1, i._2, radius))).foreach(_.join())
  }
}
