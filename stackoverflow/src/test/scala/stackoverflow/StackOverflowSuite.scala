package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.Matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  lazy val conf: SparkConf = new SparkConf().setMaster("local[6]").setAppName("stackoverflow-test")
  lazy val sc: SparkContext = new SparkContext(conf)

  private def filePath = {
    val resource = this.getClass.getClassLoader.getResource("stackoverflow/stackoverflow.csv")
    if (resource == null) sys.error("Please download the dataset as explained in the assignment instructions")
    new File(resource.toURI).getPath
  }
  lazy val lines: RDD[String] = sc.textFile(filePath)
  lazy val raw: RDD[Posting] = testObject.rawPostings(lines)
  lazy val grouped: RDD[(Int, Iterable[(Posting, Posting)])] = testObject.groupedPostings(raw)
  lazy val scored: RDD[(Posting, Int)] = testObject.scoredPostings(grouped)
  lazy val vectors: RDD[(Int, Int)] = testObject.vectorPostings(scored)
  lazy val vectorsArray: Array[(Int, Int)] = vectors.collect()
  lazy val means: Array[(Int, Int)] = testObject.kmeans(testObject.sampleVectors(vectors), vectors, debug = true)
  lazy val results: Array[(String, Double, Int, Int)] = testObject.clusterResults(means, vectors)

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("scored should contain allOf") {
    scored.collect() should contain allOf (
        (Posting(1,6,None,None,140,Some("CSS")),67),
        (Posting(1,42,None,None,155,Some("PHP")),89),
        (Posting(1,72,None,None,16,Some("Ruby")),3),
        (Posting(1,126,None,None,33,Some("Java")),30),
        (Posting(1,174,None,None,38,Some("C#")),20)
    )
  }

  test("vectors should contain allOf") {
    vectorsArray should contain allOf (
      (350000,67),
      (100000,89),
      (300000,3),
      (50000,30),
      (200000,20)
    )
  }

  test("vectors should have length") {
    vectorsArray should have length 2121822
  }

  test("results"){
    testObject.printResults(results)
    //assert(results(0) ===  ("Java", 100, 1361, 0))
  }
}
