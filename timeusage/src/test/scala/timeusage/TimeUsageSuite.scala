package timeusage

import org.apache.spark.sql.DataFrame
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest._
import Matchers._
import TimeUsage._

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
  val (columns, initDf) = read("/timeusage/atussum.csv")
  val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
  val summaryDf: DataFrame = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
  val finalDf: DataFrame = timeUsageGrouped(summaryDf)
  val finalDfSql: DataFrame = timeUsageGroupedSql(summaryDf)

  test("dataframes should have same number of rows") {
    val rowsNumber = finalDfSql.intersect(finalDf).count()
    finalDf.count() shouldBe rowsNumber
    finalDfSql.count() shouldBe rowsNumber
  }

  test("dataset should have same number of rows") {
    val typed = timeUsageSummaryTyped(finalDf)
    finalDf.count() shouldBe typed.count()
  }

  test("timeUsageSummary") {
    summaryDf.show()
  }

  /**
    * [Test Description] timeUsageSummary
[Observed Error] Unexpected column value in the resulting dataset: 12.5 did not equal 5.0 +- 0.001
[Lost Points] 10

[Test Description] timeUsageGrouped && timeUsageGroupedSql && timeUsageGroupedTyped
[Observed Error] timeUsageGrouped returned an incorrect dataset. scala.this.Predef.refArrayOps[org.apache.spark.sql.Row](collectedResult).sameElements[org.apache.spark.sql.Row](scala.this.Predef.wrapRefArray[org.apache.spark.sql.Row](collectedExpect)) was false
[Lost Points] 10

======== TESTING ENVIRONMENT ========
Limits: memory: 1540m,  total time: 900s,  per test case time: 240s

======== DEBUG OUTPUT OF TESTING TOOL ========
Using Spark's default log4j profile: org/apache/spark/log4j-defaults.properties
17/04/13 13:28:41 WARN NativeCodeLoader: Unable to load native-hadoop library for your platform... using builtin-java classes where applicable

[Stage 2:=====================>                                  (78 + 1) / 200]
[Stage 2:===============================>                       (116 + 1) / 200]
[Stage 2:============================================>          (161 + 1) / 200]


[Stage 4:======================>                                 (82 + 1) / 200]
[Stage 4:====================================>                  (132 + 1) / 200]
[Stage 4:=========================================>             (152 + 2) / 200]
[Stage 4:===============================================>       (174 + 1) / 200]
    */
}
