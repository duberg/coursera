package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  val temperaturesFile = "/test_2015.csv"
  val stationsFile = "/test_stations.csv"

  test("locateTemperatures should produce allOf") {
    val data = Extraction.locateTemperatures(2015, stationsFile, temperaturesFile)
    data should contain allOf(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
  }

  test("locationYearlyAverageRecords should produce allOf") {
    val data = Extraction.locationYearlyAverageRecords(
      Extraction.locateTemperatures(2015, stationsFile, temperaturesFile)
    )
    data should contain allOf(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
  }
  
}