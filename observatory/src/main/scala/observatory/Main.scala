package observatory

import org.apache.spark.sql.functions.col

object Main extends App {
  import Extraction.spark.implicits._

  //Extraction.read("/stations.csv")
  //Extraction.readStations("/2015.csv", 2015).show()

  val stationsFile = "/stations.csv"
  val temperaturesFile = "/2015.csv"
  val year = 2015

  Extraction.locationYearlyAverageRecords(
    Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
  )

//  Extraction.readStations(stationsFile)
//    .join(Extraction.readTemperatureRecords(temperaturesFile, year), col("id") === col("stationId"))
//    .select(col("location").as[Location],col("date").as[RecordDate], col("temperature").as[Double])
//      .show()

}
