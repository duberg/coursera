package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark._
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Column, DataFrame, Dataset, Row}
import org.apache.spark.sql.types._
import java.sql.Date

/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local[6]")
      .getOrCreate()

  import spark.implicits._

  def parseOptInt(v: String): Option[Int] =
    if (v.isEmpty) None else Some(v.toInt)
  def parseOptDouble(v: String): Option[Double] =
    if (v.isEmpty) None else Some(v.toDouble)
  def parseOptLocation(v1: String, v2: String): Option[Location] = {
    val lat: Option[Double] = parseOptDouble(v1)
    val lon: Option[Double] = parseOptDouble(v2)
    if (lat.isEmpty && lon.isEmpty) None else Some(Location(lat.get, lon.get))
  }

  val stations: Option[Dataset[Station]] = None

  def readStations(resource: String): Dataset[Station] = {
    def read: Dataset[Station] = {
      val rdd = spark.sparkContext.textFile(fsPath(resource))

      val data =
        rdd
          .map(_.split(","))
          // ignore data coming from stations that have no GPS coordinates
          .collect {
            case Array(stn, wban, lat, lon) =>
              Station(
                StationId(parseOptInt(stn), parseOptInt(wban)),
                parseOptLocation(lat, lon)
              )
          }

      data.toDS().persist()
    }
    stations.getOrElse(read)
  }

  def readTemperatureRecords(resource: String, year: Int): Dataset[TemperatureByStation] = {
    val rdd = spark.sparkContext.textFile(fsPath(resource))

    def toCelsius(t: Double) = Math.round((t - 32) * 5 / 9 * 100.0) / 100.0

    val data =
      rdd
        .map(_.split(","))
        .collect {
          case Array(stn, wban, month, day,	temperature) if temperature != "9999.9" =>
            TemperatureByStation(
              StationId(parseOptInt(stn), parseOptInt(wban)),
              LocalDate.of(year, month.toInt, day.toInt),
              toCelsius(temperature.toDouble)
            )
        }

    data.toDS()
  }

  implicit def toSqlDate(localDate: LocalDate): Date = Date.valueOf(localDate)
  implicit def toLocalDateDate(date: Date): Date = date.toLocalDate

  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  implicit val dateEncoder = org.apache.spark.sql.Encoders.DATE

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val data = readStations(stationsFile)
      .join(readTemperatureRecords(temperaturesFile, year), 'id === 'stationId)
      .select('location.as[Location], 'date.as[java.sql.Date], 'temperature.as[Double])

    data
      .collect()
      .map({case (location, date, temperature) => (date.toLocalDate, location, temperature)})
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
   // import org.apache.spark.sql.expressions.scalalang.typed._

    val data = records.toSeq.map(r => (r._2, r._3))//.toDS()

    val rdd = spark.sparkContext.parallelize(data)

    val average = rdd
      .mapValues((1, _))
      .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
      .mapValues(v => v._2 / v._1)

//    val average = data
//      .map(r => TemperatureByLocation(r._1, r._2))
//      .groupByKey(r => r.location)
//      .agg(
//        avg(_.temperature)
//      )

    average.collect()
  }
}
