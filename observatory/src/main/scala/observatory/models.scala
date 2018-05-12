package observatory

import java.sql.Date

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class StationId(stn: Option[Int], wban: Option[Int]) {
  assert(stn.nonEmpty || wban.nonEmpty, "Invalid StationId.")
}

case class Station(id: StationId,  location: Option[Location])

case class TemperatureByStation(stationId: StationId, date: Date, temperature: Double)

case class TemperatureByLocation(location: Location, temperature: Double)