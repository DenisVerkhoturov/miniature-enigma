package observatory

import java.time.LocalDate

import org.apache.spark.sql.{Dataset, SparkSession}
import org.apache.spark.SparkConf
import org.apache.spark.sql.types.{DoubleType, IntegerType}
import org.apache.spark.sql.functions.{lit, concat_ws, coalesce}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("observatory")
  val spark: SparkSession = SparkSession.builder().config(conf).getOrCreate()
  import spark.implicits._

  def extractStations(path: String): Dataset[StationRecord] = spark.read.csv(resource(path))
    .select(
      concat_ws(":", coalesce('_c0, lit("")), coalesce('_c1, lit(""))).as("id"),
      '_c2.cast(DoubleType).as("lat"),
      '_c3.cast(DoubleType).as("lon")
    )
    .where('lat.isNotNull && 'lon.isNotNull && 'lat =!= 0.0 && 'lon =!= 0.0)
    .as[StationRecord]

  def extractTemperatures(year: Year, path: String): Dataset[TemperatureRecord] = spark.read.csv(resource(path))
    .select(
      concat_ws(":", coalesce('_c0, lit("")), coalesce('_c1, lit(""))).as("id"),
      lit(year).as("year"),
      '_c2.cast(IntegerType).as("month"),
      '_c3.cast(IntegerType).as("day"),
      '_c4.cast(DoubleType).as("temperature")
    )
    .as[TemperatureRecord]

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations: Dataset[StationRecord] = extractStations(stationsFile)
    val temperatures: Dataset[TemperatureRecord] = extractTemperatures(year, temperaturesFile)

    stations
      .join(temperatures, usingColumn = "id")
      .as[StationTemperatures].rdd
      .map(row => (LocalDate.of(row.year, row.month, row.day), Location(row.lat, row.lon), (row.temperature - 32) / 9 * 5))
      .collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    records
      .groupBy(_._2)
      .mapValues(locations => locations.aggregate(0.0)(_ + _._3, _ + _) / locations.size)

  private def resource(path: String): String = getClass.getResource(path).toString
}
