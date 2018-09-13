package observatory

import java.time.LocalDate

import observatory.Extraction.{extractStations, extractTemperatures, locateTemperatures, locationYearlyAverageRecords}
import org.scalatest.{Matchers, WordSpec}

trait ExtractionTest extends WordSpec with Matchers {
  "extract stations" should {
    "identify stations by the composite (STN, WBAN)" in {
      val stations = extractStations("/stations.csv").collect
      val expected = Array(
        StationRecord("724017:03707", +37.358, -78.438),
        StationRecord("724017:", +37.350, -78.433)
      )
      stations should contain theSameElementsAs expected
    }
  }
  "extract temperatures" should {
    "identify temperature by the composite (STN, WBAN)" in {
      val year = 2015
      val temperatures = extractTemperatures(year, s"/temperatures/$year.csv").collect
      val expected = Array(
        TemperatureRecord("010013:", year, 11, 25, 39.2),
        TemperatureRecord("724017:", year, 8, 11, 81.14),
        TemperatureRecord("724017:03707", year, 12, 6, 32.0),
        TemperatureRecord("724017:03707", year, 1, 29, 35.6)
      )
      temperatures should contain theSameElementsAs expected
    }
  }
  "locate temperatures" should {
    "join data by the composite (STN, WBAN)" in {
      val year = 2015
      val stationTemperatures = locateTemperatures(year, "/stations.csv" ,s"/temperatures/$year.csv")
      val expected = Iterable(
        (LocalDate.of(year, 8, 11), Location(37.35, -78.433), 27.3),
        (LocalDate.of(year, 12, 6), Location(37.358, -78.438), 0.0),
        (LocalDate.of(year, 1, 29), Location(37.358, -78.438), 2.000000000000001)
      )
      stationTemperatures should contain theSameElementsAs expected
    }
  }
  "location yearly average records" should {
    "average temperature over the year" in {
      val year = 2015
      val yearlyAverage = locationYearlyAverageRecords(Iterable(
        (LocalDate.of(year, 8, 11), Location(37.35, -78.433), 27.3),
        (LocalDate.of(year, 12, 6), Location(37.358, -78.438), 0.0),
        (LocalDate.of(year, 1, 29), Location(37.358, -78.438), 2.0)
      ))
      val expected = Iterable(
        Location(37.35, -78.433) -> 27.3,
        Location(37.358, -78.438) -> 1.0
      )
      yearlyAverage should contain theSameElementsAs expected
    }
  }
}
