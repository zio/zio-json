package testzio.json.data.geojson

import ai.x.play.json.Encoders.encoder
import ai.x.play.json.{ Jsonx => Playx }
import io.circe
import play.api.libs.{ json => Play }
import zio.json._
import zio.json.ast._

object playtuples extends Play.GeneratedReads with Play.GeneratedWrites
import playtuples._

package generated {

  import com.github.ghik.silencer.silent

  @jsonDiscriminator("type")
  sealed abstract class Geometry
  final case class Point(coordinates: (Double, Double))                          extends Geometry
  final case class MultiPoint(coordinates: List[(Double, Double)])               extends Geometry
  final case class LineString(coordinates: List[(Double, Double)])               extends Geometry
  final case class MultiLineString(coordinates: List[List[(Double, Double)]])    extends Geometry
  final case class Polygon(coordinates: List[List[(Double, Double)]])            extends Geometry
  final case class MultiPolygon(coordinates: List[List[List[(Double, Double)]]]) extends Geometry
  final case class GeometryCollection(
    geometries: List[Geometry] // NOTE: recursive
  ) extends Geometry

  @jsonDiscriminator("type")
  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry) extends GeoJSON
  final case class FeatureCollection(
    features: List[GeoJSON] // NOTE: recursive
  ) extends GeoJSON

  @silent("Block result was adapted via implicit conversion")
  object Geometry {
    implicit lazy val zioJsonJsonDecoder: JsonDecoder[Geometry] =
      DeriveJsonDecoder.gen[Geometry]
    implicit lazy val zioJsonEncoder: JsonEncoder[Geometry] =
      DeriveJsonEncoder.gen[Geometry]

    implicit val customConfig: circe.generic.extras.Configuration =
      circe.generic.extras.Configuration.default
        .copy(discriminator = Some("type"))
    implicit lazy val circeJsonDecoder: circe.Decoder[Geometry] =
      circe.generic.extras.semiauto.deriveConfiguredDecoder[Geometry]
    implicit lazy val circeEncoder: circe.Encoder[Geometry] =
      circe.generic.extras.semiauto.deriveConfiguredEncoder[Geometry]

    // it's not clear why this needs the extras package...
    implicit val playPoint: Play.Format[Point]                                = Playx.formatCaseClass[Point]
    implicit val playMultiPoint: Play.Format[MultiPoint]                      = Play.Json.format[MultiPoint]
    implicit val playLineString: Play.Format[LineString]                      = Play.Json.format[LineString]
    implicit val playMultiLineString: Play.Format[MultiLineString]            = Play.Json.format[MultiLineString]
    implicit val playPolygon: Play.Format[Polygon]                            = Play.Json.format[Polygon]
    implicit val playMultiPolygon: Play.Format[MultiPolygon]                  = Play.Json.format[MultiPolygon]
    implicit lazy val playGeometryCollection: Play.Format[GeometryCollection] = Play.Json.format[GeometryCollection]
    implicit val playFormatter: Play.Format[Geometry]                         = Playx.formatSealed[Geometry]

  }
  @silent("Block result was adapted via implicit conversion")
  object GeoJSON {
    implicit lazy val zioJsonJsonDecoder: JsonDecoder[GeoJSON] =
      DeriveJsonDecoder.gen[GeoJSON]
    implicit lazy val zioJsonEncoder: JsonEncoder[GeoJSON] =
      DeriveJsonEncoder.gen[GeoJSON]

    implicit val customConfig: circe.generic.extras.Configuration =
      circe.generic.extras.Configuration.default
        .copy(discriminator = Some("type"))
    implicit lazy val circeJsonDecoder: circe.Decoder[GeoJSON] =
      circe.generic.extras.semiauto.deriveConfiguredDecoder[GeoJSON]
    implicit lazy val circeEncoder: circe.Encoder[GeoJSON] =
      circe.generic.extras.semiauto.deriveConfiguredEncoder[GeoJSON]

    implicit val playFeature: Play.Format[Feature]                          = Play.Json.format[Feature]
    implicit lazy val playFeatureCollection: Play.Format[FeatureCollection] = Play.Json.format[FeatureCollection]
    implicit val playFormatter: Play.Format[GeoJSON]                        = Playx.formatSealed[GeoJSON]

  }
}

package handrolled {

  import com.github.ghik.silencer.silent

  sealed abstract class Geometry
  final case class Point(coordinates: (Double, Double))                          extends Geometry
  final case class MultiPoint(coordinates: List[(Double, Double)])               extends Geometry
  final case class LineString(coordinates: List[(Double, Double)])               extends Geometry
  final case class MultiLineString(coordinates: List[List[(Double, Double)]])    extends Geometry
  final case class Polygon(coordinates: List[List[(Double, Double)]])            extends Geometry
  final case class MultiPolygon(coordinates: List[List[List[(Double, Double)]]]) extends Geometry
  final case class GeometryCollection(
    geometries: List[Geometry] // NOTE: recursive
  ) extends Geometry

  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry) extends GeoJSON
  final case class FeatureCollection(
    features: List[GeoJSON] // NOTE: recursive
  ) extends GeoJSON

  @silent("Block result was adapted via implicit conversion")
  object Geometry {
    // this is an example of a handrolled decoder that avoids using the
    // backtracking algorithm that is normally used for sealed traits with a
    // discriminator. If we see a "properties" field, we count the number of
    // brackets to decide what needs to be decoded.
    //
    // This should be considered an extremely advanced example of how to write
    // custom decoders and is not a requirement to use the JsonDecoder[GeoJSON]
    // custom decoder (below) which is necessary to avert a DOS attack.
    implicit lazy val zioJsonJsonDecoder: JsonDecoder[Geometry] =
      new JsonDecoder[Geometry] {
        import zio.json._, internal._, JsonDecoder.{ JsonError, UnsafeJson }
        import scala.annotation._

        val names: Array[String]    = Array("type", "coordinates", "geometries")
        val matrix: StringMatrix    = new StringMatrix(names)
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        val subtypes: StringMatrix = new StringMatrix(
          Array(
            "Point",
            "MultiPoint",
            "LineString",
            "MultiLineString",
            "Polygon",
            "MultiPolygon",
            "GeometryCollection"
          )
        )
        val coordinatesD: JsonDecoder[Json.Arr]           = JsonDecoder[Json.Arr]
        lazy val geometriesD: JsonDecoder[List[Geometry]] = JsonDecoder[List[Geometry]]

        def coordinates0(
          trace: List[JsonError],
          js: Json.Arr
        ): (Double, Double) =
          js match {
            case Json.Arr(chunk)
                if chunk.length == 2 && chunk(0).isInstanceOf[Json.Num] && chunk(1).isInstanceOf[Json.Num] =>
              (chunk(0).asInstanceOf[Json.Num].value.doubleValue(), chunk(1).asInstanceOf[Json.Num].value.doubleValue())
            case _ =>
              throw UnsafeJson(
                JsonError.Message("expected coordinates") :: trace
              )
          }
        def coordinates1(
          trace: List[JsonError],
          js: Json.Arr
        ): List[(Double, Double)] =
          js.elements.map {
            case js1: Json.Arr => coordinates0(trace, js1)
            case _ =>
              throw UnsafeJson(JsonError.Message("expected list") :: trace)
          }.toList
        def coordinates2(
          trace: List[JsonError],
          js: Json.Arr
        ): List[List[(Double, Double)]] =
          js.elements.map {
            case js1: Json.Arr => coordinates1(trace, js1)
            case _ =>
              throw UnsafeJson(JsonError.Message("expected list") :: trace)
          }.toList
        def coordinates3(
          trace: List[JsonError],
          js: Json.Arr
        ): List[List[List[(Double, Double)]]] =
          js.elements.map {
            case js1: Json.Arr => coordinates2(trace, js1)
            case _ =>
              throw UnsafeJson(JsonError.Message("expected list") :: trace)
          }.toList

        def unsafeDecode(
          trace: List[JsonError],
          in: RetractReader
        ): Geometry = {
          Lexer.char(trace, in, '{')

          var coordinates: Json.Arr      = null
          var geometries: List[Geometry] = null
          var subtype: Int               = -1

          if (Lexer.firstField(trace, in))
            do {
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                (field: @switch) match {
                  case 0 =>
                    if (subtype != -1)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                    subtype = Lexer.enumeration(trace_, in, subtypes)
                  case 1 =>
                    if (coordinates != null)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                    coordinates = coordinatesD.unsafeDecode(trace_, in)
                  case 2 =>
                    if (geometries != null)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)

                    geometries = geometriesD.unsafeDecode(trace_, in)
                }
              }
            } while (Lexer.nextField(trace, in))

          if (subtype == -1)
            throw UnsafeJson(
              JsonError.Message("missing discriminator") :: trace
            )

          if (subtype == 6) {
            if (geometries == null)
              throw UnsafeJson(
                JsonError.Message("missing 'geometries' field") :: trace
              )
            else GeometryCollection(geometries)
          }

          if (coordinates == null)
            throw UnsafeJson(
              JsonError.Message("missing 'coordinates' field") :: trace
            )
          val trace_ = spans(1) :: trace
          (subtype: @switch) match {
            case 0 => Point(coordinates0(trace_, coordinates))
            case 1 => MultiPoint(coordinates1(trace_, coordinates))
            case 2 => LineString(coordinates1(trace_, coordinates))
            case 3 => MultiLineString(coordinates2(trace_, coordinates))
            case 4 => Polygon(coordinates2(trace_, coordinates))
            case 5 => MultiPolygon(coordinates3(trace_, coordinates))
          }
        }

      }
    implicit lazy val zioJsonEncoder: JsonEncoder[Geometry] =
      DeriveJsonEncoder.gen[Geometry]

    implicit val customConfig: circe.generic.extras.Configuration =
      circe.generic.extras.Configuration.default
        .copy(discriminator = Some("type"))
    implicit lazy val circeJsonDecoder: circe.Decoder[Geometry] =
      circe.generic.extras.semiauto.deriveConfiguredDecoder[Geometry]
    implicit lazy val circeEncoder: circe.Encoder[Geometry] =
      circe.generic.extras.semiauto.deriveConfiguredEncoder[Geometry]
    implicit val playPoint: Play.Format[Point]                                = Playx.formatCaseClass[Point]
    implicit val playMultiPoint: Play.Format[MultiPoint]                      = Play.Json.format[MultiPoint]
    implicit val playLineString: Play.Format[LineString]                      = Play.Json.format[LineString]
    implicit val playMultiLineString: Play.Format[MultiLineString]            = Play.Json.format[MultiLineString]
    implicit val playPolygon: Play.Format[Polygon]                            = Play.Json.format[Polygon]
    implicit val playMultiPolygon: Play.Format[MultiPolygon]                  = Play.Json.format[MultiPolygon]
    implicit lazy val playGeometryCollection: Play.Format[GeometryCollection] = Play.Json.format[GeometryCollection]
    implicit val playFormatter: Play.Format[Geometry]                         = Playx.formatSealed[Geometry]

  }
  @silent("Block result was adapted via implicit conversion")
  object GeoJSON {
    // This uses a hand rolled decoder that guesses the type based on the field
    // names to protect against attack vectors that put the hint at the end of
    // the object. This is only needed because the contents of the GeoJSON is
    // potentially complex and even skipping over it is expensive... it's a bit
    // of a corner case.
    implicit lazy val zioJsonJsonDecoder: JsonDecoder[GeoJSON] =
      new JsonDecoder[GeoJSON] {
        import zio.json._, internal._, JsonDecoder.{ JsonError, UnsafeJson }
        import scala.annotation._

        val names: Array[String] =
          Array("type", "properties", "geometry", "features")
        val matrix: StringMatrix    = new StringMatrix(names)
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        val subtypes: StringMatrix = new StringMatrix(
          Array("Feature", "FeatureCollection")
        )
        val propertyD: JsonDecoder[Map[String, String]] =
          JsonDecoder[Map[String, String]]
        val geometryD: JsonDecoder[Geometry] = JsonDecoder[Geometry]
        lazy val featuresD: JsonDecoder[List[GeoJSON]] =
          JsonDecoder[List[GeoJSON]] // recursive

        def unsafeDecode(trace: List[JsonError], in: RetractReader): GeoJSON = {
          Lexer.char(trace, in, '{')

          var properties: Map[String, String] = null
          var geometry: Geometry              = null
          var features: List[GeoJSON]         = null
          var subtype: Int                    = -1

          if (Lexer.firstField(trace, in))
            do {
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                (field: @switch) match {
                  case 0 =>
                    if (subtype != -1)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)

                    subtype = Lexer.enumeration(trace_, in, subtypes)
                  case 1 =>
                    if (properties != null)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)

                    properties = propertyD.unsafeDecode(trace_, in)
                  case 2 =>
                    if (geometry != null)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)

                    geometry = geometryD.unsafeDecode(trace_, in)
                  case 3 =>
                    if (features != null)
                      throw UnsafeJson(JsonError.Message("duplicate") :: trace_)

                    features = featuresD.unsafeDecode(trace_, in)
                }
              }
            } while (Lexer.nextField(trace, in))

          if (subtype == -1)
            // we could infer the type but that would mean accepting invalid data
            throw UnsafeJson(
              JsonError.Message("missing required fields") :: trace
            )

          if (subtype == 0) {
            if (properties == null)
              throw UnsafeJson(
                JsonError.Message("missing 'properties' field") :: trace
              )
            if (geometry == null)
              throw UnsafeJson(
                JsonError.Message("missing 'geometry' field") :: trace
              )
            Feature(properties, geometry)
          } else {

            if (features == null)
              throw UnsafeJson(
                JsonError.Message("missing 'features' field") :: trace
              )
            FeatureCollection(features)
          }
        }

      }
    implicit lazy val zioJsonEncoder: JsonEncoder[GeoJSON] =
      DeriveJsonEncoder.gen[GeoJSON]

    implicit val customConfig: circe.generic.extras.Configuration =
      circe.generic.extras.Configuration.default
        .copy(discriminator = Some("type"))
    implicit lazy val circeJsonDecoder: circe.Decoder[GeoJSON] =
      circe.generic.extras.semiauto.deriveConfiguredDecoder[GeoJSON]
    implicit lazy val circeEncoder: circe.Encoder[GeoJSON] =
      circe.generic.extras.semiauto.deriveConfiguredEncoder[GeoJSON]

    implicit val playFeature: Play.Format[Feature]                          = Play.Json.format[Feature]
    implicit lazy val playFeatureCollection: Play.Format[FeatureCollection] = Play.Json.format[FeatureCollection]

    implicit val playFormatter: Play.Format[GeoJSON] = Playx.formatSealed[GeoJSON]

  }
}
