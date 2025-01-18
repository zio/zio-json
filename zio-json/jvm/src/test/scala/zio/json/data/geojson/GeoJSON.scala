package zio.json.data.geojson

import zio.json._
import zio.json.ast._
import com.github.ghik.silencer.silent
import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps

package generated {

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

    implicit lazy val circeJsonCodec: Codec[Geometry] = {
      implicit val c1: Codec[Point]              = deriveCodec
      implicit val c2: Codec[MultiPoint]         = deriveCodec
      implicit val c3: Codec[LineString]         = deriveCodec
      implicit val c4: Codec[MultiLineString]    = deriveCodec
      implicit val c5: Codec[Polygon]            = deriveCodec
      implicit val c6: Codec[MultiPolygon]       = deriveCodec
      implicit val c8: Codec[GeometryCollection] = deriveCodec
      Codec.from(
        Decoder.instance(c =>
          c.downField("type").as[String].flatMap {
            case "Point"              => c.as[Point]
            case "MultiPoint"         => c.as[MultiPoint]
            case "LineString"         => c.as[LineString]
            case "MultiLineString"    => c.as[MultiLineString]
            case "Polygon"            => c.as[Polygon]
            case "MultiPolygon"       => c.as[MultiPolygon]
            case "GeometryCollection" => c.as[GeometryCollection]
          }
        ),
        Encoder.instance {
          case x: Point              => x.asJson.mapObject(_.+:("type" -> "Point".asJson))
          case x: MultiPoint         => x.asJson.mapObject(_.+:("type" -> "MultiPoint".asJson))
          case x: LineString         => x.asJson.mapObject(_.+:("type" -> "LineString".asJson))
          case x: MultiLineString    => x.asJson.mapObject(_.+:("type" -> "MultiLineString".asJson))
          case x: Polygon            => x.asJson.mapObject(_.+:("type" -> "Polygon".asJson))
          case x: MultiPolygon       => x.asJson.mapObject(_.+:("type" -> "MultiPolygon".asJson))
          case x: GeometryCollection => x.asJson.mapObject(_.+:("type" -> "GeometryCollection".asJson))
        }
      )
    }
  }
  @silent("Block result was adapted via implicit conversion")
  object GeoJSON {
    implicit lazy val zioJsonJsonDecoder: JsonDecoder[GeoJSON] =
      DeriveJsonDecoder.gen[GeoJSON]
    implicit lazy val zioJsonEncoder: JsonEncoder[GeoJSON] =
      DeriveJsonEncoder.gen[GeoJSON]

    implicit lazy val circeCodec: Codec[GeoJSON] = {
      implicit val c1: Codec[Feature]           = deriveCodec
      implicit val c2: Codec[FeatureCollection] = deriveCodec
      Codec.from(
        Decoder.instance(c =>
          c.downField("type").as[String].flatMap {
            case "Feature"           => c.as[Feature]
            case "FeatureCollection" => c.as[FeatureCollection]
          }
        ),
        Encoder.instance {
          case x: Feature           => x.asJson.mapObject(_.+:("type" -> "Feature".asJson))
          case x: FeatureCollection => x.asJson.mapObject(_.+:("type" -> "FeatureCollection".asJson))
        }
      )
    }
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
        import zio.json._
        import JsonDecoder.JsonError
        import internal._

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
            case _ => Lexer.error("expected coordinates", trace)
          }
        def coordinates1(
          trace: List[JsonError],
          js: Json.Arr
        ): List[(Double, Double)] =
          js.elements.map {
            case js1: Json.Arr => coordinates0(trace, js1)
            case _             => Lexer.error("expected list", trace)
          }.toList
        def coordinates2(
          trace: List[JsonError],
          js: Json.Arr
        ): List[List[(Double, Double)]] =
          js.elements.map {
            case js1: Json.Arr => coordinates1(trace, js1)
            case _             => Lexer.error("expected list", trace)
          }.toList
        def coordinates3(
          trace: List[JsonError],
          js: Json.Arr
        ): List[List[List[(Double, Double)]]] =
          js.elements.map {
            case js1: Json.Arr => coordinates2(trace, js1)
            case _             => Lexer.error("expected list", trace)
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
            while ({
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                (field: @switch) match {
                  case 0 =>
                    if (subtype != -1) Lexer.error("duplicate", trace_)
                    subtype = Lexer.enumeration(trace_, in, subtypes)
                  case 1 =>
                    if (coordinates != null) Lexer.error("duplicate", trace_)
                    coordinates = coordinatesD.unsafeDecode(trace_, in)
                  case 2 =>
                    if (geometries != null) Lexer.error("duplicate", trace_)
                    geometries = geometriesD.unsafeDecode(trace_, in)
                }
              }
              Lexer.nextField(trace, in)
            }) ()

          if (subtype == -1) Lexer.error("missing discriminator", trace)
          if (subtype == 6) {
            if (geometries == null) Lexer.error("missing 'geometries' field", trace)
            else GeometryCollection(geometries)
          }
          if (coordinates == null) Lexer.error("missing 'coordinates' field", trace)
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
    implicit lazy val circeJsonCodec: Codec[Geometry] = {
      implicit val c1: Codec[Point]              = deriveCodec
      implicit val c2: Codec[MultiPoint]         = deriveCodec
      implicit val c3: Codec[LineString]         = deriveCodec
      implicit val c4: Codec[MultiLineString]    = deriveCodec
      implicit val c5: Codec[Polygon]            = deriveCodec
      implicit val c6: Codec[MultiPolygon]       = deriveCodec
      implicit val c8: Codec[GeometryCollection] = deriveCodec
      Codec.from(
        Decoder.instance(c =>
          c.downField("type").as[String].flatMap {
            case "Point"              => c.as[Point]
            case "MultiPoint"         => c.as[MultiPoint]
            case "LineString"         => c.as[LineString]
            case "MultiLineString"    => c.as[MultiLineString]
            case "Polygon"            => c.as[Polygon]
            case "MultiPolygon"       => c.as[MultiPolygon]
            case "GeometryCollection" => c.as[GeometryCollection]
          }
        ),
        Encoder.instance {
          case x: Point              => x.asJson.mapObject(_.+:("type" -> "Point".asJson))
          case x: MultiPoint         => x.asJson.mapObject(_.+:("type" -> "MultiPoint".asJson))
          case x: LineString         => x.asJson.mapObject(_.+:("type" -> "LineString".asJson))
          case x: MultiLineString    => x.asJson.mapObject(_.+:("type" -> "MultiLineString".asJson))
          case x: Polygon            => x.asJson.mapObject(_.+:("type" -> "Polygon".asJson))
          case x: MultiPolygon       => x.asJson.mapObject(_.+:("type" -> "MultiPolygon".asJson))
          case x: GeometryCollection => x.asJson.mapObject(_.+:("type" -> "GeometryCollection".asJson))
        }
      )
    }
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
        import zio.json._
        import JsonDecoder.JsonError
        import internal._

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
            while ({
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                (field: @switch) match {
                  case 0 =>
                    if (subtype != -1) Lexer.error("duplicate", trace_)
                    subtype = Lexer.enumeration(trace_, in, subtypes)
                  case 1 =>
                    if (properties != null) Lexer.error("duplicate", trace_)
                    properties = propertyD.unsafeDecode(trace_, in)
                  case 2 =>
                    if (geometry != null) Lexer.error("duplicate", trace_)
                    geometry = geometryD.unsafeDecode(trace_, in)
                  case 3 =>
                    if (features != null) Lexer.error("duplicate", trace_)
                    features = featuresD.unsafeDecode(trace_, in)
                }
              }
              Lexer.nextField(trace, in)
            }) ()

          // we could infer the type but that would mean accepting invalid data
          if (subtype == -1) Lexer.error("missing required fields", trace)
          if (subtype == 0) {
            if (properties == null) Lexer.error("missing 'properties' field", trace)
            if (geometry == null) Lexer.error("missing 'geometry' field", trace)
            Feature(properties, geometry)
          } else {
            if (features == null) Lexer.error("missing 'features' field", trace)
            FeatureCollection(features)
          }
        }

      }
    implicit lazy val zioJsonEncoder: JsonEncoder[GeoJSON] =
      DeriveJsonEncoder.gen[GeoJSON]
    implicit lazy val circeCodec: Codec[GeoJSON] = {
      implicit val c1: Codec[Feature]           = deriveCodec
      implicit val c2: Codec[FeatureCollection] = deriveCodec
      Codec.from(
        Decoder.instance(c =>
          c.downField("type").as[String].flatMap {
            case "Feature"           => c.as[Feature]
            case "FeatureCollection" => c.as[FeatureCollection]
          }
        ),
        Encoder.instance {
          case x: Feature           => x.asJson.mapObject(_.+:("type" -> "Feature".asJson))
          case x: FeatureCollection => x.asJson.mapObject(_.+:("type" -> "FeatureCollection".asJson))
        }
      )
    }
  }
}
