package zio.json

import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RetractReader, StringMatrix }

import scala.annotation.tailrec
import scala.collection.immutable
import scala.compiletime.*
import scala.compiletime.ops.any.IsConst
import scala.NamedTuple.{ AnyNamedTuple, DropNames, NamedTuple, Names }

private[json] trait JsonDecoderVersionSpecific {
  implicit def arraySeq[A: JsonDecoder: reflect.ClassTag]: JsonDecoder[immutable.ArraySeq[A]] =
    new CollectionJsonDecoder[immutable.ArraySeq[A]] {
      private[this] val arrayDecoder = JsonDecoder.array[A]

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ArraySeq[A] = immutable.ArraySeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.ArraySeq[A] =
        immutable.ArraySeq.unsafeWrapArray(arrayDecoder.unsafeDecode(trace, in))
    }

  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonDecoder[A] =
    DeriveJsonDecoder.gen[A]

  implicit def iArray[A](implicit A: JsonDecoder[A], classTag: reflect.ClassTag[A]): JsonDecoder[IArray[A]] =
    JsonDecoder.array[A].map(IArray.unsafeFromArray)

}

trait DecoderLowPriorityVersionSpecific extends DecoderNamedTuples {
  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonDecoder[T] =
    val values = UnionDerivation.constValueUnionTuple[String, T]
    JsonDecoder.string.mapOrFail {
      case raw if values.toList.contains(raw) => Right(raw.asInstanceOf[T])
      case _                                  => Left("expected one of: " + values.toList.mkString(", "))
    }
}

trait DecoderNamedTuples {
  inline given deriveJsonDecoder[T <: AnyNamedTuple](using config: JsonCodecConfiguration): JsonDecoder[T] =
    namedTupleDecoder[T](
      paramCount = constValue[NamedTuple.Size[T]],
      paramNames = constValueTuple[Names[T]],
      paramDecoders = summonAll[Tuple.Map[DropNames[T], JsonDecoder]],
      config = config
    )

  final def namedTupleDecoder[T <: AnyNamedTuple](
    paramCount: Int,
    paramNames: Names[T],
    paramDecoders: => Tuple.Map[DropNames[T], JsonDecoder],
    config: JsonCodecConfiguration
  ): JsonDecoder[T] =
    if (paramCount == 0)
      DeriveHelpers.buildEmptyProductDecoder[T](!config.allowExtraFields, _ => EmptyTuple.asInstanceOf[T])
    else new NamedTupleDecoder[T](paramNames, paramDecoders, config)
}

/**
 * Decodes a non-empty named tuple from a JSON object. Unlike case classes, named tuples have no annotations, aliases or
 * default values, so the only thing that shapes decoding is the [[JsonCodecConfiguration]].
 */
private[json] final class NamedTupleDecoder[T <: AnyNamedTuple](
  fieldNames: Tuple,
  decoders: => Tuple,
  config: JsonCodecConfiguration
) extends CollectionJsonDecoder[T] {
  private[this] val names: Array[String] = {
    val nameTransformer: String => String = config.fieldNameMapping
    fieldNames.productIterator.map(name => nameTransformer(name.asInstanceOf[String])).toArray
  }
  require(
    names.length == names.distinct.length,
    s"Field names of named tuple (${names.mkString(", ")}) must be distinct after applying ${config.fieldNameMapping}"
  )

  private[this] val len: Int                = names.length
  private[this] val noExtra: Boolean        = !config.allowExtraFields
  private[this] val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

  // Streaming decoding only needs the matrices and AST decoding only needs the map, so build each on first use.
  private[this] lazy val matrix1: StringMatrix      = new StringMatrix(names.take(64))
  private[this] lazy val matrix2: StringMatrix      = if (len > 64) new StringMatrix(names.drop(64)) else null
  private[this] lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap

  private[this] lazy val tcs: Array[JsonDecoder[Any]] =
    decoders.productIterator.map(_.asInstanceOf[JsonDecoder[Any]]).toArray

  // When empty collections must be explicit, a missing field may only fall back to its decoder's
  // `unsafeDecodeMissing` if that decoder isn't a collection; `null` marks fields that must be present.
  private[this] lazy val missingDecoders: Array[JsonDecoder[Any]] =
    if (config.explicitEmptyCollections.decoding) tcs.map(d => if (allowsMissing(d)) d else null)
    else tcs

  @tailrec
  private[this] def allowsMissing(d: JsonDecoder[?]): Boolean = d match {
    case _: OptionJsonDecoder[?]     => true
    case _: CollectionJsonDecoder[?] => false
    case d: MappedJsonDecoder[?]     => allowsMissing(d.underlying)
    case _                           => true
  }

  private[this] def construct(ps: Array[Any], trace: List[JsonError]): T = {
    var i = 0
    while (i < len) {
      if (ps(i) == null) {
        val trace_  = spans(i) :: trace
        val decoder = missingDecoders(i)
        if (decoder eq null) Lexer.error("missing", trace_)
        ps(i) = decoder.unsafeDecodeMissing(trace_)
      }
      i += 1
    }
    Tuple.fromIArray(IArray.unsafeFromArray(ps)).asInstanceOf[T]
  }

  def unsafeDecode(trace: List[JsonError], in: RetractReader): T = {
    Lexer.char(trace, in, '{')
    val ps = new Array[Any](len)
    if (Lexer.firstField(in)) {
      while ({
        val field =
          if (matrix2 eq null) Lexer.field(trace, in, matrix1) else Lexer.field128(trace, in, matrix1, matrix2)
        if (field != -1) {
          if (ps(field) != null) Lexer.error("duplicate", trace)
          ps(field) = tcs(field).unsafeDecode(spans(field) :: trace, in)
        } else if (noExtra) Lexer.error("invalid extra field", trace)
        else Lexer.skipValue(trace, in)
        Lexer.nextField(trace, in)
      }) ()
    }
    construct(ps, trace)
  }

  override def unsafeFromJsonAST(trace: List[JsonError], json: Json): T =
    json match {
      case Json.Obj(fields) =>
        val ps = new Array[Any](len)
        val it = fields.iterator
        while (it.hasNext) {
          val (key, value) = it.next()
          val field        = namesMap.getOrElse(key, -1)
          if (field != -1) {
            if (ps(field) != null) Lexer.error("duplicate", trace)
            ps(field) = tcs(field).unsafeFromJsonAST(spans(field) :: trace, value)
          } else if (noExtra) Lexer.error("invalid extra field", trace)
        }
        construct(ps, trace)
      case _ => Lexer.error("expected object", trace)
    }

  override def unsafeDecodeMissing(trace: List[JsonError]): T = construct(new Array[Any](len), trace)
}
