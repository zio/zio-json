package zio.json

import zio.Chunk
import zio.json.ast.Json
import zio.json.internal.{ Write, FieldEncoder }

import scala.collection.immutable
import scala.compiletime.ops.any.IsConst
import scala.compiletime.{ constValue, summonAll, constValueTuple }
import scala.NamedTuple.{ AnyNamedTuple, Names, DropNames, NamedTuple, withNames }

private[json] trait JsonEncoderVersionSpecific {
  implicit def arraySeq[A: JsonEncoder: scala.reflect.ClassTag]: JsonEncoder[immutable.ArraySeq[A]] =
    new JsonEncoder.AbstractJsonEncoder[immutable.ArraySeq[A]] {
      private[this] val arrayEnc = JsonEncoder.array[A]

      override def isEmpty(as: immutable.ArraySeq[A]): Boolean = as.isEmpty

      def unsafeEncode(as: immutable.ArraySeq[A], indent: Option[Int], out: Write): Unit =
        arrayEnc.unsafeEncode(as.unsafeArray.asInstanceOf[Array[A]], indent, out)

      override final def toJsonAST(as: immutable.ArraySeq[A]): Either[String, Json] =
        arrayEnc.toJsonAST(as.unsafeArray.asInstanceOf[Array[A]])
    }

  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonEncoder[A] =
    DeriveJsonEncoder.gen[A]

  implicit def iArray[A](implicit A: JsonEncoder[A], classTag: scala.reflect.ClassTag[A]): JsonEncoder[IArray[A]] =
    JsonEncoder.array[A].contramap[IArray[A]](arr => IArray.genericWrapArray(arr).toArray)

}

private[json] trait EncoderLowPriorityVersionSpecific extends EncoderNamedTuplesVersionSpecific {
  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonEncoder[T] =
    JsonEncoder.string.asInstanceOf[JsonEncoder[T]]
}

private[json] trait EncoderNamedTuplesVersionSpecific {
  inline given deriveJsonEncoder[T <: AnyNamedTuple](using config: JsonCodecConfiguration): JsonEncoder[T] =
    namedTupleEncoder[T, Names[T], DropNames[T]](
      paramCount = constValue[NamedTuple.Size[T]],
      paramNames = constValueTuple[NamedTuple.Names[T]],
      paramEncoders = summonAll[Tuple.Map[DropNames[T], JsonEncoder]],
      config = config
    )

  final def namedTupleEncoder[T <: AnyNamedTuple, N <: Tuple, V <: Tuple](
    paramCount: Int,
    paramNames: NamedTuple.Names[T],
    paramEncoders: => Tuple.Map[DropNames[T], JsonEncoder],
    config: JsonCodecConfiguration
  ): JsonEncoder[T] =
    if (paramCount == 0) DeriveHelpers.buildEmptyProductEncoder[T]
    else new NamedTupleEncoder(paramCount, paramNames, paramEncoders, config).asInstanceOf[JsonEncoder[T]]
}

private[json] class NamedTupleEncoder[T <: AnyNamedTuple, N <: Tuple, V <: Tuple](
  fieldSize: Int,
  fieldNames: NamedTuple.Names[T],
  encoders: => Tuple.Map[DropNames[T], JsonEncoder],
  config: JsonCodecConfiguration
) extends JsonEncoder[NamedTuple[N, V]] {
  private lazy val fieldValues: Array[V => Any]          = Array.tabulate(fieldSize)(i => (v: V) => v(i))
  private lazy val fields: Array[FieldEncoder[Any, Int]] = {
    val nameTransformer: String => String = config.fieldNameMapping
    val explicitNulls                     = config.explicitNulls
    val explicitEmptyCollections          = config.explicitEmptyCollections.encoding

    val enc = encoders.productIterator
    fieldNames.productIterator
      .zip(enc)
      .zipWithIndex
      .map { case ((name, encoder), index) =>
        val fieldName = nameTransformer(name.asInstanceOf[String])
        FieldEncoder[Any, Int](
          p = index,
          name = fieldName,
          encoder = encoder.asInstanceOf[JsonEncoder[Any]],
          withExplicitNulls = explicitNulls,
          withExplicitEmptyCollections = explicitEmptyCollections
        )
      }
      .toArray
  }

  override def unsafeEncode(a: NamedTuple[N, V], indent: Option[Int], out: Write): Unit = {
    out.write('{')
    val indent_ = JsonEncoder.bump(indent)
    val fs      = this.fields
    var idx     = 0
    var comma   = false
    while (idx < fs.length) {
      val field = fs(idx)
      val p     = a(field.p)
      idx += 1
      if (field.skip(p)) ()
      else {
        if (comma) out.write(',')
        else comma = true
        JsonEncoder.pad(indent_, out)
        out.write(if (indent eq None) field.encodedName else field.prettyEncodedName)
        field.encoder.unsafeEncode(p, indent_, out)
      }
    }
    JsonEncoder.pad(indent, out)
    out.write('}')
  }

  override final def toJsonAST(a: NamedTuple[N, V]): Either[String, Json] = {
    val fs     = this.fields
    var buf    = new Array[(String, Json)](fs.length)
    var i, idx = 0
    while (idx < fs.length) {
      val field = fs(idx)
      val p     = fieldValues(field.p)(a)
      idx += 1
      if (field.skip(p)) ()
      else {
        field.encoder.toJsonAST(p) match {
          case Right(value) =>
            buf(i) = (field.name, value)
            i += 1
          case left =>
            return left
        }
      }
    }
    if (i != buf.length) buf = java.util.Arrays.copyOf(buf, i)
    new Right(Json.Obj(Chunk.fromArray(buf)))
  }
}
