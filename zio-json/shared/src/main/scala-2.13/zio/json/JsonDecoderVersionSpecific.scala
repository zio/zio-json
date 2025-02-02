package zio.json

import zio.json.JsonDecoder.JsonError
import zio.json.internal.RetractReader

import scala.collection.immutable

private[json] trait JsonDecoderVersionSpecific {
  implicit def arraySeq[A: JsonDecoder: reflect.ClassTag]: JsonDecoder[immutable.ArraySeq[A]] =
    new CollectionJsonDecoder[immutable.ArraySeq[A]] {
      private[this] val arrayDecoder = JsonDecoder.array[A]

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ArraySeq[A] = immutable.ArraySeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.ArraySeq[A] =
        immutable.ArraySeq.unsafeWrapArray(arrayDecoder.unsafeDecode(trace, in))
    }
}

private[json] trait DecoderLowPriorityVersionSpecific
