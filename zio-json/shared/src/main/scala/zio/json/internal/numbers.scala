/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json.internal

// specialised Options to avoid boxing. Prefer .isEmpty guarded access to .value
// for higher performance: pattern matching is slightly slower.

sealed abstract class ByteOption {
  def isEmpty: Boolean
  def value: Byte
}

case object ByteNone extends ByteOption {
  def isEmpty     = true
  def value: Byte = throw new java.util.NoSuchElementException
}

case class ByteSome(value: Byte) extends ByteOption {
  def isEmpty = false
}

sealed abstract class ShortOption {
  def isEmpty: Boolean
  def value: Short
}

case object ShortNone extends ShortOption {
  def isEmpty      = true
  def value: Short = throw new java.util.NoSuchElementException
}

case class ShortSome(value: Short) extends ShortOption {
  def isEmpty = false
}

sealed abstract class IntOption {
  def isEmpty: Boolean
  def value: Int
}

case object IntNone extends IntOption {
  def isEmpty    = true
  def value: Int = throw new java.util.NoSuchElementException
}

case class IntSome(value: Int) extends IntOption {
  def isEmpty = false
}

sealed abstract class LongOption {
  def isEmpty: Boolean
  def value: Long
}

case object LongNone extends LongOption {
  def isEmpty     = true
  def value: Long = throw new java.util.NoSuchElementException
}

case class LongSome(value: Long) extends LongOption {
  def isEmpty = false
}

sealed abstract class FloatOption {
  def isEmpty: Boolean
  def value: Float
}

case object FloatNone extends FloatOption {
  def isEmpty      = true
  def value: Float = throw new java.util.NoSuchElementException
}

case class FloatSome(value: Float) extends FloatOption {
  def isEmpty = false
}

sealed abstract class DoubleOption {
  def isEmpty: Boolean
  def value: Double
}

case object DoubleNone extends DoubleOption {
  def isEmpty       = true
  def value: Double = throw new java.util.NoSuchElementException
}

case class DoubleSome(value: Double) extends DoubleOption {
  def isEmpty = false
}
