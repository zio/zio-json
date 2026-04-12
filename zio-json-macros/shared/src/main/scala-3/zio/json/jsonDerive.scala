package zio.json

import scala.annotation.StaticAnnotation

/**
 * Scala 3 compatible version of @jsonDerive.
 *
 * NOTE:
 * Macro annotations are not supported in Scala 3.
 * This exists only for source compatibility.
 *
 * Use DeriveJsonCodec.gen[A] instead.
 */
final class jsonDerive extends StaticAnnotation
