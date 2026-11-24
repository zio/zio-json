package zio.json.interop

/**
 * Brings `jsonOf` and `jsonEncoderOf` into scope with `import zio.json.interop.http4s._`. The package object extends
 * [[ZIOJsonInstances]]; it defines no implicit instances itself (see [[ZIOEntityCodec]] for those).
 */
package object http4s extends ZIOJsonInstances
