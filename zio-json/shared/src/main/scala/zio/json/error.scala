package zio.json

/**
 * A `JsonError` value describes the ways in which decoding could fail. This structure is used
 * to facilitate human-readable error messages during decoding failures.
 */
sealed abstract class JsonError

object JsonError {

  def render(trace: List[JsonError]): String =
    trace.reverse.map {
      case Message(txt)        => s"($txt)"
      case ArrayAccess(i)      => s"[$i]"
      case ObjectAccess(field) => s".$field"
      case SumType(cons)       => s"{$cons}"
    }.mkString

  final case class Message(txt: String) extends JsonError

  final case class ArrayAccess(i: Int) extends JsonError

  final case class ObjectAccess(field: String) extends JsonError

  final case class SumType(cons: String) extends JsonError

}
