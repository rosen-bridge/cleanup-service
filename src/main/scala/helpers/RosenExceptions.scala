package helpers

object RosenExceptions {
  private def ifNonEmpty(info: String): String = if (info.nonEmpty) s", $info" else ""

  final case class ConnectionException(private val message: String = "Network Error")
    extends Throwable(message)
  final case class ProveException(private val message: String = "Tx proving failed")
    extends Throwable(message)
  final case class NotEnoughErgException(found: Long, required: Long, private val message: String = "Not enough erg in address")
    extends Throwable(message + s", found $found, required $required")
  final case class FailedTxException(info: String = "", private val message: String = "Tx sending failed")
    extends Throwable(message + ifNonEmpty(info))
  final case class UnexpectedException(info: String = "", private val message: String = "Unexpected case happened")
    extends Throwable(message + ifNonEmpty(info))
}
