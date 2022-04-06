package helpers

object RosenExceptions {
  private def ifNonEmpty(info: String): String = if (info.nonEmpty) s", $info" else ""

  final case class requestException(private val message: String = "Explorer error")
    extends Throwable(message)
  final case class connectionException(private val message: String = "Network Error")
    extends Throwable(message)
  final case class proveException(private val message: String = "Tx proving failed")
    extends Throwable(message)
  final case class failedTxException(info: String = "", private val message: String = "Tx sending failed")
    extends Throwable(message + ifNonEmpty(info))
  final case class parseJsonException(info: String = "", private val message: String = "Error occurred while parsing json")
    extends Throwable(message + ifNonEmpty(info))
  final case class unexpectedException(info: String = "", private val message: String = "Unexpected case happened")
    extends Throwable(message + ifNonEmpty(info))
}
