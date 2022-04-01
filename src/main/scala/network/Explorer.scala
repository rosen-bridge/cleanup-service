package network

import helpers.{Configs, RosenLogging}
import helpers.RosenExceptions._
import io.circe.parser._

class Explorer extends RosenLogging {
  private val baseUrlV0 = s"${Configs.explorer}/api/v0"
  private val baseUrlV1 = s"${Configs.explorer}/api/v1"
  private val unspentBoxesByTokenId = s"$baseUrlV1/boxes/unspent/byTokenId"

  /**
   * @param tokenId token id to search for
   * @return list of unspent boxes containing the token
   */
  def getUnspentTokenBoxIds(tokenId: String): Seq[String] = try {
    parse(Request.httpGet(s"$unspentBoxesByTokenId/$tokenId").toString()) match {
      case Right(js) => (js \\ "items").headOption.getOrElse(return List.empty[String]).asArray.get
        .map(boxJson => (boxJson \\ "boxId").head.asString.get)
      case Left(e) =>
        log.error(e.getMessage)
        throw parseJsonException("method: getUnspentTokenBoxIds")
    }
  } catch {
    case e: requestException =>
      log.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      log.error(e.getMessage)
      throw connectionException()
  }

}
