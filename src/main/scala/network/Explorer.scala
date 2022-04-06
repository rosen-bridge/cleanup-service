package network

import helpers.{Configs, RosenLogging}
import helpers.RosenExceptions._
import io.circe.parser._

class Explorer extends RosenLogging {
  private val baseUrlV1 = s"${Configs.explorer}/api/v1"
  private val unspentBoxSearch = s"$baseUrlV1/boxes/unspent/search"

  /**
   * @param ergoTreeTemplateHash ergo tree template hash of target address (sha256)
   * @param tokenId token id to search for
   * @return list of unspent boxes containing the token
   */
  def getUnspentTokenBoxIdsForAddress(ergoTreeTemplateHash: String, tokenId: String): Seq[String] = try {
    val postData =
      s"""
         | {
         |    "ergoTreeTemplateHash": "$ergoTreeTemplateHash",
         |    "assets": [
         |      "$tokenId"
         |    ]
         |}""".stripMargin
    println(s"[*] post data:\n$postData")
    parse(Request.httpPost(unspentBoxSearch, postData).toString()) match {
      case Right(js) => (js \\ "items").headOption.getOrElse(return List.empty[String]).asArray.get
        .map(boxJson => (boxJson \\ "boxId").head.asString.get)
      case Left(e) =>
        log.error(e.getMessage)
        throw parseJsonException("method: getUnspentTokenBoxIdsForAddress")
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
