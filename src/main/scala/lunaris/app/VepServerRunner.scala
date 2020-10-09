package lunaris.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, Multipart}
import akka.http.scaladsl.server.Directives.{complete, get, path, _}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import lunaris.io.ResourceConfig
import lunaris.io.query.{HeaderExtractor, HeaderJson}
import lunaris.utils.{HttpUtils, SnagJson}
import lunaris.vep.VepFileManager.ResultId
import lunaris.vep.{VepFileManager, VepFormData, VepJson, VepRunSettingsBox}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import scala.util.{Failure, Success}

object VepServerRunner {
  def run(vepServerSettings: VepServerSettings): Unit = {
    val serverSettings = vepServerSettings.serverSettings
    val host = serverSettings.webInterface
    val port = serverSettings.port
    val vepSettings = vepServerSettings.vepSettings
    VepRunSettingsBox.setVepRunSettings(vepSettings.runSettings)
    val resourceConfig = ResourceConfig.empty
    val vepFileManager = new VepFileManager(vepSettings, resourceConfig)
    vepFileManager.foldersExistOrSnag() match {
      case Left(snag) =>
        println("Unable to establish storage for inputs and results.")
        println(snag.message)
        println("Exiting.")
      case Right(_) =>
        implicit val actorSystem: ActorSystem = ActorSystem("Lunaris-Actor-System")
        implicit val materializer: Materializer = Materializer(actorSystem)
        implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
        val route: Route = {
          concat(
            path("lunaris" / "vep.html") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/html(UTF-8)`, "web/vep.html")
                )
              }
            },
            path("lunaris" / "css" / "lunaris.css") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.css, "web/css/lunaris.css")
                )
              }
            },
            path("lunaris" / "js" / "vep.js") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.js, "web/js/vep.js")
                )
              }
            },
            path("lunaris" / "predictor" / "upload") {
              post {
                withSizeLimit(1000000000L) {
                  decodeRequest {
                    entity(as[Multipart.FormData]) { httpFormData =>
                      val uploadFut = httpFormData.parts.mapAsync(1) {
                        VepFormData.FormField.bodyPartToFieldFut(_, vepFileManager)
                      }.runFold(Map.empty[String, VepFormData.FormField]) { (fieldsByName, field) =>
                        fieldsByName + (field.name -> field)
                      }.map(VepFormData.fromFields).map { variantEffectFormData =>
                        vepFileManager.submit(variantEffectFormData)
                      }
                      onComplete(uploadFut) {
                        case Success(submissionResponse) =>
                          println("Submission response: " + submissionResponse.resultId)
                          complete(
                            HttpUtils.ResponseBuilder.fromPlainTextString(submissionResponse.resultId.toString)
                          )
                        case Failure(ex) =>
                          println("Submission failed: " + ex.getMessage)
                          println(Snag(ex).report)
                          complete(
                            HttpUtils.ResponseBuilder.fromPlainTextString(ex.getMessage)
                          )
                      }
                    }
                  }
                }
              }
            },
            path("lunaris" / "predictor" / "status" / Remaining) { resultIdString =>
              get {
                val status = ResultId.fromString(resultIdString) match {
                  case Left(snag) => VepFileManager.ResultStatus.createInvalid(snag.message)
                  case Right(resultId) => vepFileManager.getStatus(resultId)
                }
                complete(
                  HttpUtils.ResponseBuilder.fromJson(VepJson.resultStatusToJson(status))
                )
              }
            },
            path("lunaris" / "predictor" / "results" / Remaining) { resultIdString =>
              get {
                val snagOrSource = for {
                  resultId <- ResultId.fromString(resultIdString)
                  source <- vepFileManager.streamResults(resultId)
                } yield source
                snagOrSource match {
                  case Left(snag) =>
                    complete(
                      HttpUtils.ResponseBuilder.forError(snag.message)
                    )
                  case Right(source) =>
                    complete(
                      HttpUtils.ResponseBuilder.fromTsvByteStream(source)
                    )
                }
              }
            },
            path("lunaris" / "predictor" / "schema") {
              get {
                HeaderExtractor.extractHeader(vepSettings.dataFileWithIndex, resourceConfig).useUp {
                  case Left(snag) =>
                    complete(
                      HttpUtils.ResponseBuilder.fromJson(SnagJson.snagEncoder(snag))
                    )
                  case Right(header) =>
                    complete(
                      HttpUtils.ResponseBuilder.fromJson(HeaderJson.headerEncoder(header))
                    )
                }
              }
            }
          )
        }
        HttpUtils.runWebServiceWhileWaiting(route, host, port)(StdIn.readLine())
    }
  }
}
