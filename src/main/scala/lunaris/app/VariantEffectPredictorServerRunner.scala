package lunaris.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, Multipart}
import akka.http.scaladsl.server.Directives.{complete, get, path, _}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import better.files.File
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.{InputId, ResourceConfig}
import lunaris.io.query.{HeaderExtractor, HeaderJson}
import lunaris.recipes.parsing.RecordExpressionParser
import lunaris.streams.TsvHeader
import lunaris.utils.{HttpUtils, SnagJson}
import lunaris.varianteffect.{ResultFileManager, VariantEffectFormData, VariantEffectJson}
import lunaris.varianteffect.ResultFileManager.ResultId

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import scala.util.{Failure, Success}

object VariantEffectPredictorServerRunner {

  object Defaults {
    val host: String = "localhost"
    val port: Int = 8080
  }

  def run(hostOpt: Option[String], portOpt: Option[Int], resultsFolder: File,
          dataFileWithIndex: BlockGzippedWithIndex, varId: String): Unit = {
    val host = hostOpt.getOrElse(Defaults.host)
    val port = portOpt.getOrElse(Defaults.port)
    val resourceConfig = ResourceConfig.empty
    val resultFileManager = new ResultFileManager(resultsFolder, dataFileWithIndex, varId, resourceConfig)
    resultFileManager.resultsFolderOrSnag() match {
      case Left(snag) =>
        println("Unable to establish storage for result file")
        println(snag.message)
        println("Exiting.")
      case Right(_) =>
        implicit val actorSystem: ActorSystem = ActorSystem("Lunaris-Actor-System")
        implicit val materializer: Materializer = Materializer(actorSystem)
        implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
        val route: Route =
          concat(
            path("lunaris" / "predictor.html") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/html(UTF-8)`, "web/predictor.html")
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
            path("lunaris" / "js" / "predictor.js") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.js, "web/js/predictor.js")
                )
              }
            },
            path("lunaris" / "predictor" / "upload") {
              post {
                decodeRequest {
                  entity(as[Multipart.FormData]) { httpFormData =>
                    val uploadFut = httpFormData.parts.mapAsync(1) {
                      VariantEffectFormData.FormField.bodyPartToFieldFut(_)
                    }.runFold(Map.empty[String, VariantEffectFormData.FormField]) { (fieldsByName, field) =>
                      fieldsByName + (field.name -> field)
                    }.map(VariantEffectFormData.fromFields).map { variantEffectFormData =>
                      resultFileManager.submit(variantEffectFormData)
                    }
                    onComplete(uploadFut) {
                      case Success(submissionResponse) =>
                        complete(
                          HttpUtils.ResponseBuilder.fromPlainTextString(submissionResponse.resultId.toString)
                        )
                      case Failure(ex) =>
                        complete(
                          HttpUtils.ResponseBuilder.fromPlainTextString(ex.getMessage)
                        )
                    }
                  }
                }
              }
            },
            path("lunaris" / "predictor" / "status" / Remaining) { resultIdString =>
              get {
                val status = ResultId.fromString(resultIdString) match {
                  case Left(snag) => ResultFileManager.ResultStatus.createInvalid(snag.message)
                  case Right(resultId) => resultFileManager.getStatus(resultId)
                }
                complete(
                  HttpUtils.ResponseBuilder.fromJson(VariantEffectJson.resultStatusToJson(status))
                )
              }
            },
            path("lunaris" / "predictor" / "results" / Remaining) { resultIdString =>
              get {
                val snagOrSource = for {
                  resultId <- ResultId.fromString(resultIdString)
                  source <- resultFileManager.streamResults(resultId)
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
                HeaderExtractor.extractHeader(dataFileWithIndex, resourceConfig).useUp {
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
        HttpUtils.runWebServiceWhileWaiting(route, host, port)(StdIn.readLine())
    }
  }
}
