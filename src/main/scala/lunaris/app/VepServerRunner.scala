package lunaris.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, Multipart}
import akka.http.scaladsl.server.Directives.{complete, get, path, _}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import io.circe.Json
import lunaris.io.ResourceConfig
import lunaris.io.query.{HeaderExtractor, HeaderJson}
import lunaris.utils.{AkkaUtils, Crypt, DebugUtils, HttpUtils, SnagJson}
import lunaris.vep.VepJobManager.{JobId, SessionId}
import lunaris.vep.{VepFormData, VepJobManager, VepJson, VepMasksManager}
import org.broadinstitute.yootilz.core.snag.Snag

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import scala.util.{Failure, Success}

object VepServerRunner {
  private def askForEncryptionKey(): String = {
    print("Please enter key to decode encrypted properties: ")
    StdIn.readLine()
  }
  def run(vepServerSettings: VepServerSettings): Unit = {
    val serverSettings = vepServerSettings.serverSettings
    val host = serverSettings.webInterface
    val port = serverSettings.port
    val vepSettings = vepServerSettings.vepSettings
    val resourceConfig = ResourceConfig.empty
    val emailSettings = vepServerSettings.emailSettings
    val dbName = vepServerSettings.dbName
    val encryptionKey = askForEncryptionKey()
    val crypt = new Crypt(encryptionKey)
    val emailApiKey = crypt.decrypt(emailSettings.keyEncrypted)
    val vepFileManager = new VepJobManager(vepSettings, emailSettings, dbName, emailApiKey, resourceConfig)
    vepFileManager.vepFolders.foldersExistOrSnag() match {
      case Left(snag) =>
        println("Unable to establish storage for inputs and results.")
        println(snag.message)
        println("Exiting.")
      case Right(_) =>
        implicit val actorSystem: ActorSystem = ActorSystem("Lunaris-Actor-System")
        implicit val materializer: Materializer = Materializer(actorSystem)
        implicit val executionContext: ExecutionContextExecutor = AkkaUtils.getDispatcher(actorSystem)
        val route: Route = {
          concat(
            path("lunaris" / "vep.html") {
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/html(UTF-8)`,
                    "web/vep.html")
                )
              }
            },
            path("lunaris" / "css" / Remaining) { remaining =>
              val location = "web/css/" + remaining
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.css, location)
                )
              }
            },
            path("lunaris" / "js" / Remaining) { remaining =>
              val location = "web/js/" + remaining
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.js, location)
                )
              }
            },
            path("lunaris" / "svg" / Remaining) { remaining =>
              val location = "web/svg/" + remaining
              get {
                complete(
                  HttpUtils.ResponseBuilder.fromResourceOrError(HttpUtils.ContentTypes.svg, location)
                )
              }
            },
            path("lunaris" / "codemirror" / Remaining) { remaining =>
              get {
                complete {
                  val contentType =
                    if (remaining.endsWith(".js")) {
                      HttpUtils.ContentTypes.js
                    } else if (remaining.endsWith(".css")) {
                      HttpUtils.ContentTypes.css
                    } else {
                      HttpUtils.ContentTypes.plain
                    }
                  val location = "web/codemirror/" + remaining
                  HttpUtils.ResponseBuilder.fromResourceOrError(contentType, location)
                }
              }
            },
            path("lunaris" / "predictor" / "upload") {
              post {
                withSizeLimit(1000000000L) {
                  decodeRequest {
                    entity(as[Multipart.FormData]) { httpFormData =>
                      val uploadFut = httpFormData.parts.mapAsync(3) {
                        VepFormData.FormField.bodyPartToFieldFut(_, vepFileManager)
                      }.runFold(Map.empty[String, VepFormData.FormField]) { (fieldsByName, field) =>
                        fieldsByName + (field.name -> field)
                      }.map(VepFormData.fromFields).map { variantEffectFormData =>
                        vepFileManager.submit(variantEffectFormData)
                      }
                      onComplete(uploadFut) {
                        case Success(submissionResponse) =>
                          println("Submission response: " + submissionResponse.jobId)
                          complete(
                            HttpUtils.ResponseBuilder.fromPlainTextString(submissionResponse.jobId.toString)
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
                val status = vepFileManager.getStatus(JobId(resultIdString))
                complete(
                  HttpUtils.ResponseBuilder.fromJson(VepJson.resultStatusToJson(status))
                )
              }
            },
            path("lunaris" / "predictor" / "session" / Remaining) { sessionIdString =>
              get {
                val session = vepFileManager.getSession(SessionId(sessionIdString))
                complete(
                  HttpUtils.ResponseBuilder.fromJson(VepJson.snagOrSessionOptEncoder(session))
                )
              }
            },
            path("lunaris" / "predictor" / "results" / Remaining) { rawString =>
              get {
                val resultIdString =
                  if(rawString.endsWith(".tsv")) rawString.dropRight(4) else rawString
                val resultId = JobId(resultIdString)
                val snagOrSource = for {
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
                HeaderExtractor.extractHeader(vepSettings.hg38Settings.dataFileWithIndex, resourceConfig).useUp {
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
            },
            path("lunaris" / "predictor" / "masks" / "list") {
              get {
                val maskNamesJson = Json.fromValues(VepMasksManager.maskNames.map(Json.fromString))
                complete(
                  HttpUtils.ResponseBuilder.fromJson(maskNamesJson)
                )
              }
            },
            path("lunaris" / "predictor" / "masks" / Remaining) { remaining =>
              get {
                val maskPath = VepMasksManager.getPathForMask(remaining)
                complete {
                  HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/plain(UTF-8)`, maskPath)
                }
              }
            },
            path("lunaris" / "predictor" / "sample_input.vcf") {
              get {
                val sampleInputPath = "lunaris/vep/sample_input.vcf"
                complete {
                  HttpUtils.ResponseBuilder.fromResourceOrError(ContentTypes.`text/plain(UTF-8)`, sampleInputPath)
                }
              }
            },
          )
        }
        HttpUtils.runWebServiceWhileWaiting(route, host, port)(StdIn.readLine())
    }
  }
}
