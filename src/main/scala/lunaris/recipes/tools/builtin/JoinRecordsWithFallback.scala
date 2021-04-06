package lunaris.recipes.tools.builtin

import akka.stream.Materializer
import lunaris.app.VepRunSettings
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.RecordStreamJoinerWithFallback
import lunaris.streams.utils.RecordStreamTypes.Record
import lunaris.utils.EitherSeqUtils
import lunaris.vep.{VepRunSettingsBox, VepRunner}
import org.broadinstitute.yootilz.core.snag.Snag

object JoinRecordsWithFallback extends Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val driver: String = "driver"
      val data: String = "data"
      val fallback: String = "fallback"
    }

    val driver: Tool.RefParam = Tool.RefParam(Keys.driver, LunType.RecordStreamType, isRequired = true)
    val data: Tool.RefParam = Tool.RefParam(Keys.data, LunType.ArrayType(LunType.RecordStreamType), isRequired = true)
    val fallback: Tool.ValueParam = Tool.ValueParam(Keys.fallback, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.driver, Params.data, Params.fallback)

  override def isFinal: Boolean = false

  sealed trait FallbackGenerator {
    def createFallback(snagLogger: Snag => (), materializer: Materializer): Record => Either[Snag, Record]
  }

  class VepFallbackGenerator(vepRunSettings: VepRunSettings) extends FallbackGenerator {
    private val vepRunner = VepRunner.createNewVepRunner(vepRunSettings)
    override def createFallback(snagLogger: Snag => (), materializer: Materializer): Record => Either[Snag, Record] =
      (record: Record) => vepRunner.processRecord(record, snagLogger)(materializer)
  }

  private def getFallbackGenerator(fallbackString: String): Either[Snag, FallbackGenerator] = {
    fallbackString match {
      case "vep" => VepRunSettingsBox.getVepRunSettings.map(new VepFallbackGenerator(_))
      case _ => Left(Snag(s"Unknown fallback '$fallbackString'"))
    }
  }

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      driver <- ToolArgUtils.asRef(Params.Keys.driver, args)
      datas <- ToolArgUtils.asRefs(Params.Keys.data, args)
      fallbackString <- ToolArgUtils.asString(Params.Keys.fallback, args)
      fallbackGenerator <- getFallbackGenerator(fallbackString)
    } yield ToolInstance(driver, datas, fallbackGenerator)
  }

  case class ToolInstance(driver: String,
                          datas: Seq[String],
                          fallbackGenerator: FallbackGenerator) extends tools.ToolInstance {
    override def refs: Map[String, String] = {
      val dataRefs = datas.zipWithIndex.collect {
        case (ref, index) => (Params.Keys.data + index, ref)
      }.toMap
      Map(Params.Keys.driver -> driver) ++ dataRefs
    }

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1Set(Params.Keys.driver, Seq(Params.Keys.data), workers) {
        (driverWorker, dataWorkers) => new WorkerMaker(driverWorker, dataWorkers, fallbackGenerator)
      }
    }
  }

  class WorkerMaker(driverWorker: RecordStreamWorker,
                    dataWorkers: Seq[RecordStreamWorker],
                    fallbackGenerator: FallbackGenerator) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] = {
        Some {
          (context: LunRunContext, runTracker: RunTracker) => {
            val snagOrStream =
              for {
                driverStream <- driverWorker.getStreamBox(context, runTracker).snagOrStream
                dataStreams <-
                  EitherSeqUtils.sequence(dataWorkers.map(_.getStreamBox(context, runTracker).snagOrStream))
                metaJoined <- Meta.sequence(driverStream.meta +: dataStreams.map(_.meta))
              } yield {
                val fallback = fallbackGenerator.createFallback(runTracker.snagTracker.trackSnag, context.materializer)
                val sourceJoined = RecordStreamJoinerWithFallback.joinWithFallback(metaJoined,
                  driverStream.source, dataStreams.map(_.source)) {
                  _.joinWith(_)
                } {
                  fallback
                } {
                  (snag: Snag) => runTracker.snagTracker.trackSnag(snag)
                } {
                  dataRecordId => runTracker.statsTracker.postMessage(s"Cache hit for $dataRecordId")
                } {
                  fallbackRecordId => runTracker.statsTracker.postMessage(s"Cache miss for $fallbackRecordId")
                }
                RecordStreamWithMeta(metaJoined, sourceJoined)
              }
            LunWorker.StreamBox(snagOrStream)
          }
        }
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
