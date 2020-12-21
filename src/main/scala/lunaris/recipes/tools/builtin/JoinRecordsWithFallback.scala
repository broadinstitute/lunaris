package lunaris.recipes.tools.builtin

import lunaris.app.VepRunSettings
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.{MafForVepCalculator, RecordStreamJoinerWithFallback}
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
    val data: Tool.RefParam = Tool.RefParam(Keys.data, LunType.RecordStreamType, isRequired = true)
    val fallback: Tool.ValueParam = Tool.ValueParam(Keys.fallback, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.driver, Params.data, Params.fallback)

  override def isFinal: Boolean = false

  sealed trait FallbackGenerator {
    def createFallback(): Record => Either[Snag, Record]
  }

  class VepFallbackGenerator(vepRunSettings: VepRunSettings) extends FallbackGenerator {
    private val vepRunner = VepRunner.createNewVepRunner(vepRunSettings)
    override def createFallback(): Record => Either[Snag, Record] =
      (record: Record) => vepRunner.processRecord(record)
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
      data <- ToolArgUtils.asRef(Params.Keys.data, args)
      fallbackString <- ToolArgUtils.asString(Params.Keys.fallback, args)
      fallbackGenerator <- getFallbackGenerator(fallbackString)
    } yield ToolInstance(driver, data, fallbackGenerator)
  }

  case class ToolInstance(driver: String,
                          data: String,
                          fallbackGenerator: FallbackGenerator) extends tools.ToolInstance {
    override def refs: Map[String, String] = {
      Map(Params.Keys.driver -> driver, Params.Keys.data -> data)
    }

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker2(Params.Keys.driver, Params.Keys.data, workers) {
        (driverWorker, dataWorker) => new WorkerMaker(driverWorker, Seq(dataWorker), fallbackGenerator)
      }
    }
  }

  class WorkerMaker(driverWorker: RecordStreamWorker,
                    dataWorkers: Seq[RecordStreamWorker],
                    fallbackGenerator: FallbackGenerator) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] = {
        Some {
          (context: LunRunContext, snagTracker: SnagTracker) => {
            val snagOrStream =
              for {
                driverStream <- driverWorker.getStreamBox(context, snagTracker).snagOrStream
                dataStreams <-
                  EitherSeqUtils.sequence(dataWorkers.map(_.getStreamBox(context, snagTracker).snagOrStream))
                metaJoined <- Meta.sequence(driverStream.meta +: dataStreams.map(_.meta))
              } yield {
                val fallback = fallbackGenerator.createFallback()
                val sourceJoined = RecordStreamJoinerWithFallback.joinWithFallback(metaJoined,
                  driverStream.source, dataStreams.map(_.source)) {
                  _.joinWith(_)
                } {
                  fallback
                } {
                  (snag: Snag) => snagTracker.trackSnag(snag)
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
