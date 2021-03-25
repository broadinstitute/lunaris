package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, RunTracker, WorkerMaker}
import lunaris.recipes.{eval, tools}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstance, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.values.RecordStreamWithMeta.Meta
import lunaris.streams.transform.RecordStreamComplementFilter
import lunaris.utils.EitherSeqUtils
import org.broadinstitute.yootilz.core.snag.Snag

object FindRecordsNotInData extends Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {
    object Keys {
      val driver: String = "driver"
      val data: String = "data"
    }
    val driver: Tool.RefParam = Tool.RefParam(Keys.driver, LunType.RecordStreamType, isRequired = true)
    val data: Tool.RefParam = Tool.RefParam(Keys.data, LunType.RecordStreamType, isRequired = true)
  }
  override def params: Seq[Tool.Param] = Seq(Params.driver, Params.data)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      driver <- ToolArgUtils.asRef(Params.Keys.driver, args)
      data <- ToolArgUtils.asRef(Params.Keys.data, args)
    } yield ToolInstance(driver, data)
  }

  case class ToolInstance(driver: String, data: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = {
      Map(Params.Keys.driver -> driver, Params.Keys.data -> data)
    }

    override def newWorkerMaker(context: LunCompileContext, workers: Map[String, LunWorker]):
    Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker2(Params.Keys.driver, Params.Keys.data, workers) {
        (driverWorker, dataWorker) => new WorkerMaker(driverWorker, Seq(dataWorker))
      }
    }
  }

  class WorkerMaker(driverWorker: RecordStreamWorker,
                    dataWorkers: Seq[RecordStreamWorker]) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] = {
        Some((context: LunRunContext, runTracker: RunTracker) => {
          val snagOrStream =
            for {
              driverStream <- driverWorker.getStreamBox(context, runTracker).snagOrStream
              dataStreams <-
                EitherSeqUtils.sequence(dataWorkers.map(_.getStreamBox(context, runTracker).snagOrStream))
              metaJoined <- Meta.sequence(driverStream.meta +: dataStreams.map(_.meta))
            } yield {
              val sourceComplement =
                RecordStreamComplementFilter.diff(metaJoined, driverStream.source, dataStreams.map(_.source))
              RecordStreamWithMeta(metaJoined, sourceComplement)
            }
          LunWorker.StreamBox(snagOrStream)
        })
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
