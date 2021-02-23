package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.MafForVepCalculator
import org.broadinstitute.yootilz.core.snag.Snag

object CalculateMaf extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
    } yield ToolInstance(from)
  }

  case class ToolInstance(from: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] = {
        Some[RecordStreamWorker] {
          (context: LunRunContext, runTracker: RunTracker) => {
            val snagOrStream = fromWorker.getStreamBox(context, runTracker).snagOrStream.map { fromStream =>
              val sourceWithMaf = fromStream.source.map(MafForVepCalculator.addMaf)
              RecordStreamWithMeta(fromStream.meta, sourceWithMaf)
            }
            LunWorker.StreamBox(snagOrStream)
          }
        }
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
