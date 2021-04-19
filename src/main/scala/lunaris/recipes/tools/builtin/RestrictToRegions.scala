package lunaris.recipes.tools.builtin

import lunaris.genomics.LociSet
import lunaris.io.{ExonsFileReader, InputId}
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, RunTracker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.utils.SnagUtils
import org.broadinstitute.yootilz.core.snag.Snag

object RestrictToRegions extends Tool {
  override def resultType: LunType.RecordStreamType.type = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
      val regionsFile: String = "regionsFile"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val regionsFile: Tool.ValueParam = Tool.ValueParam(Keys.regionsFile, LunType.FileType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.regionsFile)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      regionsFile <- ToolArgUtils.asInputId(Params.Keys.regionsFile, args)
    } yield ToolInstance(from, regionsFile)
  }

  case class ToolInstance(from: String, regionsFile: InputId) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, regionsFile)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker, regionsFile: InputId)
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] =
        Some[RecordStreamWorker] {
          (context: LunRunContext, runTracker: RunTracker) => {
            val exonsSet: LociSet = SnagUtils.throwIfSnag(ExonsFileReader.read(regionsFile))
            val snagOrStream = fromWorker.getStreamBox(context, runTracker).snagOrStream.map { fromStream =>
              val filteredStream = fromStream.source.filter { record =>
                val recordIsInRegions = exonsSet.overlapsLocus(record.locus)
                if (!recordIsInRegions) {
                  runTracker.snagTracker.trackSnag(Snag(s"${record.id} is outside of specified regions."))
                }
                recordIsInRegions
              }
              RecordStreamWithMeta(fromStream.meta, filteredStream)
            }
            LunWorker.StreamBox(snagOrStream)
          }
        }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
