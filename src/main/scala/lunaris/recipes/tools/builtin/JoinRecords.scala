package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.RecordStreamJoiner
import lunaris.utils.EitherSeqUtils
import org.broadinstitute.yootilz.core.snag.Snag

object JoinRecords extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.ArrayType(LunType.RecordStreamType), isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRefs(Params.Keys.from, args)
    } yield ToolInstance(from)
  }

  case class ToolInstance(from: Seq[String]) extends tools.ToolInstance {

    private val fromKeys: Seq[String] = from.indices.map(Params.Keys.from + _)

    override def refs: Map[String, String] = fromKeys.zip(from).toMap

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMakerSet(fromKeys, workers)(new WorkerMaker(_))
    }
  }

  class WorkerMaker(fromWorkers: Seq[RecordStreamWorker])
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {

    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerMaker.WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = {
        Some(new RecordStreamWorker {
          override def getStreamBox(context: LunRunContext, runTracker: RunTracker): LunWorker.StreamBox = {
            val snagOrStreams =
              EitherSeqUtils.sequence(fromWorkers.map(_.getStreamBox(context, runTracker).snagOrStream))
            val snagOrStream = snagOrStreams.flatMap { streams =>
              RecordStreamWithMeta.Meta.sequence(streams.map(_.meta)).map { meta =>
                val stream =
                  RecordStreamJoiner.join(meta, streams.map(_.source))(_.joinWith(_))(
                    runTracker.snagTracker.trackSnag)
                RecordStreamWithMeta(meta, stream)
              }
            }
            LunWorker.StreamBox(snagOrStream)
          }
        })
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
