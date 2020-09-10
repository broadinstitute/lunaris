package lunaris.recipes.tools.builtin

import lunaris.io.Disposable
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.RecordStreamMerger
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
    override def refs: Map[String, String] = {
      from.zipWithIndex.collect {
        case (ref, index) => ("from" + index, ref)
      }.toMap
    }

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, WorkerMaker] = {
      EitherSeqUtils.traverse(from) { ref =>
        workers.get(ref) match {
          case Some(fromWorker: RecordStreamWorker) => Right(fromWorker)
          case Some(_) => Left(Snag(s"Reference $ref for '${Params.Keys.from}' is not the correct type."))
          case None => Left(Snag(s"No value available for reference $ref for ${Params.Keys.from}."))
        }
      }.map(new WorkerMaker(_))
    }
  }

  class WorkerMaker(fromWorkers: Seq[RecordStreamWorker])
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {

    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerMaker.WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = {
        Some(new RecordStreamWorker {
          override def getStreamBox(context: LunRunContext): LunWorker.StreamBox = {
            val snagOrStreamsDisposables = fromWorkers.map(_.getStreamBox(context).snagOrStreamDisposable)
            val snagOrStreamDisposable =
              Disposable.sequence(snagOrStreamsDisposables).map(EitherSeqUtils.sequence).map(_.flatMap { streams =>
              RecordStreamWithMeta.Meta.sequence(streams.map(_.meta)).map { meta =>
                val stream = RecordStreamMerger.merge(meta, streams.map(_.source))
                RecordStreamWithMeta(meta, stream)
              }
            })
            LunWorker.StreamBox(snagOrStreamDisposable)
          }
        })
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
