package lunaris.recipes.tools.native

import akka.NotUsed
import akka.stream.scaladsl.Source
import lunaris.io.{Disposable, ResourceConfig}
import lunaris.recipes.eval.LunWorker.ObjectStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue, RecordStream, RecordStreamOld}
import lunaris.recipes.{eval, tools}
import lunaris.streams.JoinedObjectsEitherator
import lunaris.utils.{EitherSeqUtils, SeqBasedOrdering}
import org.broadinstitute.yootilz.core.snag.Snag

object JoinObjects extends tools.Tool {
  override def resultType: LunType = LunType.ObjectStreamType

  object Params {

    object Keys {
      val from: String = "from"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.ArrayType(LunType.ObjectStreamType), isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from)

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
          case Some(fromWorker: ObjectStreamWorker) => Right(fromWorker)
          case Some(_) => Left(Snag(s"Reference $ref for '${Params.Keys.from}' is not the correct type."))
          case None => Left(Snag(s"No value available for reference $ref for ${Params.Keys.from}."))
        }
      }.map(new WorkerMaker(_))
    }
  }

  override def isFinal: Boolean = false

  class WorkerMaker(fromWorkers: Seq[ObjectStreamWorker]) extends eval.WorkerMaker {
    private var nOrdersField: Int = 0

    override def nOrders: Int = nOrdersField

    override def orderAnotherWorker: Either[Snag, WorkerMaker.Receipt] = {
      if (nOrdersField == 0) {
        nOrdersField = 1
        Right(WorkerMaker.Receipt(0))
      } else {
        Left(Snag(s"Multiplication of streams is not supported at this time."))
      }
    }
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerMaker.WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = {
        Some(new ObjectStreamWorker {
          override def getSnagOrStreamDisposable(resourceConfig: ResourceConfig):
          Disposable[Either[Snag, RecordStreamOld]] = {
            val snagOrStreamsDisposables = fromWorkers.map(_.getSnagOrStreamDisposable(resourceConfig))
            Disposable.sequence(snagOrStreamsDisposables).map(EitherSeqUtils.sequence).map(_.flatMap { streams =>
              RecordStream.Meta.sequence(streams.map(_.meta)).map { meta =>
                val etor = new JoinedObjectsEitherator(streams.map(_.objects), SeqBasedOrdering(meta.chroms))
                RecordStreamOld(meta, etor)
              }
            })
          }
        })
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
