package lunaris.recipes.tools.native

import lunaris.io.{Disposable, ResourceConfig}
import lunaris.recipes.eval.LunWorker.ObjectStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.native.JSONWriter.{Params, WorkerMaker}
import lunaris.recipes.{eval, tools}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.utils.{EitherSeqUtils, Eitherator}
import org.broadinstitute.yootilz.core.snag.Snag

object JoinObjects extends tools.Tool {
  override def resultType: LunType = LunType.ObjectStreamType

  object Params {

    object Keys {
      val from: String = "from"
      val from1: String = "from1"
      val from2: String = "from2"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.ArrayType(LunType.ObjectStreamType), isRequired = true)
    val from1: Tool.RefParam = Tool.RefParam(Keys.from1, LunType.ObjectStreamType, isRequired = true)
    val from2: Tool.RefParam = Tool.RefParam(Keys.from2, LunType.ObjectStreamType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from)

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRefs(Params.Keys.from, args)
      from1 <- ToolArgUtils.asRef(Params.Keys.from1, args)
      from2 <- ToolArgUtils.asRef(Params.Keys.from2, args)
    } yield ToolInstance(from, from1, from2)
  }

  case class ToolInstance(from: Seq[String], from1: String, from2: String) extends tools.ToolInstance {
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
          case None => Left(Snag(s"No value available for reference $ref for ${Params.Keys.from2}."))
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
          Disposable[Either[Snag, Eitherator[LunValue.ObjectValue]]] = {
            fromWorkers.map(_.getSnagOrStreamDisposable(resourceConfig))
            ??? // TODO
          }
        })
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
