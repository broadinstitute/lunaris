package lunaris.recipes.tools.native

import lunaris.recipes.eval.LunWorker.ObjectStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.native.JSONWriter.{Params, WorkerMaker}
import lunaris.recipes.{eval, tools}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.LunType
import org.broadinstitute.yootilz.core.snag.Snag

object JoinObjects extends tools.Tool {
  override def resultType: LunType = LunType.ObjectStreamType

  object Params {

    object Keys {
      val from1: String = "from1"
      val from2: String = "from2"
    }

    val from1: Tool.RefParam = Tool.RefParam(Keys.from1, LunType.ObjectStreamType, isRequired = true)
    val from2: Tool.RefParam = Tool.RefParam(Keys.from2, LunType.ObjectStreamType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from1, Params.from2)

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from1 <- ToolArgUtils.asRef(Params.Keys.from1, args)
      from2 <- ToolArgUtils.asRef(Params.Keys.from2, args)
    } yield ToolInstance(from1, from2)
  }

  case class ToolInstance(from1: String, from2: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from1 -> from1, Params.Keys.from2 -> from2)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, WorkerMaker] = {
      workers.get(Params.Keys.from1) match {
        case Some(fromWorker1: ObjectStreamWorker) =>
          workers.get(Params.Keys.from2) match {
            case Some(fromWorker2: ObjectStreamWorker) => Right(new WorkerMaker(fromWorker1, fromWorker2))
            case Some(_) => Left(Snag(s"Argument for '${Params.Keys.from2}' is not the correct type."))
            case None => Left(Snag(s"No argument provided for ${Params.Keys.from2}."))
          }
       case Some(_) => Left(Snag(s"Argument for '${Params.Keys.from1}' is not the correct type."))
        case None => Left(Snag(s"No argument provided for ${Params.Keys.from1}."))
      }
    }
  }

  override def isFinal: Boolean = false

  class WorkerMaker(fromWorker1: ObjectStreamWorker, fromWorker2: ObjectStreamWorker) extends eval.WorkerMaker {
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
        ???
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
