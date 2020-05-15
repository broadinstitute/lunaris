package lunaris.recipes.tools.native

import lunaris.io.OutputId
import lunaris.recipes.eval.{LunCompileContext, LunWorker}
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.LunType
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object TSVWriter extends Tool {
  override def stage: Tool.Stage = Tool.Stage.Output

  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {

    object Keys {
      val from = "from"
      val file = "file"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.StringType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.file)

  override def isFinal: Boolean = true

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, tools.ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      fileOpt <- ToolArgUtils.asOutputIdOpt(Params.Keys.file, args)
    } yield ToolInstance(from, fileOpt)
  }

  case class ToolInstance(from: String, fileOpt: Option[OutputId]) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker)
  }

  class WorkerMaker extends eval.WorkerMaker {
    override type Tool = TSVWriter.type

    private var nOrdersField: Int = 0

    override def nOrders: Int = nOrdersField

    override def orderAnotherWorker: Either[Snag, eval.WorkerMaker.Receipt] = {
      val receipt = eval.WorkerMaker.Receipt(nOrdersField)
      nOrdersField += 1
      Right(receipt)
    }

    override def finalizeAndShip(): WorkerBox = ??? //  TODO
  }

}
