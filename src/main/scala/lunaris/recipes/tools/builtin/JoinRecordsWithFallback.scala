package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstance}
import lunaris.recipes.values.LunType
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object JoinRecordsWithFallback extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val driver: String = "driver"
      val data: String = "data"
      val fallback: String = "fallback"
    }

    val driver: Tool.RefParam = Tool.RefParam(Keys.driver, LunType.RecordStreamType, isRequired = true)
    val data: Tool.RefParam = Tool.RefParam(Keys.data, LunType.RecordStreamType, isRequired = true)
    val fallback: Tool.RefParam = Tool.RefParam(Keys.fallback, LunType.StringType, isRequired = true)
  }

  sealed trait FallbackGenerator
  object VepFallbackGenerator extends FallbackGenerator

  private def getFallbackGenerator(fallbackString: String): Either[Snag, FallbackGenerator] = {
    fallbackString match {
      case "vep" => Right(VepFallbackGenerator)
      case _ => Left(Snag(s"Unknown fallback '$fallbackString'"))
    }
  }

  override def params: Seq[Tool.Param] = Seq(Params.driver, Params.data, Params.fallback)

  override def isFinal: Boolean = false

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
                                workers: Map[String, LunWorker]): Either[Snag, WorkerMaker] = {
      ???
    }
  }

  class WorkerMaker(driverWorker: RecordStreamWorker,
                    dataWorker: RecordStreamWorker,
                    fallbackGenerator: FallbackGenerator) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = ???
  }
}
