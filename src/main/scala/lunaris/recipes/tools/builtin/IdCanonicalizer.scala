package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.{eval, tools}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object IdCanonicalizer extends tools.Tool {
  override def resultType: LunType.RecordStreamType.type = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
      val refField: String = "refField"
      val altField: String = "altField"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val refField: Tool.ValueParam = Tool.ValueParam(Keys.refField, LunType.StringType, isRequired = true)
    val altField: Tool.ValueParam = Tool.ValueParam(Keys.altField, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      refField <- ToolArgUtils.asString(Params.Keys.refField, args)
      altField <- ToolArgUtils.asString(Params.Keys.altField, args)
    } yield ToolInstance(from, refField, altField)
  }

  case class ToolInstance(from: String, refField: String, altField: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext, workers: Map[String, LunWorker]):
    Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, refField, altField)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker, refField: String, altField: String)
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerMaker.WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] =
      Some[RecordStreamWorker] {
        (context: LunRunContext) => {
          val snagOrStream =
            fromWorker.getStreamBox(context).snagOrStream.map { fromStream =>
              val mappedSource = fromStream.source.mapConcat { record =>
                val snagOrRecord = for {
                  ref <- RecordUtils.getString(record, refField)
                  alt <- RecordUtils.getString(record, altField)
                } yield ???
                snagOrRecord match {
                  case Left(snag) =>
                    context.observer.logSnag(snag)
                    Seq()
                  case Right(record) =>
                    Seq(record)
                }
              }
            }
          ???
        }
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
