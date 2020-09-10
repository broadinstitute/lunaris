package lunaris.recipes.tools.builtin

import lunaris.io.Disposable
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.LunValue.PrimitiveValue.StringValue
import lunaris.recipes.values.{LunType, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordsSimpleFilter extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val from = "from"
      val field = "field"
      val stringValue = "stringValue"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val field: Tool.ValueParam = Tool.ValueParam(Keys.field, LunType.StringType, isRequired = true)
    val stringValue: Tool.ValueParam = Tool.ValueParam(Keys.stringValue, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.field, Params.stringValue)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      field <- ToolArgUtils.asString(Params.Keys.field, args)
      stringValue <- ToolArgUtils.asString(Params.Keys.stringValue, args)
    } yield ToolInstance(from, field, stringValue)
  }

  case class ToolInstance(from: String, field: String, value: String) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, field, value)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker, field: String, stringValue: String)
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] =
        Some[RecordStreamWorker] {
          new RecordStreamWorker {
            override def getStreamBox(context: LunRunContext): LunWorker.StreamBox = {
              val snagOrStreamDisposable =
                fromWorker.getStreamBox(context).snagOrStreamDisposable.map(_.map { fromStream =>
                  val filteredSource = fromStream.source.filter { record =>
                    record.values.get(field) match {
                      case Some(StringValue(valueAsString)) => valueAsString == stringValue
                      case _ => false
                    }
                  }
                  val meta = fromStream.meta
                  RecordStreamWithMeta(meta, filteredSource)
                })
              LunWorker.StreamBox(snagOrStreamDisposable)
            }
          }
        }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
