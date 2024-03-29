package lunaris.recipes.tools.builtin

import lunaris.expressions.{FieldFilterExpression, LunBoolExpression}
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, RunTracker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordsFilter extends Tool {
  override def resultType: LunType.RecordStreamType.type = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
      val filter: String = "filter"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val filter: Tool.ValueParam =
      Tool.ValueParam(Keys.filter, LunType.ExpressionType(LunType.BoolType), isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.filter)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      expression <- ToolArgUtils.asExpression(Params.Keys.filter, args)
      booleanExpression <- {
        expression match {
          case booleanExpression: LunBoolExpression => Right(booleanExpression)
          case otherExpression =>
            Left(
              Snag(s"Expected boolean expression, but expression is of type ${otherExpression.returnType.asString}")
            )
        }
      }
    } yield ToolInstance(from, booleanExpression)
  }

  case class ToolInstance(from: String, filter: LunBoolExpression) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, filter)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker, filter: LunBoolExpression)
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] =
        Some[RecordStreamWorker] {
          (context: LunRunContext, runTracker: RunTracker) => {
            val snagOrStream =
              fromWorker.getStreamBox(context, runTracker).snagOrStream.map { fromStream =>
                val filteredSource = fromStream.source.filter { record =>
                  filter.evaluate(record) match {
                    case Right(LunValue.PrimitiveValue.BoolValue(value)) => value
                    case Left(snag) =>
                      if(!snag.tags.contains(FieldFilterExpression.missingFieldSnagTag)) {
                        runTracker.snagTracker.trackSnag(snag)
                      }
                      false
                  }
                }
                val meta = fromStream.meta
                RecordStreamWithMeta(meta, filteredSource)
              }
            LunWorker.StreamBox(snagOrStream)
          }
        }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
