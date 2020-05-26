package lunaris.recipes.tools.native

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets

import lunaris.io.{Disposable, OutputId}
import lunaris.recipes.eval.LunWorker.{ObjectStreamWorker, RecordStreamWorker}
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue, LunValueJson}
import lunaris.recipes.{eval, tools}
import lunaris.utils.Eitherator
import org.broadinstitute.yootilz.core.snag.Snag

object JSONWriter extends Tool {
  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {

    object Keys {
      val from = "from"
      val file = "file"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.ObjectStreamType, isRequired = true)
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

  case class ToolInstance(from: String,
                          fileOpt: Option[OutputId]) extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] = {
      workers.get(Params.Keys.from) match {
        case Some(fromWorker: ObjectStreamWorker) => Right(new WorkerMaker(fromWorker, fileOpt))
        case Some(_) => Left(Snag(s"Argument for '${Params.Keys.from}' is not the correct type."))
        case None => Left(Snag(s"No argument provided for ${Params.Keys.from}."))
      }
    }
  }

  class WorkerMaker(fromWorker: ObjectStreamWorker,
                    fileOpt: Option[OutputId]) extends eval.WorkerMaker {
    override def nOrders: Int = 0

    override def orderAnotherWorker: Either[Snag, eval.WorkerMaker.Receipt] =
      Left(Snag(s"Tool $name is final and cannot be used as input for other tools."))

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[LunWorker] = None

      private def writeObjects(objectEter: Eitherator[LunValue.ObjectValue],
                               observer: LunRunContext.Observer)(linePrinter: String => Unit): Unit = {
        linePrinter("{")
        var current: Either[Snag, Option[LunValue.ObjectValue]] = objectEter.next()
        var next: Either[Snag, Option[LunValue.ObjectValue]] = objectEter.next()
        var keepGoing: Boolean = true
        while(keepGoing) {
          current match {
            case Left(snag) =>
              observer.logSnag(snag)
              keepGoing = false
            case Right(Some(objectValue)) =>
              val objectJsonString = LunValueJson.toJson(objectValue)
              val idWithJson =
                "  \"" + objectValue.id + "\" : " + objectJsonString.replace("\n", "\n  ")
              val maybeComma = next match {
                case Right(Some(_)) => ","
                case _ => ""
              }
              linePrinter(idWithJson + maybeComma)
            case Right(None) =>
              keepGoing = false
          }
          current = next
          next = objectEter.next()
        }
        linePrinter("}")
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = Some[LunRunnable]((context: LunRunContext) => {
        fromWorker.getSnagOrStreamDisposable(context.resourceConfig).useUp {
          case Left(snag) => context.observer.logSnag(snag)
          case Right(objectEter) =>
            fileOpt match {
              case Some(file) => file.newWriteChannelDisposable(context.resourceConfig).useUp { channel =>
                Disposable.forCloseable(new  PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))).useUp {
                  writer =>
                    writeObjects(objectEter.objects, context.observer)(writer.println)
                }
              }
              case None =>
                writeObjects(objectEter.objects, context.observer)(println)
            }
        }
      })
    }
  }
}
