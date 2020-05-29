package lunaris.recipes.tools.native

import java.io.PrintWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets

import lunaris.io.{Disposable, OutputId}
import lunaris.recipes.eval.LunWorker.ObjectStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.LunType.ObjectType
import lunaris.recipes.values.LunValue.ObjectValue
import lunaris.recipes.values.{LunType, LunValue, LunValueJson, ObjectStream}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object TSVWriter extends Tool {
  override def resultType: LunType.UnitType.type = LunType.UnitType

  object Params {

    object Keys {
      val from = "from"
      val file = "file"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.ObjectStreamType, isRequired = true)
    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = false)
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

      private def headerLine(objectType: ObjectType): String = "#" + objectType.fields.mkString("\t")

      private def valueToString(value: LunValue): String = {
        value match {
          case primitiveValue: LunValue.PrimitiveValue => primitiveValue.value.toString
          case _ => LunValueJson.valueEncoder.apply(value).noSpaces
        }
      }

      private def dataLine(objectValue: ObjectValue): String = {
        objectValue.lunType.fields.map(objectValue.values.get).map(_.map(valueToString))
          .map(_.getOrElse("")).mkString("\t")
      }

      private def writeStreamAsTsv(stream: ObjectStream)(writer: String => Unit): Unit = {
        writer(headerLine(stream.meta.objectType))
        stream.objects.foreach(objectValue => writer(dataLine(objectValue)))
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = Some[LunRunnable]((context: LunRunContext) => {
        fromWorker.getSnagOrStreamDisposable(context.resourceConfig).useUp {
          case Left(snag) => context.observer.logSnag(snag)
          case Right(headerAndRecordEtor) =>
            fileOpt match {
              case Some(file) => file.newWriteChannelDisposable(context.resourceConfig).useUp { channel =>
                Disposable.forCloseable(new PrintWriter(Channels.newWriter(channel, StandardCharsets.UTF_8))).useUp {
                  writer => writeStreamAsTsv(headerAndRecordEtor)(writer.println)
                }
              }
              case None => writeStreamAsTsv(headerAndRecordEtor)(println)
            }
        }
      })
    }
  }

}
