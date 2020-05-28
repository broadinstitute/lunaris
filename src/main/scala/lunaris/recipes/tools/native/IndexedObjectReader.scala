package lunaris.recipes.tools.native

import lunaris.data.BlockGzippedWithIndex
import lunaris.io.query.RecordExtractor
import lunaris.io.{InputId, ResourceConfig}
import lunaris.recipes.eval.LunWorker.{ObjectStreamWorker, RecordStreamWorker}
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue, ObjectStream}
import lunaris.recipes.{eval, tools}
import lunaris.streams.RecordProcessor
import org.broadinstitute.yootilz.core.snag.Snag

object IndexedObjectReader extends tools.Tool {
  override def resultType: LunType = LunType.ObjectStreamType

  object Params {

    object Keys {
      val file = "file"
      val index = "index"
      val idField = "idField"
      val types = "types"
    }

    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = true)
    val index: Tool.ValueParam = Tool.ValueParam(Keys.index, LunType.FileType, isRequired = false)
    val idField: Tool.ValueParam = Tool.ValueParam(Keys.idField, LunType.StringType, isRequired = true)
    val types: Tool.ValueParam = Tool.ValueParam(Keys.types, LunType.MapType(LunType.TypeType), isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index, Params.idField, Params.types)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      file <- ToolArgUtils.asInputId(Params.Keys.file, args)
      index <- ToolArgUtils.asInputIdOr(Params.Keys.index, args, file + ".tbi")
      idField <- ToolArgUtils.asString(Params.Keys.idField, args)
    } yield ToolInstance(file, index, idField, Map.empty)
  }

  case class ToolInstance(file: InputId, index: InputId, idField: String, types: Map[String, LunType])
    extends tools.ToolInstance {
    override def refs: Map[String, String] = Map.empty

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker(file, index, idField, RecordProcessor.ignoreFaultyRecords, context))
  }

  class WorkerMaker(file: InputId,
                    index: InputId,
                    idField: String,
                    recordProcessor: RecordProcessor[LunValue.ObjectValue],
                    context: LunCompileContext) extends eval.WorkerMaker {
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

    val dataWithIndex: BlockGzippedWithIndex = BlockGzippedWithIndex(file, index)

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[ObjectStreamWorker] =
        Some[ObjectStreamWorker]((resourceConfig: ResourceConfig) => {
          RecordExtractor.extractRecords(dataWithIndex, context.regions, RecordProcessor.ignoreFaultyRecords,
            resourceConfig).map(_.map{ headerAndRecordEtor =>
            val objectEtor = headerAndRecordEtor.recordEtor.process(record => recordProcessor(record.toObject(idField)))
            ObjectStream(headerAndRecordEtor.meta, objectEtor)
          })
        })


      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
