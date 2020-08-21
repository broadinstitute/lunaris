package lunaris.recipes.tools.builtin

import lunaris.data.BlockGzippedWithIndex
import lunaris.io.InputId
import lunaris.io.query.RecordExtractor
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.RecordProcessor
import lunaris.utils.{Eitherator, EitheratorStreamsInterop}
import org.broadinstitute.yootilz.core.snag.Snag

object IndexedRecordReader extends tools.Tool {
  override def resultType: LunType = LunType.RecordStreamType

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
      typesOpt <- ToolArgUtils.asTypesOpt(Params.Keys.types, args)
    } yield ToolInstance(file, index, idField, typesOpt)
  }

  case class ToolInstance(file: InputId, index: InputId, idField: String, typesOpt: Option[Map[String, LunType]])
    extends tools.ToolInstance {
    override def refs: Map[String, String] = Map.empty

    override def newWorkerMaker(context: LunCompileContext,
                                workers: Map[String, LunWorker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker(file, index, idField, typesOpt, RecordProcessor.ignoreFaultyRecords, context))
  }

  class WorkerMaker(file: InputId,
                    index: InputId,
                    idField: String,
                    typesOpt: Option[Map[String, LunType]],
                    recordProcessor: RecordProcessor[LunValue.RecordValue],
                    compileContext: LunCompileContext) extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    val dataWithIndex: BlockGzippedWithIndex = BlockGzippedWithIndex(file, index)

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Option[RecordStreamWorker] =
        Some[RecordStreamWorker]((runContext: LunRunContext) => {
          RecordExtractor.extractRecords(dataWithIndex, compileContext.regions, idField,
            RecordProcessor.printSnagsDropFaultyRecords, runContext.resourceConfig).map(_.map{ headerAndRecordEtor =>
            def objectEtorGenerator(): Eitherator[LunValue.RecordValue] =
              headerAndRecordEtor.recordEtor.process(record => recordProcessor(record.toLunRecord(idField)))
                .map { objectValue =>
                  typesOpt match {
                    case Some(types) =>
                      objectValue.castFieldsTo(types) match {
                        case Left(_) => objectValue
                        case Right(objectValueNew) =>
                          objectValueNew
                      }
                    case None => objectValue
                  }
                }
            val meta = headerAndRecordEtor.meta
            RecordStreamWithMeta(meta, EitheratorStreamsInterop.eitheratorToStream(objectEtorGenerator, meta))
          })
        })


      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
