package lunaris.recipes.tools.builtin

import akka.stream.scaladsl.Source
import lunaris.data.BlockGzippedWithIndex
import lunaris.io.query.RecordExtractor
import lunaris.io.{Disposable, InputId}
import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.eval.{LunCompileContext, LunRunContext, LunRunnable, LunWorker, SnagTracker, WorkerMaker}
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.{LunType, LunValue, RecordStreamWithMeta}
import lunaris.recipes.{eval, tools}
import lunaris.streams.transform.RecordProcessor
import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}


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
        Some[RecordStreamWorker](new RecordStreamWorker {

          private def getSnagOrDataDisposable(context: LunRunContext):
          Disposable[Either[Snag, RecordExtractor.HeaderAndRecordEtor]] =
            RecordExtractor.extractRecords(dataWithIndex, compileContext.regions, idField,
              RecordProcessor.printSnagsDropFaultyRecords, context.resourceConfig)

          class RecordEmitter(val context: LunRunContext) {
            private val snagOrRecordEtorDisp =
              getSnagOrDataDisposable(context).map(_.map(_.recordEtor).map { tsvRecordEtor =>
                tsvRecordEtor.process(record => recordProcessor(record.toLunRecord(idField)))
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
              })
            private val snagOrRecordEtor = snagOrRecordEtorDisp.a
            private val recordEtor = snagOrRecordEtor match {
              case Left(snag) =>
                throw new SnagException(snag)
              case Right(recordEtor) => recordEtor
            }

            def nextRecord(): Option[Record] = {
              recordEtor.next() match {
                case Left(snag) =>
                  throw new SnagException(snag)
                case Right(recordOpt) =>
                  recordOpt
              }
            }

            def close(): Unit = snagOrRecordEtorDisp.dispose()
          }

          override def getStreamBox(context: LunRunContext, snagTracker: SnagTracker): LunWorker.StreamBox = {
            val snagOrStream =
              for {
                meta <- getSnagOrDataDisposable(context).useUp(_.map(_.meta))
                source = Source.unfoldResource[Record, RecordEmitter](
                  () => new RecordEmitter(context), _.nextRecord(), _.close()).mapMaterializedValue(_ => meta)
              } yield RecordStreamWithMeta(meta, source)

            LunWorker.StreamBox(snagOrStream)
          }
        })

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }

}
