package lunaris.recipes.tools.native

import lunaris.data.BlockGzippedWithIndex
import lunaris.io.query.RecordExtractor
import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import lunaris.io.{Disposable, InputId}
import lunaris.recipes.eval.{LunCompileContext, LunRunnable, WorkerMaker}
import lunaris.recipes.eval
import lunaris.recipes.eval.WorkerMaker.WorkerBox
import lunaris.recipes.tools
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall}
import lunaris.recipes.values.LunType
import lunaris.streams.RecordProcessor
import org.broadinstitute.yootilz.core.snag.Snag

object IndexedDataReader extends tools.Tool {
  override type Worker = Disposable[Either[Snag, HeaderAndRecordEtor]]

  override def stage: Tool.Stage = Tool.Stage.Input

  override def resultType: LunType = LunType.RecordStreamType

  object Params {

    object Keys {
      val file = "file"
      val index = "index"
    }

    val file: Tool.ValueParam = Tool.ValueParam(Keys.file, LunType.FileType, isRequired = true)
    val index: Tool.ValueParam = Tool.ValueParam(Keys.index, LunType.FileType, isRequired = false)
  }

  override def params: Seq[Tool.Param] = Seq(Params.file, Params.index)

  override def hasEffect: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      file <- ToolArgUtils.asInputId(Params.Keys.file, args)
      index <- ToolArgUtils.asInputIdOr(Params.Keys.index, args, file + ".tbi")
    } yield ToolInstance(file, index)
  }

  case class ToolInstance(file: InputId, index: InputId) extends tools.ToolInstance {
    override def refs: Set[String] = Set.empty

    override def newWorkerMaker(context: LunCompileContext,
                                makers: Map[String, eval.WorkerMaker]): Either[Snag, eval.WorkerMaker] =
      Right(new WorkerMaker(file, index, context))
  }

  class WorkerMaker(file: InputId, index: InputId, context: LunCompileContext) extends eval.WorkerMaker {
    override type Tool = IndexedDataReader.type

    private var nOrdersField: Int = 0

    override def orderAnotherWorker: Either[Snag, WorkerMaker.Receipt] = {
      if (nOrdersField == 0) {
        nOrdersField = 1
        Right(WorkerMaker.Receipt(0))
      } else {
        Left(Snag(s"Multiplication of streams is not supported at this time."))
      }
    }

    val dataWithIndex: BlockGzippedWithIndex = BlockGzippedWithIndex(file, index)
    val recordEitheratorDisposable: Worker =
      RecordExtractor.extract(dataWithIndex, context.regions, RecordProcessor.ignoreFaultyRecords)

    override def finalizeAndShip(): WorkerBox = new WorkerBox {
      override def pickupWorker(receipt: WorkerMaker.Receipt):
      Disposable[Either[Snag, HeaderAndRecordEtor]] = recordEitheratorDisposable

      override def pickupWorkerAsRunnable(receipt: WorkerMaker.Receipt): LunRunnable =
        (observer: LunRunnable.Observer) => {
        recordEitheratorDisposable.useUp {
          case Left(snag) => observer.logSnag(snag)
          case Right(headerAndRecordEtor) =>
            headerAndRecordEtor.recordEtor.foreach { record =>
              observer.logPos(record.seq, record.region.start)
            }
        }
      }
    }

    override def nOrders: Int = nOrdersField
  }

}
