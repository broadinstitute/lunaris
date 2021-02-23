package lunaris.recipes.tools.builtin

import lunaris.recipes.eval.LunWorker.RecordStreamWorker
import lunaris.recipes.eval._
import lunaris.recipes.tools.{Tool, ToolArgUtils, ToolCall, ToolInstanceUtils}
import lunaris.recipes.values.{LunType, RecordStreamWithMeta, RecordUtils}
import lunaris.recipes.{eval, tools}
import org.broadinstitute.yootilz.core.snag.Snag

object IdCanonicalizer extends tools.Tool {
  override def resultType: LunType.RecordStreamType.type = LunType.RecordStreamType

  object Params {

    object Keys {
      val from: String = "from"
      val refField: String = "refField"
      val altField: String = "altField"
      val idFieldNew: String = "idFieldNew"
    }

    val from: Tool.RefParam = Tool.RefParam(Keys.from, LunType.RecordStreamType, isRequired = true)
    val refField: Tool.ValueParam = Tool.ValueParam(Keys.refField, LunType.StringType, isRequired = true)
    val altField: Tool.ValueParam = Tool.ValueParam(Keys.altField, LunType.StringType, isRequired = true)
    val idFieldNew: Tool.ValueParam = Tool.ValueParam(Keys.idFieldNew, LunType.StringType, isRequired = true)
  }

  override def params: Seq[Tool.Param] = Seq(Params.from, Params.refField, Params.altField, Params.idFieldNew)

  override def isFinal: Boolean = false

  override def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance] = {
    for {
      from <- ToolArgUtils.asRef(Params.Keys.from, args)
      refField <- ToolArgUtils.asString(Params.Keys.refField, args)
      altField <- ToolArgUtils.asString(Params.Keys.altField, args)
      idFieldNew <- ToolArgUtils.asString(Params.Keys.idFieldNew, args)
    } yield ToolInstance(from, refField, altField, idFieldNew)
  }

  case class ToolInstance(from: String, refField: String, altField: String, idFieldNew: String)
    extends tools.ToolInstance {
    override def refs: Map[String, String] = Map(Params.Keys.from -> from)

    override def newWorkerMaker(context: LunCompileContext, workers: Map[String, LunWorker]):
    Either[Snag, eval.WorkerMaker] = {
      ToolInstanceUtils.newWorkerMaker1(Params.Keys.from, workers) { fromWorker =>
        new WorkerMaker(fromWorker, refField, altField, idFieldNew)
      }
    }
  }

  class WorkerMaker(fromWorker: RecordStreamWorker, refField: String, altField: String, idFieldNew: String)
    extends eval.WorkerMaker with eval.WorkerMaker.WithOutput {
    override def finalizeAndShip(): WorkerMaker.WorkerBox = new WorkerMaker.WorkerBox {
      override def pickupWorkerOpt(receipt: WorkerMaker.Receipt): Some[RecordStreamWorker] =
      Some[RecordStreamWorker] {
        (context: LunRunContext, runTracker: RunTracker) => {
          val snagOrStream = {
            fromWorker.getStreamBox(context, runTracker).snagOrStream.map { fromStream =>
              val mappedSource = fromStream.source.mapConcat { record =>
                val snagOrRecord = for {
                  ref <- RecordUtils.getString(record, refField)
                  alt <- RecordUtils.getString(record, altField)
                  chrom = record.locus.chrom
                  pos = record.locus.region.begin
                  idNew = s"$chrom:${pos}_$ref/$alt"
                  recordNew <- record.addAsNewId(idFieldNew, idNew)
                } yield recordNew
                snagOrRecord match {
                  case Left(snag) =>
                    runTracker.snagTracker.trackSnag(snag)
                    Seq()
                  case Right(record) =>
                    Seq(record)
                }
              }
              val metaOld = fromStream.meta
              val meta = metaOld.copy(objectType = metaOld.objectType.addField(idFieldNew, LunType.StringType))
              RecordStreamWithMeta(meta, mappedSource)
            }
          }
          LunWorker.StreamBox(snagOrStream)
        }
      }

      override def pickupRunnableOpt(): Option[LunRunnable] = None
    }
  }
}
