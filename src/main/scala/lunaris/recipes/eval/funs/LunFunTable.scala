package lunaris.recipes.eval.funs

import org.broadinstitute.yootilz.core.snag.Snag

case class LunFunTable(funs: Map[String, LunFun]) {
  def findFun(name: String): Either[Snag, LunFun] = {
    funs.get(name) match {
      case Some(fun) => Right(fun)
      case None => Left(Snag(s"Unknown function '$name'."))
    }
  }
}

object LunFunTable {
  def fromFuns(funs: Iterable[LunFun]): LunFunTable = LunFunTable(funs.map(fun => (fun.name, fun)).toMap)
  def fromBuiltinFuns: LunFunTable = ???
}

