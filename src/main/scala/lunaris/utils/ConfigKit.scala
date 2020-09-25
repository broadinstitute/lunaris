package lunaris.utils

import better.files.File
import com.typesafe.config.{Config, ConfigValue, ConfigValueFactory, ConfigValueType}
import lunaris.app.{Lunaris, LunarisMode}
import lunaris.io.InputId
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.{Failure, Success, Try}

trait ConfigKit[B <: ConfigKit[B]] {
  def map(mapper: Config => Config): B

  def withFallback(oConfigKit: B): B = map(_.withFallback(oConfigKit.config))

  def config: Config
}

object ConfigKit {

  trait Field[B <: ConfigKit[B], V] {
    def configKit: B

    def path: String

    def getValueOpt: Either[Snag, Option[ConfigValue]] = {
      val config = configKit.config
      if (config.hasPath(path)) {
        Right(Some(config.getValue(path)))
      } else {
        if (config.hasPathOrNull(path)) {
          Left(Snag(s"Config value at $path is null."))
        } else {
          Right(None)
        }
      }
    }

    def getValue: Either[Snag, ConfigValue] = {
      getValueOpt match {
        case Left(snag) => Left(snag)
        case Right(Some(value)) => Right(value)
        case Right(None) => Left(Snag(s"No config value at $path."))
      }
    }

    def getValueOfTypeOpt(valueType: ConfigValueType): Either[Snag, Option[ConfigValue]] = {
      val snagOrValueOpt = getValueOpt
      snagOrValueOpt match {
        case Right(Some(value)) =>
          if(value.valueType() == valueType) {
            snagOrValueOpt
          } else {
            Left(Snag(s"Wrong type of config value: expected $valueType, but got ${value.valueType()}"))
          }
        case _ => snagOrValueOpt
      }
    }

    def getValueOfType(valueType: ConfigValueType): Either[Snag, ConfigValue] = {
      for {
        value <- getValue
        _ <- {
          if (value.valueType() == valueType) {
            Right(())
          } else {
            Left(Snag(s"Wrong type of config value: expected $valueType, but got ${value.valueType()}"))
          }
        }
      } yield value
    }

    def get: Either[Snag, V]

    def getOpt: Either[Snag, Option[V]]

    def set(value: V): B
  }

  trait PrimitiveValueField[B <: ConfigKit[B], T] extends Field[B, T] {
    def configValueType: ConfigValueType

    def unwrapValue(configValue: ConfigValue): Either[Snag, T]

    def wrapValue(value: T): ConfigValue

    override def get: Either[Snag, T] = {
      getValueOfType(configValueType).flatMap(unwrapValue)
    }

    override def getOpt: Either[Snag, Option[T]] = {
      getValueOfTypeOpt(configValueType) match {
        case Right(Some(value)) =>
          unwrapValue(value) match {
            case Left(snag) => Left(snag)
            case Right(value) => Right(Some(value))
          }
      }
    }

    override def set(value: T): B = {
      configKit.map { config =>
        config.withValue(path, wrapValue(value))
      }
    }
  }

  case class StringField[B <: ConfigKit[B]](configKit: B, path: String) extends PrimitiveValueField[B, String] {
    override def configValueType: ConfigValueType = ConfigValueType.STRING

    override def unwrapValue(configValue: ConfigValue): Either[Snag, String] =
      Right(configValue.unwrapped().asInstanceOf[String])

    override def wrapValue(string: String): ConfigValue = ConfigValueFactory.fromAnyRef(string)
  }

  case class IntField[B <: ConfigKit[B]](configKit: B, path: String) extends PrimitiveValueField[B, Int] {
    override def configValueType: ConfigValueType = ConfigValueType.NUMBER

    override def unwrapValue(configValue: ConfigValue): Either[Snag, Int] =
      Right(configValue.unwrapped().asInstanceOf[Int])

    override def wrapValue(integer: Int): ConfigValue = ConfigValueFactory.fromAnyRef(integer)
  }

  case class FileField[B <: ConfigKit[B]](configKit: B, path: String) extends PrimitiveValueField[B, File] {
    override def configValueType: ConfigValueType = ConfigValueType.STRING

    override def unwrapValue(configValue: ConfigValue): Either[Snag, File] = {
      Try(File(configValue.unwrapped().asInstanceOf[String])) match {
        case Failure(exception) => Left(Snag(exception))
        case Success(file) => Right(file)
      }
    }

    override def wrapValue(file: File): ConfigValue = ConfigValueFactory.fromAnyRef(file.toString())
  }

  case class InputIdField[B <: ConfigKit[B]](configKit: B, path: String) extends PrimitiveValueField[B, InputId] {
    override def configValueType: ConfigValueType = ConfigValueType.STRING

    override def unwrapValue(configValue: ConfigValue): Either[Snag, InputId] = {
      Right(InputId(configValue.unwrapped().asInstanceOf[String]))
    }

    override def wrapValue(inputId: InputId): ConfigValue = ConfigValueFactory.fromAnyRef(inputId.toString())
  }

  case class LunarisModeField[B <: ConfigKit[B]](configKit: B, path: String)
    extends PrimitiveValueField[B, LunarisMode] {
    override def configValueType: ConfigValueType = ConfigValueType.STRING

    override def unwrapValue(configValue: ConfigValue): Either[Snag, LunarisMode] =
      LunarisMode.parse(configValue.unwrapped().asInstanceOf[String])

    override def wrapValue(mode: LunarisMode): ConfigValue = ConfigValueFactory.fromAnyRef(mode.toString)
  }

}
