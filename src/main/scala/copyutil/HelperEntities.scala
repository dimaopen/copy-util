package copyutil

import cats.effect.Sync
import cats.syntax.either._

/**
 *
 * @author Dmitry Openkov
 */
object HelperEntities {
  type ErrorOrOk = Either[String, Unit]

  def toError(msg: String): ErrorOrOk = msg.asLeft

  val Ok: ErrorOrOk = Right(Unit)
  def syncOk[F[_] : Sync]: F[ErrorOrOk] = Sync[F].pure(Ok)

  implicit class ErrorString(val msg: String) extends AnyVal {
    def toError: ErrorOrOk = HelperEntities.toError(msg)
    def toSyncError[F[_] : Sync]: F[ErrorOrOk] = Sync[F].pure(HelperEntities.toError(msg))
  }
}
