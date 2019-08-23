package copyutil

import java.io.File

import cats.effect.Sync
import cats.syntax.flatMap._
import copyutil.HelperEntities._

/**
 *
 * @author Dmitry Openkov
 */
object DirUtil {

  def createPath[F[_] : Sync](dir: File): F[ErrorOrOk] = {
    Sync[F].delay(if (dir.mkdirs()) Ok else s"Cannot create path ${dir.getAbsolutePath}".toError)
  }

  def createPathIfNotExists[F[_] : Sync](dir: File): F[ErrorOrOk] = {
    Sync[F].delay(if (dir.exists()) syncOk else createPath(dir)).flatten
  }

  def isDirectory[F[_] : Sync](file: File): F[Boolean] = Sync[F].delay(file.isDirectory)

  def listFiles[F[_] : Sync](dir: File): F[List[File]] = Sync[F].delay(dir.listFiles().toList)

}
