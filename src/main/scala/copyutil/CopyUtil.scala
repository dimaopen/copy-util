package copyutil

import java.io.File

import cats.effect.{Concurrent, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._
import cats.instances.either._
import cats.syntax.traverse._
import copyutil.DirUtil.{createPathIfNotExists, listFiles}
import copyutil.HelperEntities._
import enumeratum._

/**
 *
 * @author Dmitry Openkov
 */
object CopyUtil {

  sealed trait FileState extends EnumEntry

  object FileState extends Enum[FileState] {
    val values = findValues

    case object Missed extends FileState
    case object MissedPath extends FileState
    case object File extends FileState
    case object Dir extends FileState

  }


  def fileState[F[_] : Sync](file: File): F[FileState] = {
    Sync[F].delay {
      if (file.exists())
        if (file.isDirectory) FileState.Dir else FileState.File
      else if (file.getParentFile.exists()) FileState.Missed else FileState.MissedPath
    }
  }

  def checkFilesAndCopy[F[_] : Concurrent](orig: File, dest: File): F[ErrorOrOk] = {
    for {
      origState <- fileState(orig)
      destState <- fileState(dest)

      f1: Concurrent[F] = implicitly[Concurrent[F]]
      copyFun = if (origState == FileState.Dir)
        copyDir(_: File, _: File)(f1)
      else
        copyFile(_: File, _: File)(f1)

      result <- (origState, destState) match {
        case (FileState.Missed | FileState.MissedPath, _) =>
          "Source is missed".toSyncError
        case (FileState.Dir, FileState.File) =>
          s"Cannot copy a dir to a file (${orig.getAbsolutePath})".toSyncError
        case (_, FileState.MissedPath) =>
          UserInput.confirmOrA(s"No path exists (${dest.getAbsolutePath}). Create? (y/n)",
            "Aborted".toError,
            () => {
              val createPathResult: F[ErrorOrOk] = DirUtil.createPath(dest.getParentFile)
              createPathResult.flatMap {
                case Left(_) => createPathResult
                case Right(_) => copyFun(orig, dest)
              }
            }
          )
        case (FileState.File, FileState.File) =>
          UserInput.confirmOrA(s"File exists (${dest.getAbsolutePath}). Overwrite? (y/n)",
            "Aborted".toError,
            () => copyFun(orig, dest)
          )
        case (_, _) =>
          copyFun(orig, dest)
      }
    } yield result
  }

  def copyFile[F[_] : Concurrent](orig: File, dest: File): F[ErrorOrOk] = {
    if (orig == dest)
      s"Cannot copy a file to itself (${orig.getAbsolutePath})".toSyncError
    else
      CopyFile.copy(orig, dest).map(_ => Ok)
  }

  def copyDir[F[_] : Concurrent](orig: File, dest: File): F[ErrorOrOk] = {
    if (orig == dest)
      s"Cannot copy a directory to itself (${orig.getAbsolutePath})".toSyncError
    else
      copyDirectory(orig, dest)
  }

  def copyDirectory[F[_] : Concurrent](orig: File, dest: File):F[ErrorOrOk] = {
    createPathIfNotExists(dest).flatMap {
      case x@Left(_) => Sync[F].pure(x)
      case Right(_) => listFiles(orig).flatMap(list => copyList(list, dest))
    }
  }

  def copyList[F[_] : Concurrent](list: List[File], dest: File):F[ErrorOrOk] = {
    val results: List[F[ErrorOrOk]] = for {
      file <- list
      destination = new File(dest, file.getName)
    } yield for {
      result <- checkFilesAndCopy(file, destination)
    } yield result
    for {
      listEok <- results.sequence
    } yield for {
      r <- listEok.sequence
    } yield r.headOption.getOrElse(Ok)
  }

}
