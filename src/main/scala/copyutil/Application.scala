package copyutil

import java.io.File

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}

/**
 *
 * @author Dmitry Openkov
 */
object Application extends IOApp {


  def getFileNames(args: List[String]): Either[String, (String, String)] = {
    if (args.length < 2) "Need origin and destination files".asLeft
    else (args(0), args(1)).asRight
  }

  def usage = IO {
    println("usage: program source target")
  }

  def copySourceToTarget(source: String, target: String): IO[ExitCode] = {
    val orig = new File(source)
    val dest = new File(target)
      CopyUtil.checkFilesAndCopy[IO](orig, dest).map {
        case Left(error) =>
          println(error)
          ExitCode.Error
        case Right(_) =>
          println("copied")
          ExitCode.Success
      }
  }

  override def run(args: List[String]): IO[ExitCode] =
    getFileNames(args) match {
      case Left(err) => usage *> IO.pure(ExitCode.Error)
      case Right((source, target)) => copySourceToTarget(source, target)
    }
}
