package copyutil

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.io.StdIn

/**
 *
 * @author Dmitry Openkov
 */
object UserInput {

  def askUser[F[_] : Sync](question: String): F[Boolean] = for {
    _ <- Sync[F].delay(println(question))
    input <- Sync[F].delay(StdIn.readLine())
  } yield input.toLowerCase match {
    case "y" => true
    case _ => false
  }

  def confirm[F[_] : Sync, A](question: String, fn: () => F[A], fy: () => F[A]): F[A] = {
    for {
      confirmed <- UserInput.askUser(question)
      res <- if (confirmed) fy() else fn()
    } yield res
  }

  def confirmOrA[F[_] : Sync, A](question: String, noVal: => A, fy: () => F[A]): F[A] = {
    for {
      confirmed <- UserInput.askUser(question)
      res <- if (confirmed) fy() else Sync[F].pure(noVal)
    } yield res
  }

}
