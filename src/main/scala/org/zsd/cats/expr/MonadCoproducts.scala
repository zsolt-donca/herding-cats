package org.zsd.cats.expr

import cats.free.{:<:, Free}
import org.zsd.cats.expr.ExprProblem.:+:

import scala.language.higherKinds

object MonadCoproducts extends App {

  sealed trait ConsoleOp[A]
  case class Read() extends ConsoleOp[String]
  case class Write(s: String) extends ConsoleOp[Unit]

  sealed trait Auth[A]
  case class Login() extends Auth[Unit]
  case class Logout() extends Auth[Unit]

  def read[F[_]](implicit I: (ConsoleOp :<: F)): Free[F, String] = Free.inject[ConsoleOp, F](Read())

  def write[F[_]](s: String)(implicit I: (ConsoleOp :<: F)): Free[F, Unit] = Free.inject[ConsoleOp, F](Write(s))

  def login[F[_]](implicit I: (Auth :<: F)): Free[F, Unit] = Free.inject[Auth, F](Login())

  def logout[F[_]](implicit I: (Auth :<: F)): Free[F, Unit] = Free.inject[Auth, F](Logout())

  type ConsoleOrAuthF[A] = (ConsoleOp :+: Auth)#L[A]

  type ConsoleOrAuth[A] = Free[ConsoleOrAuthF, A]

  def validate(usernane: String, password: String): Boolean = usernane == "dick" && password == "big"

  def program: Free[ConsoleOrAuthF, Unit] = {
    for {
      _ <- write[ConsoleOrAuthF]("Enter your username:")
      username <- read[ConsoleOrAuthF]
      _ <- write[ConsoleOrAuthF]("Enter your password:")
      password <- read[ConsoleOrAuthF]
      _ <- if (validate(username, password)) login[ConsoleOrAuthF] else write[ConsoleOrAuthF]("Get off.")
      message <- read[ConsoleOrAuthF]
      _ <- write[ConsoleOrAuthF]("foo")
      _ <- logout[ConsoleOrAuthF]
    }
      yield ()
  }
}
