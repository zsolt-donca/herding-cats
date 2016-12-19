package org.zsd.cats.free

import cats.data.State
import cats.free.Free
import cats.{Id, _}

object ConsoleIO extends App {

  sealed trait ConsoleOp[A]
  case class Read() extends ConsoleOp[String]
  case class Write(s: String) extends ConsoleOp[Unit]

  type ConsoleOpF[A] = Free[ConsoleOp, A]

  def read: ConsoleOpF[String] = Free.liftF(Read())

  def write(s: String): ConsoleOpF[Unit] = Free.liftF(Write(s))

  //   side-effectful interpreter
  def consoleInterpreter: ConsoleOp ~> Id = {
    new ~>[ConsoleOp, Id] {
      override def apply[A](fa: ConsoleOp[A]): Id[A] = {
        fa match {
          case Read() => io.StdIn.readLine().asInstanceOf[A]
          case Write(s) => println(s); ().asInstanceOf[A]
        }
      }
    }
  }

  //  def consoleInterpreter: ConsoleOp ~> Id = λ[ConsoleOp ~> Id] {
  //    case Read() => io.StdIn.readLine()
  //    case Write(s) => println(s); ()
  //  }

  // a program
  def greet: ConsoleOpF[Unit] = {
    for {
      _ <- write("What is your name?")
      name <- read
      _ <- write(s"Hello, $name!")
    } yield ()
  }

  //  execute our program
  greet.foldMap(consoleInterpreter)

  // test interpreter
  sealed trait ConsoleLine
  case class InputLine(s: String) extends ConsoleLine
  case class OutputLine(s: String) extends ConsoleLine

  case class ConsoleState(feed: Option[List[String]], lines: Vector[ConsoleLine])
  object ConsoleState {
    def apply(inputFeed: List[String]): ConsoleState = ConsoleState(Some(inputFeed), Vector())
  }

  def verifyAndRun: ConsoleOp ~> State[ConsoleState, ?] = {
    new ~>[ConsoleOp, State[ConsoleState, ?]] {
      override def apply[A](fa: ConsoleOp[A]): State[ConsoleState, A] = {
        fa match {
          case Read() => State {
            case ConsoleState(Some(in :: inx), lines) => (ConsoleState(Some(inx), lines :+ InputLine(in)), in.asInstanceOf[A])
            case ConsoleState(_, lines) => (ConsoleState(None, lines), "???".asInstanceOf[A])
          }

          case Write(s) => State {
            case ConsoleState(feed, lines) => (ConsoleState(feed, lines :+ OutputLine(s)), ().asInstanceOf[A])
          }
        }
      }
    }
  }

  //  def verifyAndRun: ConsoleOp ~> State[ConsoleState, ?] = λ[ConsoleOp ~> State[ConsoleState, ?]] {
  //    case Read() => State {
  //      case ConsoleState(Some(in :: inx), lines) => (ConsoleState(Some(inx), lines :+ InputLine(in)), in)
  //      case ConsoleState(_, lines) => (ConsoleState(None, lines), "???")
  //    }
  //
  //    case Write(s) => State {
  //      case ConsoleState(feed, lines) => (ConsoleState(feed, lines :+ OutputLine(s)), ())
  //    }
  //  }

  // execute our program with out test interpreter
  val consoleState = greet.foldMap[State[ConsoleIO.ConsoleState, ?]](verifyAndRun)
  val finalState = consoleState.runS(ConsoleState(List("Zsolt"))).value
  println(finalState)
}
