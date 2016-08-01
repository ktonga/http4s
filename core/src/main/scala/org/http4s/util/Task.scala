package org.http4s
package util

import cats.data._
import fs2._

import org.http4s.compat._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

trait TaskFunctions {
  def unsafeTaskToFuture[A](task: Task[A]): Future[A] = {
    val p = Promise[A]()
    task.unsafeRunAsync {
      case Right(a) => p.success(a)
      case Left(t) => p.failure(t)
    }
    p.future
  }

  def futureToTask[A](f: => Future[A])(implicit ec: ExecutionContext): Task[A] = {
    Task.async[A]({ cb =>
      f.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(t) => cb(Left(t))
      }
    })(Strategy.fromExecutionContext(ec))
  }
}

object task extends TaskFunctions
