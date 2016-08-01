package org.http4s

import cats._
import cats.data._
import fs2._
import fs2.util.Attempt

import java.util.concurrent.{Executors, ThreadFactory}

/** Internal utilities to minimize differences between scalaz and cats versions
  * without affecting user experience. */
private[http4s] object compat
  extends cats.std.AllInstances
  with cats.syntax.AllSyntax
  with cats.data.XorFunctions
{
  implicit class DisjunctionCompanionCompat(self: Xor.type) {
    def fromTryCatchNonFatal[A](a: => A): Throwable Xor A =
      Xor.catchNonFatal(a)
  }

  implicit class EitherTCompat[F[_], A, B](self: XorT[F, A, B]) {
    def run: F[A Xor B] =
      self.value
  }

  implicit class EqualCompat[A](self: Eq[A]) {
    def equalA: Eq[A] =
      Eq.fromUniversalEquals
  }

  implicit class KleisliSyntaxCompat[F[_], A](val self: F[A]) extends AnyVal {
    def liftKleisli[R]: Kleisli[F, R, A] =
      Kleisli[F, R, A](_ => self)
  }

  implicit class KleisliObjectCompat[F[_], A](val self: Kleisli.type) extends AnyVal {
    def kleisli[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] =
      Kleisli.apply(f)
  }

  implicit class MonoidCompat[A](self: Monoid[A]) {
    def isMZero(a: A)(implicit E: Eq[A]): Boolean =
      self.isEmpty(a)
  }

  implicit class MonoidObjectCompat(self: Monoid.type) {
    def instance[A](f: (A, A) => A, z: A): Monoid[A] =
      new Monoid[A] {
        def combine(a1: A, a2: A): A = f(a1, a2)
        def empty: A = z
      }
  }

  implicit class NonEmptyListCompat[A](self: NonEmptyList[A]) {
    def collectFirst[B](f: PartialFunction[A, B]): Option[B] =
      self.unwrap.collectFirst(f)

    def contains(a: A): Boolean =
      self.contains(a)

    def mkString(sep: String): String =
      self.unwrap.mkString(sep)

    def mkString(begin: String, sep: String, end: String): String =
      self.unwrap.mkString(begin, sep, end)
  }

  implicit class OrderCompat[A](self: Order[A]) {
    def contramap[B](f: B => A): Order[B] =
      self.on(f)

    def reverseOrder: Order[A] =
      self.reverse
  }

  implicit class OrderCompanionCompat(self: Order.type) {
    def fromScalaOrdering[A: Ordering]: Order[A] =
      cats.kernel.Order.fromOrdering
  }

  implicit class ProcessCompat[F[_], O](self: Stream[F, O]) {
    def kill: Stream[F, O] =
      self.open.close
  }

  implicit class ProcessCompanionCompat(self: Stream.type) {
    def halt[A]: Stream[Nothing, A] =
      Stream.empty[Nothing, A]
  }

  implicit class Process0Compat[O](self: Stream[Nothing, O]) {
    def toSource: Stream[Task, O] =
      self.covary[Task]
  }

  implicit class ShowCompat[A](self: Show[A]) {
    def shows(a: A): String =
      self.show(a)
  }

  implicit class ShowCompanionCompat(self: Show.type) {
    def showA[A]: Show[A] =
      Show.fromToString

    def shows[A](f: A => String): Show[A] =
      Show.show(f)

    def showFromToString[A]: Show[A] =
      Show.fromToString
  }

  private val DefaultExecutorService = {
    val DefaultDaemonThreadFactory = new ThreadFactory {
      val defaultThreadFactory = Executors.defaultThreadFactory()
      def newThread(r: Runnable) = {
        val t = defaultThreadFactory.newThread(r)
        t.setDaemon(true)
        t
      }
    }
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, DefaultDaemonThreadFactory)
  }
  implicit class StrategyCompanionCompat(self: Strategy.type) {
    val DefaultExecutorService = compat.DefaultExecutorService
  }

  implicit class TaskCompat[A](self: Task[A]) {
    def runAsync(cb: Callback[A]): A =
      self.unsafeRunAsync(cb.compose[Attempt[A]](_.toXor))
  }
}