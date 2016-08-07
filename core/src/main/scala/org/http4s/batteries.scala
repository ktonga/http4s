package org.http4s

import scala.util.control.NonFatal

private[http4s] trait Batteries extends AnyRef
    with cats.syntax.AllSyntax
    with cats.std.AllInstances
    with fs2.interop.cats.Instances
    with util.CaseInsensitiveStringSyntax
    with util.ChunkInstances
    with util.EitherSyntax
    with util.NonEmptyListSyntax
    with util.StringSyntax
{
  implicit def StreamCatsOps[F[_], A](self: fs2.Stream[F, A]): fs2.interop.cats.StreamCatsOps[F, A] =
    fs2.interop.cats.StreamCatsOps(self)

  implicit class MoreFunctorSyntax[F[_], A](self: F[A])(implicit F: cats.Functor[F]) {
    def widen[B >: A]: F[B] =
      self.asInstanceOf[F[B]] // F.widen(self) in cats-0.7
  }

  def left[A](a: A): Either[A, Nothing] =
    Left(a)

  def right[B](b: B): Either[Nothing, B] =
    Right(b)

  def catchNonFatal[A](f: => A): fs2.util.Attempt[A] =
    try Right(f)
    catch { case NonFatal(t) => Left(t) }
}

/** An all-batteries included import for internal use in http4s.  This
  * is convenient on the master branch and reduces merge conflicts for
  * those maintaining ports to alternative stacks.
  */
private[http4s] object batteries extends Batteries
