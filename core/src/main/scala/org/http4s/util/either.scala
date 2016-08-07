/*
 * Derived from cats
 * https://raw.githubusercontent.com/typelevel/cats/master/core/src/main/scala/cats/data/Xor.scala
 * 
 * Cats Copyright (c) 2015 Erik Osheim.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions: 
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software. 
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. 
 */
package org.http4s
package util

import scala.util.{Failure, Success, Try}

import cats._
import cats.data._
import cats.std.either._
import org.http4s.util.either._

trait EitherSyntax extends EitherSyntax0 {
  implicit class LeftOps[A, B](self: Left[A, B]) {
    def widen[AA >: A, BB]: Left[AA, BB] =
      self.asInstanceOf[Left[AA, BB]]
  }

  implicit class RightOps[A, B](self: Right[A, B]) {
    def widen[AA, BB >: B]: Right[AA, BB] =
      self.asInstanceOf[Right[AA, BB]]
  }
}

trait EitherSyntax0 {
  // TODO Should be a value class.
  implicit class EitherOps[A, B](self: Either[A, B]) {
    def fold[C](fa: A => C, fb: B => C): C = self match {
      case Left(a) => fa(a)
      case Right(b) => fb(b)
    }

    def isLeft: Boolean = fold(_ => true, _ => false)

    def isRight: Boolean = fold(_ => false, _ => true)

    def swap: B Either A = fold(Right.apply, Left.apply)

    def foreach(f: B => Unit): Unit = fold(_ => (), f)

    def getOrElse[BB >: B](default: => BB): BB = fold(_ => default, identity)

    def orElse[C, BB >: B](fallback: => C Either BB): C Either BB = self match {
      case Left(_)      => fallback
      case r @ Right(_) => r.widen[C, BB]
    }

    def recover[BB >: B](pf: PartialFunction[A, BB]): A Either BB = self match {
      case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
      case _                                => self
    }

    def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, AA Either BB]): AA Either BB = self match {
      case Left(a) if pf.isDefinedAt(a) => pf(a)
      case _                                => self
    }

    def valueOr[BB >: B](f: A => BB): BB = fold(f, identity)

    def forall(f: B => Boolean): Boolean = fold(_ => true, f)

    def exists(f: B => Boolean): Boolean = fold(_ => false, f)

    def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): AA Either B =
      fold(_ => self, b => if (f(b)) self else Left(onFailure))

    def toIor: A Ior B = fold(Ior.left, Ior.right)

    def toEither: Either[A, B] = fold(Left(_), Right(_))

    def toOption: Option[B] = fold(_ => None, Some(_))

    def toList: List[B] = fold(_ => Nil, _ :: Nil)

    def toTry(implicit ev: A <:< Throwable): Try[B] = fold(a => Failure(ev(a)), Success(_))

    def toValidated: Validated[A, B] = fold(Validated.Invalid.apply, Validated.Valid.apply)

    /** Returns a [[ValidatedNel]] representation of self disjunction with the `Left` value
      * as a single element on the `Invalid` side of the [[NonEmptyList]]. */
    def toValidatedNel[AA >: A]: ValidatedNel[AA, B] = fold(Validated.invalidNel, Validated.valid)

    def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB]): AA Either BB =
      f(toValidated).toEither

    def to[F[_], BB >: B](implicit F: Alternative[F]): F[BB] =
      fold(_ => F.empty, F.pure)

    def bimap[C, D](fa: A => C, fb: B => D): C Either D = self match {
      case Left(a) => Left(fa(a))
      case Right(b) => Right(fb(b))
    }

    def map[D](f: B => D): A Either D = self match {
      case l @ Left(_) => l.widen[A, D]
      case Right(b)    => Right(f(b))
    }

    def map2Eval[AA >: A, C, Z](fc: Eval[AA Either C])(f: (B, C) => Z): Eval[AA Either Z] =
      self match {
        case l @ Left(_) => Now(l.widen[A, Z])
        case Right(b) => fc.map(_.map(f(b, _)))
      }

    def leftMap[C](f: A => C): C Either B = self match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.widen[C, B]
    }

    def flatMap[AA >: A, D](f: B => AA Either D): AA Either D = self match {
      case l @ Left(_) => l.widen[AA, D]
      case Right(b) => f(b)
    }

    def compare[AA >: A, BB >: B](that: AA Either BB)(implicit AA: Order[AA], BB: Order[BB]): Int = fold(
      a => that.fold(AA.compare(a, _), _ => -1),
      b => that.fold(_ => 1, BB.compare(b, _))
    )

    def partialCompare[AA >: A, BB >: B](that: AA Either BB)(implicit AA: PartialOrder[AA], BB: PartialOrder[BB]): Double = fold(
      a => that.fold(AA.partialCompare(a, _), _ => -1),
      b => that.fold(_ => 1, BB.partialCompare(b, _))
    )

    def ===[AA >: A, BB >: B](that: AA Either BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = fold(
      a => that.fold(AA.eqv(a, _), _ => false),
      b => that.fold(_ => false, BB.eqv(b, _))
    )

    def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[AA Either D] = self match {
      case l @ Left(_) => F.pure(l.widen[AA, D])
      case Right(b) => F.map(f(b))(Right.apply _)
    }

    def foldLeft[C](c: C)(f: (C, B) => C): C = fold(_ => c, f(c, _))

    def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
      fold(_ => lc, b => f(b, lc))

    def merge[AA >: A](implicit ev: B <:< AA): AA = fold(identity, ev.apply)

    /**
      * Combine with another `Xor` value.
      *
      * If self `Xor` is a `Left` then it will be returned as-is.
      * If self `Xor` is a `Right` and `that` `Xor` is a left, then `that` will be
      * returned.
      * If both `Xor`s are `Right`s, then the `Semigroup[BB]` instance will be used
      * to combine both values and return them as a `Right`.
      * Note: If both `Xor`s are `Left`s then their values are not combined. Use
      * `Validated` if you prefer to combine `Left` values.
      *
      * Examples:
      * {{{
      * scala> import cats.data.Xor
      * scala> import cats.implicits._
      * scala> val l1: Xor[String, Int] = Xor.left("error 1")
      * scala> val l2: Xor[String, Int] = Xor.left("error 2")
      * scala> val r3: Xor[String, Int] = Xor.right(3)
      * scala> val r4: Xor[String, Int] = Xor.right(4)
      *
      * scala> l1 combine l2
      * res0: Xor[String, Int] = Left(error 1)
      *
      * scala> l1 combine r3
      * res1: Xor[String, Int] = Left(error 1)
      *
      * scala> r3 combine l1
      * res2: Xor[String, Int] = Left(error 1)
      *
      * scala> r3 combine r4
      * res3: Xor[String, Int] = Right(7)
      * }}}
      */
    final def combine[AA >: A, BB >: B](that: AA Either BB)(implicit BB: Semigroup[BB]): AA Either BB = self match {
      case left @ Left(_) => left
      case Right(b1) => that match {
        case left @ Left(_) => left
        case Right(b2) => Right(BB.combine(b1, b2))
      }
    }

    def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = fold(
      a => s"Left(${AA.show(a)})",
      b => s"Right(${BB.show(b)})"
    )

    def ap[AA >: A, BB >: B, C](that: AA Either (BB => C)): AA Either C = that.flatMap(self.map)
  }
}

object either extends EitherSyntax
