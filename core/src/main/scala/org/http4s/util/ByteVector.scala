package org.http4s
package util

import cats._
import compat._

import scodec.bits.ByteVector

trait ByteVectorInstances {
  // This is defined in sodec, which we don't (yet) depend on.
  implicit val byteVectorMonoidInstance: Monoid[ByteVector] = Monoid.instance(_ ++ _, ByteVector.empty)
}

object byteVector extends ByteVectorInstances
