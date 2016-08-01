package org.http4s

import scalaz.Equal

import org.http4s.compat._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}

/**
 * Instances of [[QueryParamDecoder]] and [[QueryParamEncoder]]
 * must satisfy the following properties
 */
object QueryParamCodecLaws {

  def apply[T: Arbitrary: Equal: QueryParamDecoder: QueryParamEncoder] = new Properties("QueryParamCodec") {

    property("decode . encode == successNel") = forAll { value: T =>
      (QueryParamDecoder[T].decode _ compose QueryParamEncoder[T].encode)(value) === value.successNel
    }

  }
}
