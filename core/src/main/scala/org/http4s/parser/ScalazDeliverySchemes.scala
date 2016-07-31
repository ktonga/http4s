package org.http4s
package parser

import compat._

import scalaz._

import org.http4s.ParseFailure
import org.parboiled2.{ ErrorFormatter, ParseError }
import org.parboiled2.Parser.DeliveryScheme
import org.parboiled2.support.Unpack
import shapeless.HList

private[http4s] object ScalazDeliverySchemes {
  private val errorFormattter = new ErrorFormatter()

  implicit def Disjunction[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
    new DeliveryScheme[L] {
      type Result = ParseFailure \/ Out
      def success(result: L) = right(unpack(result))
      def parseError(error: ParseError) = left(ParseFailure("", errorFormattter.formatExpectedAsString(error)))
      def failure(error: Throwable) = left(ParseFailure("Exception during parsing.", error.getMessage))
    }
}
