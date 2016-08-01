package org.http4s
package jawn

import scalaz.stream.Process.emit

import _root_.jawn.{AsyncParser, Facade, ParseException}
import jawnstreamz._
import org.http4s.compat._

trait JawnInstances {
  def jawnDecoder[J](implicit facade: Facade[J]): EntityDecoder[J] =
    EntityDecoder.decodeBy(MediaType.`application/json`) { msg =>
      DecodeResult {
        msg.body.parseJson(AsyncParser.SingleValue).partialAttempt {
          case pe: ParseException =>
            emit(MalformedMessageBodyFailure("Invalid JSON", Some(pe)))
        }.runLastOr(left(MalformedMessageBodyFailure("Invalid JSON: empty body")))
      }
    }
}
