package org.http4s.websocket

import scalaz.concurrent.Task
import scalaz.stream._

import org.http4s.websocket.WebsocketBits.WebSocketFrame

private[http4s] final case class Websocket(
  read: Process[Task, WebSocketFrame],
  write: Sink[Task, WebSocketFrame]
)

