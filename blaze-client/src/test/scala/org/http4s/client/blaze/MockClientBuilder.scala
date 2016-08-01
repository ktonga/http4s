package org.http4s
package client
package blaze

import scalaz.concurrent.Task

import org.http4s.blaze.pipeline.{HeadStage, LeafBuilder}

import java.nio.ByteBuffer

private object MockClientBuilder {
  def builder(head: => HeadStage[ByteBuffer], tail: => BlazeConnection): ConnectionBuilder[BlazeConnection] = {
    req => Task.delay {
      val t = tail
      LeafBuilder(t).base(head)
      t
    }
  }

  def manager(head: => HeadStage[ByteBuffer], tail: => BlazeConnection): ConnectionManager[BlazeConnection] = {
    ConnectionManager.basic(builder(head, tail))
  }
}
