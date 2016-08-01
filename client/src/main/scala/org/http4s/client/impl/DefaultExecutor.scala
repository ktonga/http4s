package org.http4s.client.impl

import org.http4s.util.threads._

import java.util.concurrent.ExecutorService


private[client] object DefaultExecutor {
  /** create a new default executor */
  def newClientDefaultExecutorService(name: String): ExecutorService =
    newDefaultFixedThreadPool(
      (Runtime.getRuntime.availableProcessors * 1.5).ceil.toInt,
      threadFactory(i => s"http4s-$name-$i"))
}
