package org.http4s.server.staticcontent

import scalaz.concurrent.Task

import org.http4s._


/** Cache strategy that doesn't cache anything, ever. */
object NoopCacheStrategy extends CacheStrategy {
  override def cache(uriPath: String, resp: Response): Task[Response] = Task.now(resp)
}
