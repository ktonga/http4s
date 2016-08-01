package org.http4s

import cats._
import fs2.{Stream => Process, _}, Process._
import fs2.io.file._

import org.http4s.Status.NotModified
import org.http4s.compat._
import org.http4s.headers._
import org.log4s.getLogger
import scodec.bits.ByteVector

import java.io.File
import java.net.URL
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.time.Instant
import java.util.Collections
import java.util.concurrent.ExecutorService

// TODO: consider using the new scalaz.stream.nio.file operations
object StaticFile {
  private[this] val logger = getLogger
  private def left[A](a: A) = Left(a)
  private def right[B](b: B) = Right(b)

  val DefaultBufferSize = 10240

  def fromString(url: String, req: Option[Request] = None)
                (implicit es: ExecutorService = Strategy.DefaultExecutorService): Option[Response] = {
    fromFile(new File(url), req)
  }

  def fromResource(name: String, req: Option[Request] = None)
             (implicit es: ExecutorService = Strategy.DefaultExecutorService): Option[Response] = {
    Option(getClass.getResource(name)).flatMap(fromURL(_, req))
  }

  def fromURL(url: URL, req: Option[Request] = None)
             (implicit es: ExecutorService = Strategy.DefaultExecutorService): Option[Response] = {
    val lastmod = Instant.ofEpochMilli(url.openConnection.getLastModified())
    val expired = req
      .flatMap(_.headers.get(`If-Modified-Since`))
      .map(_.date.compareTo(lastmod) < 0)
      .getOrElse(true)

    if (expired) {
      val mime    = MediaType.forExtension(url.getPath.split('.').last)
      val headers = Headers.apply(
        mime.fold(List[Header](`Last-Modified`(lastmod)))
          (`Content-Type`(_)::`Last-Modified`(lastmod)::Nil))

      Some(Response(
        headers = headers,
        body    = Process.constant(DefaultBufferSize).toSource.through(io.chunkR(url.openStream))
      ))
    } else Some(Response(NotModified))
  }

  def fromFile(f: File, req: Option[Request] = None)(implicit es: ExecutorService = Strategy.DefaultExecutorService): Option[Response] =
    fromFile(f, DefaultBufferSize, req)

  def fromFile(f: File, buffsize: Int, req: Option[Request])
           (implicit es: ExecutorService): Option[Response] = {
    fromFile(f, 0, f.length(), buffsize, req)
  }

  def fromFile(f: File, start: Long, end: Long, buffsize: Int, req: Option[Request])
                        (implicit es: ExecutorService): Option[Response] = {
    if (!f.isFile) return None

    require (start >= 0 && end >= start && buffsize > 0, s"start: $start, end: $end, buffsize: $buffsize")

    val lastModified = Instant.ofEpochMilli(f.lastModified())

    // See if we need to actually resend the file
    val notModified = for {
      r   <- req
      h   <- r.headers.get(`If-Modified-Since`)
      exp  = h.date.compareTo(lastModified) < 0
      _    = logger.trace(s"Expired: ${exp}. Request age: ${h.date}, Modified: $lastModified")
      nm   = Response(NotModified) if (!exp)
    } yield nm

    notModified orElse {

      val (body, contentLength) =
        if (f.length() < end) (EmptyBody, 0)
        else (fileToBody(f, start, end, buffsize), (end - start).toInt)

      val contentType = {
        val name = f.getName()

        name.lastIndexOf('.') match {
          case -1 => None
          case  i => MediaType.forExtension(name.substring(i + 1)).map(`Content-Type`(_))
        }
      }

      val hs = `Last-Modified`(lastModified) ::
               `Content-Length`(contentLength) ::
               contentType.toList

      val r = Response(
        headers = Headers(hs),
        body = body,
        attributes = AttributeMap.empty.put(staticFileKey, f)
      )

      logger.trace(s"Static file generated response: $r")
      Some(r)
  }}

  private def fileToBody(f: File, start: Long, end: Long, buffsize: Int)
                (implicit es: ExecutorService): EntityBody = {
    fs2.io.file.readAll[Task](f.toPath, buffsize)
        .drop(start.toInt) // FIXME this is horrible
        .take(end.toInt)
  }

  private[http4s] val staticFileKey = AttributeKey.http4s[File]("staticFile")
}
