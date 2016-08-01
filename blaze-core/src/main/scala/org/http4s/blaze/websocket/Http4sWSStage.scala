package org.http4s
package blaze
package websocket

import scalaz.{-\/, \/, \/-}
import scalaz.concurrent._
import scalaz.stream.Process._
import scalaz.stream._

import org.http4s.blaze.pipeline.Command.EOF
import org.http4s.blaze.pipeline.stages.SerializingStage
import org.http4s.blaze.pipeline.{Command, LeafBuilder, TailStage, TrunkBuilder}
import org.http4s.blaze.util.Execution.{directec, trampoline}
import org.http4s.compat._
import org.http4s.websocket.WebsocketBits._
import org.http4s.{websocket => ws4s}

import scala.util.{Failure, Success}

class Http4sWSStage(ws: ws4s.Websocket) extends TailStage[WebSocketFrame] {
  def name: String = "Http4s WebSocket Stage"
  
  private val dead = async.signalOf(false)

  //////////////////////// Source and Sink generators ////////////////////////

  def snk: Sink[Task, WebSocketFrame] = sink.lift { frame =>
    Task.async[Unit] { cb =>
      channelWrite(frame).onComplete {
        case Success(res) => cb(right(res))
        case Failure(Command.EOF) => cb(left(Cause.Terminated(Cause.End)))
        case Failure(t) => cb(left(t))
      }(directec)
    }
  }
  
  def inputstream: Process[Task, WebSocketFrame] = {
    val t = Task.async[WebSocketFrame] { cb =>
      def go(): Unit = channelRead().onComplete {
        case Success(ws) => ws match {
            case Close(_)    =>
              dead.set(true).run
              sendOutboundCommand(Command.Disconnect)
              cb(left(Cause.Terminated(Cause.End)))

            // TODO: do we expect ping frames here?
            case Ping(d)     =>  channelWrite(Pong(d)).onComplete {
              case Success(_)   => go()
              case Failure(EOF) => cb(left(Cause.Terminated(Cause.End)))
              case Failure(t)   => cb(left(t))
            }(trampoline)

            case Pong(_)     => go()
            case f           => cb(right(f))
          }

        case Failure(Command.EOF) => cb(left(Cause.Terminated(Cause.End)))
        case Failure(e)           => cb(left(e))
      }(trampoline)

      go()
    }
    Process.repeatEval(t).onHalt(_.asHalt)
  }

  //////////////////////// Startup and Shutdown ////////////////////////

  override protected def stageStartup(): Unit = {
    super.stageStartup()

    // A latch for shutting down if both streams are closed.
    val count = new java.util.concurrent.atomic.AtomicInteger(2)

    val onFinish: \/[Throwable,Any] => Unit = {
      case \/-(_) =>
        logger.trace("WebSocket finish signaled")
        if (count.decrementAndGet() == 0) {
          logger.trace("Closing WebSocket")
          sendOutboundCommand(Command.Disconnect)
        }
      case -\/(t) =>
        logger.error(t)("WebSocket Exception")
        sendOutboundCommand(Command.Disconnect)
    }
    
    (dead.discrete).wye(ws.read.to(snk))(wye.interrupt).run.runAsync(onFinish)

    // The sink is a bit more complicated
    val discard: Sink[Task, WebSocketFrame] = Process.constant(_ => Task.now(()))

    // if we never expect to get a message, we need to make sure the sink signals closed
    val routeSink: Sink[Task, WebSocketFrame] = ws.write match {
      case Process.Halt(Cause.End) => onFinish(right(())); discard
      case Process.Halt(e)   => onFinish(left(Cause.Terminated(e))); ws.write
      case s => s ++ Process.await(Task{onFinish(right(()))})(_ => discard)
    }
    
    inputstream.to(routeSink).run.runAsync(onFinish)
  }

  override protected def stageShutdown(): Unit = {
    dead.set(true).run
    super.stageShutdown()
  }
}

object Http4sWSStage {
  def bufferingSegment(stage: Http4sWSStage): LeafBuilder[WebSocketFrame] = {
    TrunkBuilder(new SerializingStage[WebSocketFrame]).cap(stage)
  }
}
