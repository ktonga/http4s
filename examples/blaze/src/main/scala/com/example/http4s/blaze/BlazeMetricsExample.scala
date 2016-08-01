package com.example.http4s.blaze

/// code_ref: blaze_server_example

import com.codahale.metrics._
import com.codahale.metrics.json.MetricsModule
import com.example.http4s.ExampleService
import com.fasterxml.jackson.databind.ObjectMapper
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.{Router, ServerApp}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware.Metrics

import java.util.concurrent.TimeUnit

object BlazeMetricsExample extends ServerApp {

  val metrics = new MetricRegistry()
  val mapper = new ObjectMapper()
                  .registerModule(new MetricsModule(TimeUnit.SECONDS, TimeUnit.SECONDS, true))

  val metricsService = HttpService {
    case GET -> Root =>
      val writer = mapper.writerWithDefaultPrettyPrinter()
      Ok(writer.writeValueAsString(metrics))
  }

  val srvc = Router(
    "" -> Metrics.meter(metrics, "Sample")(ExampleService.service),
    "/metrics" -> metricsService
  )

  def server(args: List[String]) = BlazeBuilder.bindHttp(8080)
    .mountService(srvc, "/http4s")
    .start
}
/// end_code_ref
