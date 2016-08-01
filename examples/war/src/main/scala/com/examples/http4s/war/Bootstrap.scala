package com.examples.http4s.war

import com.example.http4s.ExampleService
import org.http4s.servlet.syntax._

import javax.servlet.annotation.WebListener
import javax.servlet.{ServletContextEvent, ServletContextListener}

@WebListener
class Bootstrap extends ServletContextListener {
  override def contextInitialized(sce: ServletContextEvent): Unit = {
    val ctx = sce.getServletContext
    ctx.mountService("example", ExampleService.service, "/*")
  }

  override def contextDestroyed(sce: ServletContextEvent): Unit = {}
}
