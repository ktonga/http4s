package com.example.http4s.tomcat

import org.http4s.servlet.DefaultFilter

import javax.servlet._
import javax.servlet.http._

object NoneShallPass extends DefaultFilter {
  override def doHttpFilter(request: HttpServletRequest, response: HttpServletResponse, chain: FilterChain): Unit =
    response.setStatus(403)
}
