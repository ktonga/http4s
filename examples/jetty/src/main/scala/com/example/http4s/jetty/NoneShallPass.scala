package com.example.http4s.jetty

import org.http4s.servlet.DefaultFilter

import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

object NoneShallPass extends DefaultFilter {
  override def doHttpFilter(request: HttpServletRequest, response: HttpServletResponse, chain: FilterChain): Unit =
    response.setStatus(403)
}
