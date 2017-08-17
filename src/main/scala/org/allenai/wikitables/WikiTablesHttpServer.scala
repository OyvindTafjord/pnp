package org.allenai.wikitables

import org.eclipse.jetty.server.{ HttpConfiguration, HttpConnectionFactory, Server, ServerConnector }
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

object WikiTablesHttpServer {

  def main(args: Array[String]): Unit = {

    val server = new Server()

    val httpConfig = new HttpConfiguration()
    httpConfig.setSendServerVersion(false)

    val connector = new ServerConnector(server, new HttpConnectionFactory(httpConfig))
    // TODO: Make port a configuration variable
    connector.setPort(8123)
    server.addConnector(connector)

    val context = new WebAppContext()
    context.setContextPath("/")
    context.setResourceBase("public")
    context.setInitParameter("org.eclipse.jetty.servlet.Default.dirAllowed", "false")
    // Disable caching
    context.setInitParameter("cacheControl", "max-age=0, public")

    context.setInitParameter("org.scalatra.cors.allowedMethods", "GET,POST,PUT,DELETE,HEAD,OPTIONS,PATCH")
    context.setInitParameter("org.scalatra.cors.preflightMaxAge", "1800")
    context.setInitParameter("org.scalatra.cors.enable", "true")

    context.addEventListener(new ScalatraListener)
    server.setHandler(context)

    server.start()
    server.join()
  }
}