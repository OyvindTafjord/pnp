import org.allenai.wikitables.WikiTablesServlet
import org.scalatra._

import java.util.concurrent.{ThreadPoolExecutor, TimeUnit}
import javax.servlet.ServletContext

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class ScalatraBootstrap extends LifeCycle {
  /** Size of the threadpool used for handling requests. Each request is handled by its own thread.
    * This is, in effect, the number of requests that can be handled simultaneiously.
    */
  val POOL_SIZE = 20
  /** If more than POOL_SIZE requests are in flight at a time, the excess requests enter a queue
    * until they can be handled by a thread in the pool.
    */
  val QUEUE_SIZE = 100
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(
    new ThreadPoolExecutor(POOL_SIZE, POOL_SIZE, 60L, TimeUnit.SECONDS,
      new java.util.concurrent.LinkedBlockingQueue[Runnable](QUEUE_SIZE))
  )

  override def init(context: ServletContext) {

    // Mount the servlets at their respective routes
    context.mount(new WikiTablesServlet, "/*")

  }
}
