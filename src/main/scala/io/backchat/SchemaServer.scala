package io.backchat

import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.Executors
import org.joda.time.format.DateTimeFormat
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http._
import rl.UrlCodingUtils
import org.jboss.netty.handler.codec.frame.TooLongFrameException
import org.joda.time.{DateTime, DateTimeZone}
import javax.activation.MimetypesFileTypeMap
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.stream.{ChunkedWriteHandler, ChunkedFile}
import java.io._
import java.net.InetSocketAddress

object SchemaServer {
  def main(args: Array[String]) {

    val server = new SchemaServer(args.headOption.map(_.toInt) getOrElse 3000, "activitystreams")

    server.start()
    sys.addShutdownHook(
      server.stop()
    )
  }

  val HttpDateGMT = "GMT"
  val HttpDateTimeZone = DateTimeZone.forID(HttpDateGMT)
  val HttpDateFormat = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss zzz").withZone(HttpDateTimeZone)
  val HttpCacheSeconds = 60


  class StaticFileHandler(publicDirectory: String) extends SimpleChannelUpstreamHandler {
    @throws(classOf[Exception])
    override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
      e.getMessage match {
        case request: HttpRequest if request.getMethod != HttpMethod.GET =>
          sendError(ctx, HttpResponseStatus.METHOD_NOT_ALLOWED)
        case request: HttpRequest => {
          val path = sanitizeUri(request.getUri)
          if (path == null) {
            sendError(ctx, HttpResponseStatus.FORBIDDEN)
          } else {
            val file = new File(path)
            if (file.isHidden || !file.exists()) sendError(ctx, HttpResponseStatus.NOT_FOUND)
            else if (!file.isFile) sendError(ctx, HttpResponseStatus.FORBIDDEN)
            else {
              val ifModifiedSince = request.getHeader(HttpHeaders.Names.IF_MODIFIED_SINCE)
              if (ifModifiedSince != null && ifModifiedSince.trim.nonEmpty) {
                val date = HttpDateFormat.parseDateTime(ifModifiedSince)
                val ifModifiedDateSeconds = date.getMillis / 1000
                val fileLastModifiedSeconds = file.lastModified() / 1000
                if (ifModifiedDateSeconds == fileLastModifiedSeconds)
                  sendNotModified(ctx)
              } else {
                try {
                  val raf = new RandomAccessFile(file, "r")
                  val length = raf.length()
                  val resp = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
                  setDateHeader(resp)
                  setCacheHeaders(resp, file)
                  val ch = e.getChannel
                  ch.write(resp)
                  val future = if (ch.getPipeline.get(classOf[SslHandler]) != null) {
                    ch.write(new ChunkedFile(raf, 0, length, 8192))
                  } else { // no ssl, zero-copy is a go
                    val region = new DefaultFileRegion(raf.getChannel, 0, length)
                    val fut = ch.write(region)
                    fut.addListener(new ChannelFutureProgressListener {
                      def operationProgressed(future: ChannelFuture, amount: Long, current: Long, total: Long) {
                        printf("%s: %d / %d (+%d)%n", path, current, total, amount)
                      }

                      def operationComplete(future: ChannelFuture) {
                        region.releaseExternalResources()
                      }
                    })
                    fut
                  }

                  if (!HttpHeaders.isKeepAlive(request)) future.addListener(ChannelFutureListener.CLOSE)
                } catch {
                  case _: FileNotFoundException => sendError(ctx, HttpResponseStatus.NOT_FOUND)
                }
              }
            }
          }
        }
      }
    }

    @throws(classOf[Exception])
    override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
      e.getCause match {
        case ex: TooLongFrameException => sendError(ctx, HttpResponseStatus.BAD_REQUEST)
        case ex =>
          ex.printStackTrace()
          if (e.getChannel.isConnected) sendError(ctx, HttpResponseStatus.INTERNAL_SERVER_ERROR)
      }
    }

    private def sendError(ctx: ChannelHandlerContext, status: HttpResponseStatus) {
      val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status)
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/plain; charset=UTF-8")
      response.setContent(ChannelBuffers.copiedBuffer("Failure: " + status.toString + "\r\n", CharsetUtil.UTF_8))

      // Close the connection as soon as the error message is sent.
      ctx.getChannel.write(response).addListener(ChannelFutureListener.CLOSE)
    }

    private def sanitizeUri(uri: String) = {
      // Decode the path.
      val decoded = try {
          UrlCodingUtils.urlDecode(uri, CharsetUtil.UTF_8)
      } catch  {
        case _: UnsupportedEncodingException =>
          UrlCodingUtils.urlDecode(uri, CharsetUtil.ISO_8859_1)
      }

      // Convert file separators.
      val converted = decoded.replace('/', File.separatorChar)

      val uf = new File(sys.props("user.dir") + File.separatorChar + publicDirectory).getAbsolutePath
      val pth = uf + File.separatorChar + converted
      val f = new File(pth)
      val absPath = f.getAbsolutePath
      if (!(absPath startsWith uf)) null
      else absPath
    }

    private def sendNotModified(ctx: ChannelHandlerContext) {
        val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_MODIFIED)
        setDateHeader(response)

        // Close the connection as soon as the error message is sent.
        ctx.getChannel.write(response).addListener(ChannelFutureListener.CLOSE)
    }

    private def setDateHeader(response: HttpResponse) {
      response.setHeader(HttpHeaders.Names.DATE, (new DateTime).toString(HttpDateFormat))
    }

    private def setCacheHeaders(response: HttpResponse, fileToCache: File) {
      val stream = getClass.getResourceAsStream("/mime.types")
      val reg = new MimetypesFileTypeMap(stream)
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, reg.getContentType(fileToCache))
      response.setHeader(HttpHeaders.Names.EXPIRES, (new DateTime).toString(HttpDateFormat))
      response.setHeader(HttpHeaders.Names.CACHE_CONTROL, "private, max-age=" + HttpCacheSeconds)
      response.setHeader(HttpHeaders.Names.LAST_MODIFIED, new DateTime(fileToCache.lastModified()).toString(HttpDateFormat))
      HttpHeaders.setContentLength(response, fileToCache.length())
    }
  }


}
class SchemaServer(port: Int, publicDirectory: String = ".") {

  import SchemaServer._
  var bootstrap: ServerBootstrap = null

  def start() = synchronized {
    bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(Executors.newCachedThreadPool(), Executors.newCachedThreadPool()))
    bootstrap.setPipelineFactory(new ChannelPipelineFactory {
      def getPipeline: ChannelPipeline = {
        val pipeline = Channels.pipeline()
        pipeline.addLast("decoder", new HttpRequestDecoder())
        pipeline.addLast("aggregator", new HttpChunkAggregator(65536))
        pipeline.addLast("encoder", new HttpResponseEncoder())
        pipeline.addLast("chunkedWriter", new ChunkedWriteHandler())
        pipeline.addLast("handler", new StaticFileHandler(publicDirectory))
        pipeline
      }
    })
    bootstrap.bind(new InetSocketAddress(port))
  }

  def stop() = synchronized {
    val thread = new Thread("server-shutdown-thread") {
      override def run = {
        if (bootstrap != null) {
          bootstrap.releaseExternalResources()
          bootstrap = null
        }
      }
    }
    thread.setDaemon(false)
    thread.start()
  }
}
