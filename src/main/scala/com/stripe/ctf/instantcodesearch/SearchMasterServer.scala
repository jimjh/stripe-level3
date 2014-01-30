package com.stripe.ctf.instantcodesearch

import scala.util.parsing.json.JSON;
import java.io.File;
import com.twitter.util.{Future, Promise}
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.util.CharsetUtil.UTF_8

class SearchMasterServer(port: Int, id: Int) extends AbstractSearchServer(port, id) {
  val NumNodes = 3

  def this(port: Int) { this(port, 0) }

  val clients = (1 to NumNodes)
    .map { id => new SearchServerClient(port + id, id)}
    .toArray

  override def isIndexed() = {
    val responsesF = Future.collect(clients.map {client => client.isIndexed()})
    val successF = responsesF.map {responses => responses.forall { response =>

        (response.getStatus() == HttpResponseStatus.OK
          && response.getContent.toString(UTF_8).contains("true"))
      }
    }
    successF.map {success =>
      if (success) {
        successResponse()
      } else {
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "Nodes are not indexed")
      }
    }.rescue {
      case ex: Exception => Future.value(
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "Nodes are not indexed")
      )
    }
  }

  override def healthcheck() = {
    val responsesF = Future.collect(clients.map {client => client.healthcheck()})
    val successF = responsesF.map {responses => responses.forall { response =>
        response.getStatus() == HttpResponseStatus.OK
      }
    }
    successF.map {success =>
      if (success) {
        successResponse()
      } else {
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "All nodes are not up")
      }
    }.rescue {
      case ex: Exception => Future.value(
        errorResponse(HttpResponseStatus.BAD_GATEWAY, "All nodes are not up")
      )
    }
  }

  // TODO split up the work
  override def index(path: String) = {
    System.err.println("[master] Requesting " + NumNodes + " nodes to index path: " + path)

    val root   = new File(path)
    val paths  = root.list().map { child => path + "/" + child }
    val paired = paths zip (Stream continually clients).flatten

    val responses = Future.collect(paired.map {case (path, client) => client.index(path)})
    responses.map {_ => successResponse()}
  }

  override def query(q: String) = {
    val responsesF = Future.collect(clients.map {client => client.query(q)})
    val resultsF   = responsesF.map {responses => responses.foldRight(List[String]()) { (resp, suffix) =>
        val content = JSON.parseFull(resp.getContent.toString(UTF_8))
        val list    = content match {
          case Some(c:Map[_,_]) => c.asInstanceOf[Map[String, List[String]]] getOrElse ("results", List[String]())
          case _ => List[String]()
        }
        list ++ suffix
    }}
    resultsF.map { results =>
      val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
      val resultString = results
        .map { r => "\"" + r + "\"" }
        .mkString("[", ",\n", "]")
      val content = "{\"success\": true,\n \"results\": " + resultString + "}"
      response.setContent(copiedBuffer(content, UTF_8))
      response
    }
  }
}
