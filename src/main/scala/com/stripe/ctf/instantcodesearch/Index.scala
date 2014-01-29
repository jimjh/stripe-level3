package com.stripe.ctf.instantcodesearch

import java.io._
import scala.collection.mutable;

// assume single-threaded access
// TODO allow parallel indexing
class Index(repoPath: String) extends Serializable {

  val map = new mutable.HashMap[String, mutable.HashSet[(String, Int)]]

  def path() = repoPath

  /**
   * Split text into words, create an index of
   * <pre>
   * {@code
   *   <word>: (<path>, <line-num>)*
   * }
   * </pre>
   */
  def addFile(file: String, text: String) {
    text.split("\n").view.zipWithIndex.foreach { case (line, num) =>
      tokenize(line).map(addToken(file, num + 1, _))
    }
  }

  def addToken(file: String, num: Int, token: String) {
    val set = map getOrElse (token, mutable.HashSet[(String, Int)]())
    map(token) = set + ((file, num))
  }

  def tokenize(line: String) = line.trim().split("[^a-zA-Z]").map(_.toLowerCase)

  def write(out: File) {
    val stream = new FileOutputStream(out)
    write(stream)
    stream.close
  }

  def write(out: OutputStream) {
    val w = new ObjectOutputStream(out)
    w.writeObject(this)
    w.close
  }
}

