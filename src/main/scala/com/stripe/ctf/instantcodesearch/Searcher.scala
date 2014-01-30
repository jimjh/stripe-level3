package com.stripe.ctf.instantcodesearch

import java.io._
import java.nio.file._
import scala.collection.mutable;

import com.twitter.concurrent.Broker

abstract class SearchResult
case class Match(path: String, line: Int) extends SearchResult
case class Done() extends SearchResult

class Searcher(indexer: Indexer, indexPath: String, val id: Int)  {
  var index : Index = if (null == indexer) readIndex(indexPath) else indexer.idx
  val root = FileSystems.getDefault().getPath(index.path)

  def search(needle: String, b: Broker[SearchResult]) = {
    val regx   = "[^a-zA-Z]"
    val tokens = needle.split(regx)//.map(_.toLowerCase)
    val founds = tokens.toList.map(find)
    founds match {
      case head::tail => {
        val set = (head /: tail)(_&_)
        set.foreach { case (path, num) => b !! new Match(path, num) }
      }
      case nil =>
    }
    b !! new Done()
  }

  def find(token: String) = {
    val keys = index.tree.find(token)
    System.err.println("[node #" + id + "] found: " + keys)
    val vals = keys.map { key => index.map getOrElse (key, mutable.HashSet[(String, Int)]()) }
    (mutable.HashSet[(String, Int)]() /: vals)(_|_)
  }

  def readIndex(path: String) : Index = {
    new ObjectInputStream(new FileInputStream(new File(path))).readObject.asInstanceOf[Index]
  }
}
