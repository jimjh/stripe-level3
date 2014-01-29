package com.stripe.ctf.instantcodesearch

import java.io._
import java.nio.file._
import scala.collection.mutable;

import com.twitter.concurrent.Broker

abstract class SearchResult
case class Match(path: String, line: Int) extends SearchResult
case class Done() extends SearchResult

class Searcher(indexPath : String)  {
  val index : Index = readIndex(indexPath)
  val root = FileSystems.getDefault().getPath(index.path)

  def search(needle : String, b : Broker[SearchResult]) = {
    val regx   = "[^a-zA-Z]"
    val tokens = needle.split(regx).toList
    val founds = tokens.map(index.map getOrElse (_, mutable.HashSet[(String, Int)]()))
    founds match {
      case head::tail => {
        val set = (head /: tail)(_&_)
        set.foreach { case (path, num) => b !! new Match(path, num) }
      }
      case nil =>
    }
    // TODO find lines with these words in the same order?
    // TODO parallel?
    //for (path <- index.files) {
    //  for (m <- tryPath(path, needle)) {
    //    b !! m
    //  }
    //}
    b !! new Done()
  }

  def tryPath(path: String, needle: String) : Iterable[SearchResult] = {
    try {
      val text : String = slurp(root.resolve(path))
      if (text.contains(needle)) {
        var line = 0
        return text.split("\n").zipWithIndex.
          filter { case (l,n) => l.contains(needle) }.
          map { case (l,n) => new Match(path, n+1) }
      }
    } catch {
      case e: IOException => {
        return Nil
      }
    }

    return Nil
  }

  def readIndex(path: String) : Index = {
    new ObjectInputStream(new FileInputStream(new File(path))).readObject.asInstanceOf[Index]
  }
}
