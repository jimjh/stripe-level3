package com.stripe.ctf.instantcodesearch

import scala.collection.mutable.Map;

class SuffixTree extends Serializable {

  var root = new Node('^', None)

  def add(s: String) {
    for (i <- s.length to 1 by -1) {
      add(s, s.substring(i-1))
    }
  }

  def add(source: String, suffix: String) {
    add(source, suffix, root)
  }

  def add(source: String, suffix: String, node: Node) {
    val childs = node.children.get(suffix(0))
    val child  = childs match {
      case Some(n) => n
      case None => node.add(new Node(suffix(0)))
    }
    if (1 == suffix.length)
      child.addSrc(source)
    else
      add(source, suffix.substring(1), child)
  }

  def find(needle: String) : List[String] = find(needle, root)

  def find(needle: String, node: Node) : List[String] = {
    val childs = node.children.get(needle(0))
    childs match {
      case Some(n) =>
        if (1 == needle.length)
          flatten(n)
        else
          find(needle.substring(1), n)
      case None => List[String]()
    }
  }

  def flatten(node: Node) : List[String] = flatten(node, List[String]())

  def flatten(node: Node, list: List[String]) : List[String] = {
    val nlist = node.src match {
      case Some(src) => src ++ list
      case None => list
    }
    (nlist /: node.children.values) { (acc, n) => flatten(n, acc) }
  }

  class Node(val value: Char, var src: Option[List[String]]) extends Serializable {
    def this(v: Char) = this(v, None)
    val children = Map[Char, Node]()
    def add(child: Node) = {
      this.children += (child.value -> child)
      child
    }
    def addSrc(s: String) {
      this.src = src match {
        case Some(l) => Some(s::l)
        case None    => Some(List[String](s))
      }
    }
    override def toString() = value + ", " + src
  }

}
