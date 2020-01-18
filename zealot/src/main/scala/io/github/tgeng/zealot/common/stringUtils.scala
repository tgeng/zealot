package io.github.tgeng.zealot.common

import scala.collection.mutable.ArrayBuffer

def (s: String | Null) indented(count: Int) =
  if (s == null) null else s.replaceAll("\n", "\n" + (" " * count))

def (s: String) ifEmpty(default: String) = if (s.isEmpty) default else s

def (s: String) isBlank = s.trim == s

def (s: String) trimIndent : String = {
  val buffer = ArrayBuffer[String]()
  var minIndent = Integer.MAX_VALUE
  (s.split("\n")!!).map{ lineOrNull =>
    val line = (lineOrNull!!)
    buffer.append(line)
    if (line.nonEmpty) {
      minIndent = line.takeWhile(_ == ' ').size
    }
  }
  val sb = StringBuilder()
  for (line <- buffer.dropWhile(_.isBlank).reverse.dropWhile(_.isBlank).reverse) {
    if (line.isBlank) {
      sb.append("")
    } else {
      sb.append(line.substring(minIndent))
    }
    sb.append('\n')
  }
  sb.deleteCharAt(sb.size - 1)
  sb.toString
}
