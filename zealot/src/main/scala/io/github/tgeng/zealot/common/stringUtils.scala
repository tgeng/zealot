package io.github.tgeng.zealot.common

def (s: String | Null) indented(count: Int) =
  if (s == null) null else s.replaceAll("\n", "\n" + (" " * count))

def (s: String) ifEmpty(default: String) = if (s.isEmpty) default else s
