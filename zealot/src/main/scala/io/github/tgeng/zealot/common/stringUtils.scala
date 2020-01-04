package io.github.tgeng.zealot.common

def (s: String | UncheckedNull) indented(count: Int) =
  if (s == null) null else s.replaceAll("\n", "\n" + (" " * count))
