package io.github.tgeng.zealot.common

def [T](t: T | Null)!! : T = if (t == null) throw NullPointerException("!! failed") else t

def [T](t: T | Null) |? (default: T) : T =
  if (t == null) default
  else t
