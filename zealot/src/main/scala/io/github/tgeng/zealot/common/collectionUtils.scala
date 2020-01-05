package io.github.tgeng.zealot.common

import scala.collection.mutable.Buffer

def [T, R] (elem: T) :: (stack: Buffer[T])(action: => R) : R = {
  stack += elem
  val result = action
  stack.dropRightInPlace(1)
  result
}
