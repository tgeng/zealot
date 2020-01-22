package io.github.tgeng.zealot.common

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

def [T, R] (elem: T) :: (stack: Buffer[T])(action: => R) : R = {
  stack += elem
  val result = action
  stack.dropRightInPlace(1)
  result
}

def flatten[T, E, R](t: T, eExtractor: T => E, tExtractor: T => Either[R, T]) : (Seq[E], R) = {
  val elems = ArrayBuffer[E]()
  var currentT = t;
  while(true) {
    elems += eExtractor(currentT)
    tExtractor(currentT) match {
      case Right(nextT) => {
        currentT = nextT
      }
      case Left(r) => return (elems.toSeq, r)
    }
  }
  throw AssertionError()
}
