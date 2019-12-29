package io.github.tgeng.zealot.tt

import scala.collection.mutable.ArrayBuffer

type Type = Whnf

class Context(content: ArrayBuffer[Type]) {

  def apply(i: Int) : Type = content(content.size - i - 1)
  def append(ty: Type) = content.append(ty)
  def dropLast() = content.dropRightInPlace(1)
}

def [T](ctx: Context)::(ty: Type)(action: () => T) : T = {
  ctx.append(ty)
  val result = action()
  ctx.dropLast()
  result
}