package io.github.tgeng.zealot.tt

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.tt.Neutral._
import io.github.tgeng.zealot.tt.Reference._
import io.github.tgeng.zealot.tt.Redux._
import io.github.tgeng.zealot.tt.Term._
import io.github.tgeng.zealot.tt.Whnf._
import io.github.tgeng.zealot.tt.Value._

type Type = Whnf

class Context(content: ArrayBuffer[Type] = ArrayBuffer()) {
  def apply(r: Reference) : Option[Type] = r match {
    case Idx(i) => if (i < 0 || i >= content.size) Option.empty else Option(content(content.size - i - 1))
    case Num(n) => Option(content(n)) // number should never be out of bound since it's only used intenrally
  }
  def append(ty: Type) = content.append(ty)
  def dropLast() = content.dropRightInPlace(1)
  def isIdxEqualNum(idx: Int, num: Int) = idx + num + 1 == content.size
  def idxToNum(idx: Int) : Int = content.size - 1 - idx
}

def [T](ctx: Context)::(ty: Type)(action: () => T) : T = {
  ctx.append(ty.replaceIdxWithNum(given ctx))
  val result = action()
  ctx.dropLast()
  result
}

private def (w: Whnf)replaceIdxWithNum(given ctx: Context) : Whnf = w match {
  case Neu(n) => Neu(n.replaceIdxWithNum)
  case Whnf.Val(v) => Whnf.Val(v.replaceIdxWithNum)
}

private def (t: Term)replaceIdxWithNum(given ctx: Context) : Term = t match {
  case Term.Ref(r) => Term.Ref(r.replaceIdxWithNum)
  case Term.Val(v) => Term.Val(v.replaceIdxWithNum)
  case Term.Rdx(r) => Term.Rdx(r.replaceIdxWithNum(_.replaceIdxWithNum))
}

private def (n: Neutral)replaceIdxWithNum(given ctx: Context) : Neutral = n match {
  case Neutral.Ref(r) => Neutral.Ref(r.replaceIdxWithNum)
  case Neutral.Rdx(r) => Neutral.Rdx(r.replaceIdxWithNum(_.replaceIdxWithNum))
}

private def (v: Value)replaceIdxWithNum(given ctx: Context) : Value = v match {
  case Pi(a, b) => Pi(a.replaceIdxWithNum, b.replaceIdxWithNum)
  case Lam(a) => Lam(a.replaceIdxWithNum)
  case Sig(a, b) => Sig(a.replaceIdxWithNum, b.replaceIdxWithNum)
  case Pair(a, b) => Pair(a.replaceIdxWithNum, b.replaceIdxWithNum)
  case _ => v
}

private def [T](r: Redux[T])replaceIdxWithNum(tConverter : T => T)(given ctx: Context) : Redux[T] = r match {
  case App(a, b) => App(tConverter(a), b.replaceIdxWithNum)
  case Prj1(p) => Prj1(tConverter(p))
  case Prj2(p) => Prj2(tConverter(p))
}

private def (r: Reference)replaceIdxWithNum(given ctx: Context) : Reference = r match {
  case Idx(i) => Num(ctx.idxToNum(i))
  case _ => r
}