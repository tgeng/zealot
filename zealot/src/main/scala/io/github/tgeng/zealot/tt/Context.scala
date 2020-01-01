package io.github.tgeng.zealot.tt

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.tt.Neutral._
import io.github.tgeng.zealot.tt.Reference._
import io.github.tgeng.zealot.tt.Redux._
import io.github.tgeng.zealot.tt.Term._
import io.github.tgeng.zealot.tt.Whnf._
import io.github.tgeng.zealot.tt.Value._

type Type = Whnf

class Context() {
  val content: ArrayBuffer[Type] = ArrayBuffer()
  def apply(r: Reference) : Option[Type] = r match {
    case Idx(i) => if (i < 0 || i >= content.size) Option.empty else Option(content(content.size - i - 1))
    case Num(n) => if (n < 0 || n >= content.size) Option.empty else Option(content(n))
  }
  def append(ty: Type) = content.append(ty)
  def dropLast() = content.dropRightInPlace(1)
  def isIdxEqualNum(idx: Int, num: Int) = idx + num + 1 == content.size
  def idxToNum(idx: Int, offset: Int) : Int = content.size + offset - 1 - idx
}

def [T](ctx: Context)::(ty: Type)(action: => T) : T = {
  ctx.append(ty.replaceIdxWithNum(0)(given ctx))
  val result = action
  ctx.dropLast()
  result
}

private def (w: Whnf)replaceIdxWithNum(offset: Int)(given ctx: Context) : Whnf = w match {
  case Neu(n) => Neu(n.replaceIdxWithNum(offset))
  case Whnf.Val(v) => Whnf.Val(v.replaceIdxWithNum(offset))
}

private def (t: Term)replaceIdxWithNum(offset: Int)(given ctx: Context) : Term = t match {
  case Term.Ref(r) => Term.Ref(r.replaceIdxWithNum(offset))
  case Term.Val(v) => Term.Val(v.replaceIdxWithNum(offset))
  case Term.Rdx(r) => Term.Rdx(r.replaceIdxWithNum(offset, _.replaceIdxWithNum(offset)))
}

private def (n: Neutral)replaceIdxWithNum(offset: Int)(given ctx: Context) : Neutral = n match {
  case Neutral.Ref(r) => Neutral.Ref(r.replaceIdxWithNum(offset))
  case Neutral.Rdx(r) => Neutral.Rdx(r.replaceIdxWithNum(offset, _.replaceIdxWithNum(offset)))
}

private def (v: Value)replaceIdxWithNum(offset: Int)(given ctx: Context) : Value = v match {
  case Pi(a, b) => Pi(a.replaceIdxWithNum(offset), b.replaceIdxWithNum(offset + 1))
  case Lam(a) => Lam(a.replaceIdxWithNum(offset + 1))
  case Sig(a, b) => Sig(a.replaceIdxWithNum(offset), b.replaceIdxWithNum(offset + 1))
  case Pair(a, b) => Pair(a.replaceIdxWithNum(offset), b.replaceIdxWithNum(offset))
  case _ => v
}

private def [T](r: Redux[T])replaceIdxWithNum(offset: Int, tConverter : T => T)(given ctx: Context) : Redux[T] = r match {
  case App(a, b) => App(tConverter(a), b.replaceIdxWithNum(offset + 1))
  case Prj1(p) => Prj1(tConverter(p))
  case Prj2(p) => Prj2(tConverter(p))
}

private def (r: Reference)replaceIdxWithNum(offset: Int)(given ctx: Context) : Reference = r match {
  case Idx(i) if (i >= offset) => Num(ctx.idxToNum(i, offset))
  case _ => r
}

type ErrorContext = Seq[TypeCheckOps]
