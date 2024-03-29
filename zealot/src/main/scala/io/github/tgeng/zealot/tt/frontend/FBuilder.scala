package io.github.tgeng.zealot.tt.frontend

import io.github.tgeng.zealot.tt.core.QualifiedName
import FTerm._
import FReference._
import FValue._
import FRedux._

import scala.language.implicitConversions

object FBuilder {
  def ft(ft: FTerm) = ft

  def (name: String) ref = FRef(FName(name))

  def set(level: Int) = FVal(FSet(level))

  def (a: (String, FTerm)) ->: (b : FTerm) = FVal(FPi(a._1, a._2, b))

  opaque type LambdaArgs = Seq[String]
  def \(names: String*) : LambdaArgs = names
  def (args: LambdaArgs) =>: (body: FTerm) = args.foldRight(body)((name, body) => FVal(FLam(name, body)))

  def (a: FTerm) apply (b: FTerm) = FRdx(FApp(a, b))

  def (a: (String, FTerm)) &: (b: FTerm) = FVal(FSig(a._1, a._2, b))

  given tupleToPairConditional[A, B](given ac: A => FTerm)(given bc: B => FTerm) : Conversion[(A, B), FTerm] = (a, b) => FVal(FPair(ac(a), bc(b)))
  given tupleToPair : Conversion[(FTerm, FTerm), FTerm] = (a, b) => FVal(FPair(a, b))
  given qualifiedNameToGlobal : Conversion[QualifiedName, FTerm] = (qn: QualifiedName) => FTerm.FRdx(FRedux.FGlobal(qn))

  def p1(a: FTerm) : FTerm = FRdx(FPrj1(a))

  def p2(a: FTerm) : FTerm = FRdx(FPrj2(a))

  def unit = FVal(FUnit())

  def star = FVal(FStar())

  given unnamedArg : Conversion[FTerm, (String, FTerm)] = ft => ("", ft)
}
