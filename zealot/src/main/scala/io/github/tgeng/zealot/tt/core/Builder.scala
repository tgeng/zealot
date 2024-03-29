package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions

object Builder {
  inline def t(t: Term) = t

  def (index: Int) ref : Term = Term.Ref(Reference.Idx(index))

  // Careful! You should not use this unless you are writing tests.
  def (index: Int) nref : Term = Term.Ref(Reference.Num(index))

  def set(level: Int) = Term.Val(Value.Set(level))

  def (a:(String, Term)) ->: (b: Term) : Term = Term.Val(Value.Pi(a._2, b)(Binder(a._1)))

  def lam(body: Term) : Term = lam("", body)
  def lam(name: String, body: Term) : Term = Term.Val(Value.Lam(body)(Binder(name)))

  def (a: Term) apply (b: Term) = Term.Rdx(Redux.App(a, b))

  def (a: (String, Term)) &: (b: Term) = Term.Val(Value.Sig(a._2, b)(Binder(a._1)))

  given tupleToPairConditional[A, B](given ac: A => Term)(given bc: B => Term) : Conversion[(A, B), Term] = (a, b) => Term.Val(Value.Pair(ac(a), bc(b)))
  given tupleToPair : Conversion[(Term, Term), Term] = (a, b) => Term.Val(Value.Pair(a, b))
  given qualifiedNameToGlobal : Conversion[QualifiedName, Term] = (qn: QualifiedName) => Term.Rdx(Redux.Global(qn))

  def p1(a: Term) = Term.Rdx(Redux.Prj1(a))

  def p2(a: Term) = Term.Rdx(Redux.Prj2(a))

  def unit = Term.Val(Value.Unit)

  def star = Term.Val(Value.Star)

  given unnamedArg : Conversion[Term, (String, Term)] = ft => ("", ft)

  def tcon(schema: InductiveTypeSchema, content: Term*) = Term.Val(Value.TCon(schema, content))

  def vcon(schema: InductiveValueSchema, content: Term*) = Term.Val(Value.VCon(schema, content))

  def (qn: QualifiedName) |: (targetType: Term)(given typeCtx: TypeContext)(given glbCtx: GlobalContext) : InductiveTypeSchema = {
    val typeSchema = InductiveTypeSchema(qn, targetType)
    val target : Term = typeSchema.typeConstructor match {
      case Right(t) => t
      case Left(e) => throw e
    }
    glbCtx(qn) = (target, targetType)
    typeSchema
  }

  def (schema: InductiveTypeSchema) where (action: (given typeSchema: InductiveTypeSchema) => Unit) : InductiveTypeSchema = {
    action(given schema)
    schema
  }

  def (vn: String) |: (targetType: Term)(given typeSchema: InductiveTypeSchema)(given typeCtx: TypeContext)(given glbCtx: GlobalContext) : InductiveValueSchema = {
    val valueSchema = typeSchema.withVCon(vn, targetType)
    val target : Term = valueSchema.valueConstructor match {
      case Right(t) => t
      case Left(e) => throw e
    }
    glbCtx(valueSchema.name) = (target, valueSchema.targetType)
    valueSchema
  }
}
