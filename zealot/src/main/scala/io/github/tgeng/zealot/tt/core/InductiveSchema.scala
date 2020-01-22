package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.common.OptionSugar._
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.tt.core.QualifiedName._
import io.github.tgeng.zealot.tt.core.Term._
import io.github.tgeng.zealot.tt.core.Value._
import io.github.tgeng.zealot.tt.core.Builder._

// TODO: differentiate parameter from index
case class InductiveTypeSchema(val name: QualifiedName, val targetType: Term) {

  private val _constructorSchemas : ArrayBuffer[InductiveValueSchema] = ArrayBuffer()
  def constructorSchemas: Iterable[InductiveValueSchema] = _constructorSchemas.view

  def apply(conName: String) : InductiveValueSchema = _constructorSchemas.find(_.conName == conName).orThrow(IllegalArgumentException(s"Could not find constructor '$conName'."))

  def typeConstructor(given typeCtx: TypeContext)(given glbCtx: GlobalContext): Either[TypeCheckError, Term] = {
    given errCtx : ErrorContext = Seq.empty
    for {
      targetTypeTy <- targetType.inferType()
      _ <- targetTypeTy.checkSetType()
      (argTypes, bodyType) = flattenPi(targetType)
      _ <- bodyType.whnf.checkSetType()
    } yield argTypes.foldRight(
        Val(TCon(this, (argTypes.size - 1).to(0, -1).map(_.ref)))
      )(
        (_, body) => lam(body)
      )
  }

  def withVCon(name: String, targetType: Term) : InductiveValueSchema = {
    val valueSchema = InductiveValueSchema(this, name, targetType)
    _constructorSchemas += valueSchema
    valueSchema
  }

  override def toString = s"TSchema{$name}"
}

case class InductiveValueSchema(val typeSchema: InductiveTypeSchema, val conName: String, val targetType: Term) {
  val name = typeSchema.name / conName
  def valueConstructor(given typeCtx: TypeContext)(given glbCtx: GlobalContext): Either[TypeCheckError, Term] = {
    given errCtx : ErrorContext = Seq.empty
    for {
      targetTypeTy <- targetType.inferType()
      _ <- targetTypeTy.checkSetType()
      (argTypes, bodyType) = flattenPi(targetType)
      _ <- bodyType.whnf.checkTConType(typeSchema)
    } yield argTypes.foldRight(
        Val(VCon(this, (argTypes.size - 1).to(0, -1).map(_.ref)))
      )(
        (_, body) => lam(body)
      )
  }

  override def toString = s"VSchema{$name}"
}
