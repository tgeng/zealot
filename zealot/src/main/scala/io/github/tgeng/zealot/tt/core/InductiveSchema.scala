package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.tt.core.QualifiedName._

// TODO: differentiate from parameter vs index
class InductiveTypeSchema(val name: QualifiedName, val targetType: Term, _constructors: Seq[(String, Term)]) {
  val constructors : Seq[InductiveValueSchema] = _constructors.map { case (cName, cTargetType) =>
    InductiveValueSchema(this, cName, cTargetType)
  }
}

class InductiveValueSchema(val typeSchema: InductiveTypeSchema, val name: String, targetType: Term)

def (qn: QualifiedName) |: (targetType: Term) = InductiveSchemaBuilder(qn, targetType)

class InductiveSchemaBuilder(qn: QualifiedName, targetType: Term) {
  {
    assert(targetType.raise(1, 0) == targetType)
  }

  def where(block: (given ctx: BuildingInductiveSchemaContext) => Unit) : InductiveTypeSchema = {
    val ctx = BuildingInductiveSchemaContext(ArrayBuffer[(String, Term)]())
    block(given ctx)
    InductiveTypeSchema(qn, targetType, ctx.content.toSeq)
  }
}

def (n: String) |: (targetType: Term)(given ctx: BuildingInductiveSchemaContext) : Unit = {
  assert(targetType.raise(1, 0) == targetType)
  ctx.content += ((n, targetType))
}

case class BuildingInductiveSchemaContext(val content: ArrayBuffer[(String, Term)])
