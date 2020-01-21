package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.Map

class GlobalContext {
  private val content: Map[QualifiedName, (Term, Term)] = Map()

  def get(qn: QualifiedName) = content.get(qn)
  def getTerm(qn: QualifiedName) = content.get(qn).map(_._1)
  def getType(qn: QualifiedName) = content.get(qn).map(_._2)

  def update(qn: QualifiedName, t: (Term, Term)) = content(qn) = t
}
