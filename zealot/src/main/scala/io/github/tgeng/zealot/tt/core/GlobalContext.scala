package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.Map
import io.github.tgeng.zealot.tt.core.Builder._

private val zealot = root/"zealot"

class GlobalContext {
  private val content: Map[QualifiedName, (Term, Term)] = Map(
    zealot/"Unit" -> (unit, set(0)),
    zealot/"Star" -> (star, unit),
  )

  def get(qn: QualifiedName) : Option[(Term, Term)] = {
    qn match {
      case QualifiedName.Sub(parent, name) if (
          parent == zealot &&
          name.startsWith("Set") &&
          name.slice(3, name.length).forall(Character.isDigit(_)))
        => {
          val numberPart = name.substring(3)
          if (numberPart == null || numberPart.isEmpty) Some(set(0), set(1))
          else {
            val number = numberPart.toInt
            Some(set(number), set(number + 1))
          }
        }
      case _ => content.get(qn)
    }
  }
  def getTerm(qn: QualifiedName) = get(qn).map(_._1)
  def getType(qn: QualifiedName) = get(qn).map(_._2)

  def update(qn: QualifiedName, t: (Term, Term)) = content(qn) = t
}
