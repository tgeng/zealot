package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions
import io.github.tgeng.zealot.common._

enum QualifiedName {
  case Root()
  case Sub(parent: QualifiedName, name: String)

  override def toString: String = this match {
    case Root() => ""
    case Sub(parent, name) => parent.toString + "." + name
  }

  def / (subName: String) : QualifiedName = QualifiedName.Sub(this, subName)
}

val root = QualifiedName.Root()
