import scala.language.implicitConversions
import io.github.tgeng.zealot.tt.core.QualifiedName._
import io.github.tgeng.zealot.tt.core.Builder.{given, _}
import io.github.tgeng.zealot.tt.core._

object Main {

  def main(args: Array[String]): Unit = {
    root/"Nat" |: set(0) where {
      "Z" |: unit
    }
  }
}
