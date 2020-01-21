import scala.language.implicitConversions
import io.github.tgeng.zealot.tt.core.QualifiedName._
import io.github.tgeng.zealot.tt.core.Builder.{given, _}
import io.github.tgeng.zealot.tt.core._

object Main {

  def main(args: Array[String]): Unit = {
    val ctx = GlobalContext()
    println(ctx.get(root/"zealot"/"Set"))
  }
}
