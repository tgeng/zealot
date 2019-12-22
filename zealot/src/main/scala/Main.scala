import scala.language.implicitConversions
import io.github.tgeng.zealot.tt._
import io.github.tgeng.zealot.tt.Builder.{given, _}

object Main {

  def main(args: Array[String]): Unit = {
    println((0 -> 1) eq (0 -> 1))
    val blah = 0 -> 1
    println(blah eq blah)
  }
}
