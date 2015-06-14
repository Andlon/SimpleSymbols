package simplesymbols
import Parser._

object ConsoleApp extends App {
    println("Starting Simple Symbols...")

//    val env = new Environment(Map("x" -> 3.0, "y" -> 5.0))

    for (line <- io.Source.stdin.getLines()) {
        try {
            val func = parse(line)
            //println("ans = " + func
        } catch {
            case e: Throwable => println("Illegal input: " + e.toString)
        }
    }
}
