package shared

import scala.annotation.tailrec
import scala.util.Try

object InputGetter {

  private val sessionFromEnvFile: String = os.read.lines(os.pwd / ".env").head.split("=").last
  private val session = Try(sys.env("SESSIONID")).getOrElse(sessionFromEnvFile)

  @tailrec
  def get(day: Int): Seq[String] = {
    val target = os.pwd / "src" / "main" / "resources" / f"day$day%02d.txt"
    if (os.exists(target))
      os.read.lines(target)
    else {
      println("Downloading day " + day)
      try {
        os.write(target, requests.get.stream(s"https://adventofcode.com/2021/day/$day/input", check = true, headers = Map("Cookie" -> s"session=$session")))
      } finally {
        if (os.size(target) == 0) os.remove(target)
      }

      get(day)
    }

  }

}
