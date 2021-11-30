package shared

import scala.annotation.tailrec
import scala.util.Try

object InputGetter {

  private val session = Try(sys.env("SESSIONID")).getOrElse(os.read.lines(os.pwd / ".env").head.split("=").last)

  @tailrec
  def get(day: Int): Seq[String] = {
    val target = os.pwd / "src" / "main" / "resources" / f"day$day%02d.txt"
    if (os.exists(target))
      os.read.lines(target)
    else {
      println("Downloading day " + day)
      os.write(target, requests.get.stream(s"https://adventofcode.com/2021/day/1/input", check = true, cookieValues = Map("session" -> session)))

      get(day)
    }

  }

}
