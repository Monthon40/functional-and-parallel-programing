class Spaghetti{

  def spaghetti: Stream[String] = {
    def recu2(list: List[Long]): Stream[String] = {
      mix(list, "") #:: recu2(loudReader(list))
    }
    recu2(List(1))
  }

  def ham: Stream[String] = {
    def rerun(num: Int): Stream[String] = {
      Stream.from(helper(num)) #::: rerun(num + 1)
    }
    rerun(1)
  }

  def loudReader(list: List[Long]): List[Long] = {
    def recure(list: List[Long], put: List[Long], generated: Long, scan: Long): List[Long] =
      list match {
        case head :: next if (head == scan) => recure(next, put, generated + 1, scan)
        case head :: next => recure(next, put :+ generated :+ scan, 1, head)
        case Nil          => put :+ generated :+ scan
      }
    recure(list, List(), 0, list.head)
  }

  def flow(xs: String, ys: String): String =
    xs.toList match {
      case head :: next if (head == '0') =>
        flow(next.mkString(""), ys + '1')
      case head :: next if (head == '1') =>
        flow(next.mkString(""), ys + '0')
      case Nil => ys
    }

  def mix(list: List[Long], ys: String): String =
    list match {
      case head :: next => mix(next, ys + head.toString())
      case Nil          => ys
    }

  def helper(ttt: Int): List[String] = {
    if (ttt == 0) {
      List("")
    } else {
      List.concat(helper(ttt - 1).map(x => '0' + x), helper(ttt - 1).map(x => '0' + x).map(b => flow(b, "")))
    }
  }
}
object main{
  def main(args: Array[String]): Unit = {
    val ttt = new Spaghetti
    println(ttt.spaghetti.take(10).toList)
    println(ttt.spaghetti.take(3).toList)
    println(ttt.ham.take(5).toList)
    println(ttt.ham.take(10).toList)

  }
}
