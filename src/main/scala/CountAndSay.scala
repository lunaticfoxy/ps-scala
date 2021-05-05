object CountAndSay {
  def countAndSay(n: Int): String = {
    val dp = collection.mutable.HashMap[Int, String]()

    def getCount(n: String): String = {
      n.size.toString + n.head.toString.toInt
    }

    def groupingRecur(n: String, last: Char, cur: String, g: Array[String]): Array[String] = {
      if(n.isEmpty) {
        g ++ Array(cur)
      } else {
        if(n.head == last)
          groupingRecur(n.drop(1), last, cur + n.head.toString, g)
        else {
          groupingRecur(n.drop(1), n.head, n.head.toString, if(cur == "") g else g ++ Array(cur))
        }
      }
    }

    def countAndSayRecur(n: Int): String = {
      if(n == 1)
        "1"
      else if(dp.contains(n))
        dp(n)
      else {
        val lastN = countAndSayRecur(n - 1)
        val temp = groupingRecur(lastN, 'x', "", Array[String]()).map(getCount).reduce(_ + _)
        dp.addOne((n, temp))
        temp
      }
    }

    countAndSayRecur(n)
  }

  def main(args: Array[String]) = {
    println(countAndSay(1))
    println(countAndSay(2))
    println(countAndSay(3))
    println(countAndSay(4))
  }
}
