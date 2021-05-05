object PerfectSquares {
  def numSquares(n: Int): Int = {
    val dp = collection.mutable.Map[Int, Int]()

    def numSquaresRecur(n: Int): Int = {
      if(n == 0)
        0
      else if(n == 1)
        1
      else if(dp.contains(n))
        dp(n)
      else {
        val maxSqrt = Math.sqrt(n.toDouble).toInt

        val res = (1 to maxSqrt).map{x =>
          numSquaresRecur(n - (x * x))
        }.min + 1

        dp.addOne((n, res))
        res
      }
    }

    numSquaresRecur(n)
  }


  def main(args: Array[String]) = {
    println(numSquares(12))
    println(numSquares(13))
  }
}
