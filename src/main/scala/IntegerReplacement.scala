object IntegerReplacement {
  def integerReplacement(n: Int): Int = {
    if(n == 1)
      0
    else {
      if (n % 2 == 0)
        integerReplacement(n / 2) + 1
      else
        Math.min(integerReplacement(n / 2), integerReplacement((n / 2) + 1)) + 2
    }
  }

  def main(args: Array[String]): Unit = {
    println(integerReplacement(8))
    println(integerReplacement(7))
    println(integerReplacement(4))
  }
}