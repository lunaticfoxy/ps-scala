object HappyNumber {
  def isHappy(n: Int): Boolean = {


    def isHappyRecur(n: Int, visited: Set[Int]): Boolean = {
      if(visited.contains(n))
        false
      else {
        if(n == 1 || n == 0)
          true
        else {

          val newN = n.toString.map {x =>
            val d =  x.toString.toInt
            d * d
          }.sum

          isHappyRecur(newN, visited ++ Set[Int](n))
        }
      }
    }

    isHappyRecur(n, Set[Int]())
  }

  def main(args: Array[String]) = {
    println(isHappy(19))
    println(isHappy(2))
  }
}
