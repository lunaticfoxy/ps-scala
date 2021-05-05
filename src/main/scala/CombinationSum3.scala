object CombinationSum3 {

  def combinationSum3(k: Int, n: Int): List[List[Int]] = {

    def combRecur(n: Int, rCnt: Int, rSum: Int, used: List[Int]): List[List[Int]] = {
      if(n >= 10 || rSum < n)
        List[List[Int]]()
      else if (rSum == n) {
        if(rCnt == 1)
          List(used ++ List[Int](n))
        else
          List[List[Int]]()
      } else{
        combRecur(n + 1, rCnt - 1, rSum - n, used ++ List[Int](n)) ++
          combRecur(n + 1, rCnt, rSum, used)
      }
    }

    combRecur(1, k, n, List[Int]())
  }

  def main(args: Array[String]): Unit = {
    println(combinationSum3(2, 18).map(_.mkString(",")).mkString(" / "))
    //println(combinationSum3(3, 9).map(_.mkString(",")).mkString(" / "))
    //println(combinationSum3(4, 1).map(_.mkString(",")).mkString(" / "))
    //println(combinationSum3(3, 2).map(_.mkString(",")).mkString(" / "))
    //println(combinationSum3(9, 45).map(_.mkString(",")).mkString(" / "))
  }
}
