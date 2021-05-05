object ProductOfarrayExceptSelf {
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val filtered = nums.filter(_ != 0)

    val prodAll = if(filtered.isEmpty) 0 else filtered.product

    val zeroCont = nums.count(_ == 0)

    nums.map{x =>
      if(zeroCont >= 2)
        0
      else {
        if(x == 0)
          prodAll
        else if(zeroCont >= 1)
          0
        else
          prodAll / x
      }

    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3, 4)
    println(productExceptSelf(nums).mkString("Array(", ", ", ")"))
    println(productExceptSelf(Array(-1, 1, 0, -3, 3)).mkString("Array(", ", ", ")"))
  }
}
