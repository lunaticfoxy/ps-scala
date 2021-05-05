object MissingNumber {
  def missingNumber(nums: Array[Int]): Int = {
    val sReal = nums.sum
    val sOri = (0 to nums.length).sum

    sOri - sReal
  }

  def main(args: Array[String]): Unit = {
    println(missingNumber(Array(3,0,1)))
    println(missingNumber(Array(0,1)))
    println(missingNumber(Array(9,6,4,2,3,5,7,0,1)))
    println(missingNumber(Array(0)))
  }
}
