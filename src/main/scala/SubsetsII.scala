object SubsetsII {
  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {

    def subsetRecur(nums: Array[Int], left: Int): List[List[Int]] = {
      //println(nums.mkString(","))
      if(nums.length == 0 || left == 0 || nums.length < left)
        List[List[Int]](List[Int]())
      else
        subsetRecur(nums.drop(1), left - 1).map { x =>
          List(nums(0)) ++ x
        } ++ subsetRecur(nums.drop(1), left)
    }

    (0 until nums.length + 1).map{i =>
      subsetRecur(nums, i)
    }.reduce(_ ++ _).map{_.sorted}.distinct
  }

  def main(args: Array[String]): Unit ={
    println(subsetsWithDup(Array(1,2,2)).map(_.mkString(",")).mkString(" / "))
  }
}
