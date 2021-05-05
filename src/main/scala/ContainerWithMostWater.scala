object ContainerWithMostWater {
  def maxAreaRecur(height: Array[Int], maxVal: Int): Int = {
    //println(height.mkString(",") +  " / " + maxVal)
    if(height == null || height.length <= 1)
      maxVal
    else{
      val newMaxVal = Math.max(maxVal, Math.min(height.head, height.last) * (height.length - 1))
      //val nextLeft = height(1)
      //val nextRightIdx = height.length - 2
      //val nextRight = height(nextRightIdx)

      if(height.last > height.head)
        maxAreaRecur(height.drop(1), newMaxVal)
      else
        maxAreaRecur(height.dropRight(1), newMaxVal)
    }
  }

  def maxArea(height: Array[Int]): Int = {
    maxAreaRecur(height, 0)
  }
}
