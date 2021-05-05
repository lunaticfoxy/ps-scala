object NumberOfIslands {
  def numIslands(grid: Array[Array[Char]]): Int = {

    def findIsland(queue: List[(Int, Int)], island: Set[(Int, Int)]): Set[(Int, Int)] = {
      if(queue == null || queue.isEmpty)
        island
      else {
        val cur = queue.head

        val left = (if(cur._2 > 0 && grid(cur._1)(cur._2 - 1) == '1' && !island.contains(cur._1, cur._2 - 1))
          List((cur._1, cur._2 - 1))
        else
          List[(Int, Int)]()
          )

        println(left)

        val right = (if(cur._2 < grid(cur._1).length - 1 && grid(cur._1)(cur._2 + 1) == '1' && !island.contains(cur._1, cur._2 + 1))
              List((cur._1, cur._2 + 1))
            else
              List[(Int, Int)]()
          )
        println(right)


        val up = (if(cur._1 > 0 && grid(cur._1 - 1)(cur._2) == '1' && !island.contains(cur._1 - 1, cur._2))
          List((cur._1 - 1, cur._2))
        else
          List[(Int, Int)]()
          )

        val down = (if(cur._1 < grid.length - 1 && grid(cur._1 + 1)(cur._2) == '1' && !island.contains(cur._1 + 1, cur._2))
          List((cur._1 + 1, cur._2))
        else
          List[(Int, Int)]()
          )
        println(down)

        findIsland(queue.drop(1) ++ left ++ right ++ up ++ down, island ++ left.toSet ++ right.toSet ++ up.toSet ++ down.toSet)
      }
    }


    val visited = collection.mutable.HashSet[(Int, Int)]()

    grid.indices.map{ i =>
      grid(i).indices.map{ j =>
        if(grid(i)(j)=='1' && !visited.contains((i, j))) {
          visited ++= findIsland(List((i, j)), Set((i, j)))
          1
        }
        else
          0
      }.sum
    }.sum
  }


  def main(args: Array[String]): Unit = {
    val grid = Array(Array('1','1','1','1','0'),Array('1','1','0','1','0'),Array('1','1','0','0','0'),Array('0','0','0','0','0'))
    println(numIslands(grid))
  }
}
