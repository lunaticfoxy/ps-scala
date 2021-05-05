object UniqueBinarySearchTrees2 {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
     var value: Int = _value
     var left: TreeNode = _left
     var right: TreeNode = _right
  }

  def generateTrees(n: Int): List[TreeNode] = {
    genBST(1, n)
  }


  def genBST(start: Int, end: Int): List[TreeNode] = {
    println(start, end)
    if(start > end)
      List[TreeNode](null)
    else{
      (start to end).flatMap{mid =>
        genBST(start, mid - 1).flatMap{left =>
          genBST(mid + 1, end).map{right =>
            println(mid, left, right)
            new TreeNode(mid, left, right)
          }
        }
      }.toList
    }
  }

  def main(args: Array[String]): Unit = {
    generateTrees(3)
  }
}
