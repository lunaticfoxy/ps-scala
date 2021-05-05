object BalancedBinaryTree{

   class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
   }

  object Solution {
    def isBalanced(root: TreeNode): Boolean = {
      isBalancedRecur(root, 0)._1
    }

    def isBalancedRecur(root: TreeNode, curDepth: Int): (Boolean, Int) ={
      if(root == null)
        (true, curDepth)
      else {
        val ld = isBalancedRecur(root.left, curDepth + 1)
        val rd = isBalancedRecur(root.right, curDepth + 1)

        (ld._1 && rd._1 && Math.abs(ld._2 - rd._2) <= 1, Math.max(ld._2, rd._2))
      }
    }
  }
}
