object SymmetricTree {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def compareTwoRecur(node1: TreeNode, node2: TreeNode): Boolean = {
    if(node1 == null){
      if(node2 == null)
        true
      else
        false
    }
    else if(node2 == null)
      false
    else{
      if(node1.value == node2.value)
        compareTwoRecur(node1.left, node2.right) && compareTwoRecur(node1.right, node2.left)
      else
        false
    }
  }

  def isSymmetric(root: TreeNode): Boolean = {
    if(root == null)
      true
    else
      compareTwoRecur(root.left, root.right)
  }

  def main(args: Array[String]) = {

  }
}
