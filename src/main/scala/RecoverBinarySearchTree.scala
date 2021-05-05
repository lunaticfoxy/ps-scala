object RecoverBinarySearchTree {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def recoverTree(root: TreeNode): Unit = {
      if(root.left == null) {
        if(root.right != null) {
          if(root.value > root.right.value){
            val temp = root.value
            root.value = root.right.value
            root.right.value = temp
          }

          recoverTree(root.right)
        }
      }
      else{
        if(root.right == null){
          if(root.value < root.left.value){
            val temp = root.value
            root.value = root.left.value
            root.left.value = temp
          }

          recoverTree(root.left)
        }
        else{



        }
      }
    }
  }
}
