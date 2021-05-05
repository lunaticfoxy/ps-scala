object SimplifyPath {

  def simplifyPath(path: String): String = {

    def recurSimplifyPath(curPath: String, remainedPath: String): String = {
      if(remainedPath.isEmpty)
        return if(curPath.isEmpty) "/" else curPath

      val front = remainedPath.indexOf("/")
      val subStr = if(front < 0) remainedPath else remainedPath.substring(0, front)
      println("---------------")
      println(curPath)
      println(remainedPath)
      println(subStr)

      val newRemainedPath = remainedPath.drop(subStr.length + 1)

      val newCurPath =
        if(subStr.length == 0 || subStr == ".") curPath
        else if(subStr == "..") {
          val curLast = curPath.lastIndexOf("/")

          if(curLast < 0)
            ""
          else
            curPath.substring(0, curLast)
      } else
        curPath + "/" + subStr

      recurSimplifyPath(newCurPath, newRemainedPath)
    }

    recurSimplifyPath("", path)
  }

  def main(args: Array[String]) = {
    //println(simplifyPath("/home/"))
    //println(simplifyPath("/../"))
    //println(simplifyPath("/home//foo/"))
    //println(simplifyPath("/a/./b/../../c/"))
    println(simplifyPath("/a//b////c/d//././/.."))
  }
}
