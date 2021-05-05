object RegularExpressionMatching {

  def isMatch(s: String, p: String): Boolean = {
    println(s + " / " + p)
    if(p.isEmpty) {
        if(s.isEmpty)
          true
        else
          false
    }
    else{
      val curPattern = p(0).toString
      val curStar = if(p.length > 1 && p(1).toString == "*") true else false

      if(s.isEmpty) {
        if(curStar)
          isMatch(s, p.drop(2))
        else
          false
      }
      else {
        val curChar = s(0).toString

        if((curPattern == ".") || (curChar == curPattern)) {
          if(curStar)
            isMatch(s.drop(1), p.drop(2)) || isMatch(s.drop(1), p) || isMatch(s, p.drop(2))
          else
            isMatch(s.drop(1), p.drop(1))
        }
        else {
          if(curStar)
            isMatch(s, p.drop(2))
          else
            false
        }
      }
    }

  }

  /*
  def main(args: Array[String]): Unit ={
    //println(isMatch("ab", ".*"))
    println(isMatch("bbbba", ".*a*a"))
    //println(isMatch("mississippi", "mis*is*p*."))
  }

   */
}