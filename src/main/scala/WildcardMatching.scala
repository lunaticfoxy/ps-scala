object WildcardMatching {
  def isMatch(s: String, p: String): Boolean = {
    val visited = scala.collection.mutable.Set[(Int, Int)]()

    def isMatchRecur(sIdx: Int, pIdx: Int): Boolean = {
      //println((sIdx, pIdx).toString() + " " + visited.contains((sIdx, pIdx)) )

      if(visited.contains(sIdx, pIdx))
        false
      else {
        visited.add((sIdx, pIdx))

        if (sIdx == s.length) {
          if (pIdx == p.length)
            true
          else if (p(pIdx).toString == "*")
            isMatchRecur(sIdx, pIdx + 1)
          else
            false
        }
        else if (pIdx == p.length)
          false
        else {
          if (p(pIdx).toString == "?")
            isMatchRecur(sIdx + 1, pIdx + 1)
          else if (p(pIdx).toString == "*")
            isMatchRecur(sIdx + 1, pIdx + 1) || isMatchRecur(sIdx + 1, pIdx) || isMatchRecur(sIdx, pIdx + 1)
          else if (p(pIdx) == s(sIdx))
            isMatchRecur(sIdx + 1, pIdx + 1)
          else
            false
        }
      }
    }

    isMatchRecur(0, 0)
  }



  def isMatchStack(s: String, p: String): Boolean = {
    val stack = scala.collection.mutable.Stack[(Int, Int)]()
    var isTrue = false

    val visited = scala.collection.mutable.Set[(Int, Int)]((0, 0))

    stack.push((0, 0))

    while(stack.nonEmpty) {
      //println(stack.take(5))
      val cur = stack.pop()
      val curSIdx = cur._1
      val curPIdx = cur._2

        if(curSIdx == s.length) {
          if(curPIdx == p.length) {
            isTrue = true
            stack.popAll()
          }
          else if(p(curPIdx).toString == "*") {
            if(!visited.contains((curSIdx, curPIdx + 1))) {
              visited.add((curSIdx, curPIdx + 1))
              stack.push((curSIdx, curPIdx + 1))
            }
          }

        }
        else if(curPIdx < p.length) {
          if(p(curPIdx).toString == "?") {
            if(!visited.contains((curSIdx + 1, curPIdx + 1))) {
              visited.add((curSIdx + 1, curPIdx + 1))
              stack.push((curSIdx + 1, curPIdx + 1))
            }
          }
          else if(p(curPIdx).toString == "*") {
            if(!visited.contains((curSIdx + 1, curPIdx + 1))) {
              visited.add((curSIdx + 1, curPIdx + 1))
              stack.push((curSIdx + 1, curPIdx + 1))
            }

            if(!visited.contains((curSIdx + 1, curPIdx))) {
              visited.add((curSIdx + 1, curPIdx))
              stack.push((curSIdx + 1, curPIdx))
            }

            if(!visited.contains((curSIdx, curPIdx + 1))) {
              visited.add((curSIdx, curPIdx + 1))
              stack.push((curSIdx, curPIdx + 1))
            }
          }
          else if(p(curPIdx) == s(curSIdx)) {
            if(!visited.contains((curSIdx + 1, curPIdx + 1))) {
              visited.add((curSIdx + 1, curPIdx + 1))
              stack.push((curSIdx + 1, curPIdx + 1))
            }
          }

        }

    }

    isTrue
  }

  def main(args: Array[String]) = {


    /*
    println("1: " + isMatch("aa", "a"))
    println("1: " + isMatchStack("aa", "a"))
    println("2: " + isMatch("aa", "*"))
    println("2: " + isMatchStack("aa", "*"))
    println("3: " + isMatch("cb", "?a"))
    println("3: " + isMatchStack("cb", "?a"))
    println("4: " + isMatch("adceb", "*a*b"))
    println("4: " + isMatchStack("adceb", "*a*b"))

     */
    println("2: " + isMatch("aa", "*"))
    println("2: " + isMatchStack("aa", "*"))

    println("5: " + isMatch("aa", "a*c?b"))
    println("5: " + isMatchStack("acdcb", "a*c?b"))
    println("6: " + isMatch("", "*****"))
    println("6: " + isMatchStack("", "*****"))


    println("7: " + isMatchStack("babaaababaabababbbbbbaabaabbabababbaababbaaabbbaaab", "***bba**a*bbba**aab**b"))
    println("7: " + isMatch("babaaababaabababbbbbbaabaabbabababbaababbaaabbbaaab", "***bba**a*bbba**aab**b"))


    println("8: " + isMatchStack("aaabbbaabaaaaababaabaaabbabbbbbbbbaabababbabbbaaaaba", "a*******b"))
    println("8: " + isMatch("aaabbbaabaaaaababaabaaabbabbbbbbbbaabababbabbbaaaaba", "a*******b"))

  }
}
