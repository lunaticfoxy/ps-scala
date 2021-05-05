
object SubstringWithConcatenationOfAllWords {
  def findSubstring(s: String, words: Array[String]): List[Int] = {

    def checkString(s: String, wordMap: Map[String, Int]): Boolean = {
      //println(s + " / " + words.mkString(","))

      if(wordMap == null || wordMap.isEmpty)
        true
      else {
        val foundWord = wordMap.keys.filter(s.startsWith)

        if(foundWord == null || foundWord.isEmpty)
          false
        else {
          val newFoundVal = wordMap(foundWord.head) - 1
          val newWordMap = if(newFoundVal == 0) wordMap - foundWord.head
          else (wordMap - foundWord.head) + (foundWord.head -> newFoundVal)


          checkString(s.drop(foundWord.head.length), newWordMap)
        }
      }
    }


    def findSubStringRecur(s: String, newWordMap: Map[String, Int], idx: Int, indicies: List[Int]): List[Int] = {
      if(s == null || s.isEmpty)
        indicies
      else if(checkString(s, newWordMap))
        findSubStringRecur(s.drop(1), newWordMap, idx + 1, indicies ++ List(idx))
      else
        findSubStringRecur(s.drop(1), newWordMap, idx + 1, indicies)
    }

    val wordMap = words.groupBy(identity).map{case (k: String, v: Array[String]) => (k, v.length)}.toMap
    findSubStringRecur(s, wordMap, 0, List[Int]())

  }
}