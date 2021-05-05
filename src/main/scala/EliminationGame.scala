object EliminationGame {
  def lastRemaining(n: Int): Int = {
    def lastRemainingRecur(front: Int, mult: Int, size: Int, max: Int, reverse: Boolean): Int = {
      if (size == 1)
        front
      else {
        val newFront = (
          if (!reverse)
            front + mult
          else {
            if (size % 2 == 0)
              front
            else
              front + mult
          })

        lastRemainingRecur(newFront, mult * 2, size / 2, max, !reverse)
      }
    }

    lastRemainingRecur(1, 1, n, n, false)

  }

  def main(args: Array[String]): Unit = {
    println(lastRemaining(1))
    println(lastRemaining(9))
  }
}
