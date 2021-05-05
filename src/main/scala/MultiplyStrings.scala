
object MultiplyStrings {
  def multiply(num1: String, num2: String): String = {

    def sumRecur(num1: String, num2: String, pad: Int, last: String): String = {
      if(num1.isEmpty){
        if(num2.isEmpty){
          if(pad > 0)
            pad.toString + last
          else
            last
        }
        else{
          val newVal = num2.last.toString.toInt + pad
          val newPad = (newVal / 10)
          sumRecur(num1, num2.dropRight(1), newPad, (newVal % 10).toString + last)
        }
      }
      else{
        if(num2.isEmpty) {
          val newVal = num1.last.toString.toInt + pad
          val newPad = (newVal / 10)
          sumRecur(num1.dropRight(1), num2, newPad, (newVal % 10).toString + last)
        }
        else {
          val newVal = num1.last.toString.toInt + num2.last.toString.toInt + pad
          val newPad = (newVal / 10)
          sumRecur(num1.dropRight(1), num2.dropRight(1), newPad, (newVal % 10).toString + last)
        }
      }
    }

    def multRecur(numStr: String, numInt: Int, pad: Int, last: String): String = {
      if(numStr.isEmpty){
        if(pad == 0)
          last
        else
          pad.toString + last
      }
      else{
        val newVal = numStr.last.toString.toInt * numInt + pad
        val newPad = newVal / 10

        multRecur(numStr.dropRight(1), numInt, newPad, (newVal % 10).toString + last)
      }
    }

    def removeZero(str: String): String = {
      if(str.isEmpty)
        "0"
      else if(str.head.toString == "0")
        removeZero(str.drop(1))
      else
        str
    }

    removeZero(num2.reverse.zipWithIndex.map{c =>
      val numInt = c._1.toString.toInt
      multRecur(num1, numInt, 0, "0" * c._2)
    }.reduce((a, b) => sumRecur(a, b, 0, "")))
  }

  def main(args: Array[String]) = {
    println(multiply("2", "3"))
    println(multiply("123", "456"))
    println(multiply("9133", "0000"))
    println(multiply("0000", "9133"))
  }
}