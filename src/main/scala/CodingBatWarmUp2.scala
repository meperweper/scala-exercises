import scala.annotation.tailrec


object CodingBatWarmUp2 {

  def main(args: Array[String]) = {
    /* println(stringTimes("Hi", "", 3))
     println(doubleX("xHixx"))
     println(last2("ssaddsflad"))
     println(array123(Array(3,4,5,1,2,1,3)))
    println(altPairs("CodingHorror"))
    println(noTriples(Array(3, 4, 2, 1, 1, 1, 3)))
    println(127.toChar)
println("kako0"*3)
    println("abcdef".take(4))
    println("abcdef".drop(4))
    println("abcdef".takeRight(4))

    println("abcdef".dropRight(4))
    println(frontTimes("chocolate", 3))
    println(stringBits("Hello my love"))
    println(arrayCount9(Array(3, 9, 2, 1, 1, 1, 3)))
    println(sign(5))
    println(sign(-5))
    println(sign(0))
    println(stringSplosion("abcd"))
    println(countXX(""))*/
    println(arrayFront9(Array(1,3,4,5,9)))
    println(stringX("xabxcd"))
    println(array667(Array(3,6,7,5,3,2,6,7,6,6)))

    @tailrec
    def stringTimes(str: String, text: String, num: Int): String = {
      if (num == 0)
        text
      else
        stringTimes(str, text + str, num - 1)

    }
    def doubleX(str: String): Boolean = {
      val xInd: Int = str.indexOf("x")
      if ((xInd < str.length - 1) && str(xInd.+(1)) == 'x') true
      else false
    }
    def last2(str: String): Int = {
      val lNum = str.length - 1
      val lst = str.substring(lNum - 1)
      def check(ind: Int): Int = {
        if (ind == lNum - 1) 0
        else if (str.substring(ind, ind + 2) == lst) {
          1 + check(ind + 1)
        }
        else 0 + check(ind + 1)
      }
      check(0)
    }
    def array123(arr: Array[Int]): Boolean = {
      def check(ind: Int): Boolean = {
        if (ind == arr.length - 2) false
        else if (arr(ind) == 1 && arr(ind + 1) == 2 && arr(ind + 2) == 3) {
          true
        }
        else check(ind + 1)
      }
      check(0)
    }

    def altPairs(str: String): String = {
      def nth(index: Int): String = {
        if (index <= str.length - 1) {

          str.substring(index, index + 2) + nth(index + 4)
        }
        else
          ""
      }
      nth(0)
    }
    def noTriples(arr: Array[Int]): Boolean = {
      def check(ind: Int): Boolean = {
        if (ind == arr.length - 2) false
        else if (arr(ind) == arr(ind + 1) && arr(ind) == arr(ind + 2)) {
          true
        }
        else check(ind + 1)
      }
      !check(0)
    }
    def frontTimes(str: String, num: Int): String = str.take(3) * num
    def stringBits(str: String, num: Int = 0): String = {
      if (num >= str.length) ""
      else
        str(num) + stringBits(str, num + 2)
    }
    def arrayCount9(arr: Array[Int]): Int = arr.filter(x => x == 9).length
    def sign(num: Int): Int = {
      if (num == 0) 0
      else num / Math.abs(num)
    }
    def stringYak(str: String): String = {
 str.replaceAll("yak","")
 }
    def stringSplosion(str: String, ind: Int = 0): String = {
      if (ind == str.length)
        ""
      else
        str.take(ind + 1) + stringSplosion(str, ind + 1)
    }
    def countXX(str: String, ind: Int = 0): Int = {
      if (ind == str.length - 1) 0
      else if (str.substring(ind, ind + 2) == "xx")
        1 + countXX(str, ind + 1)
      else 0 + countXX(str, ind + 1)

    }
    def arrayFront9(arr:Array[Int]):Boolean= arr.indexOf(9)<4
    def stringX (str:String):String={
      str(0)+str.substring(0,str.length-1).replace("x","")+str.takeRight(1)
    }
    def array667(arr:Array[Int], ind: Int = 0):Int= {
      if (ind == arr.length - 1) 0
      else if ((arr(ind) == 6) && (arr(ind + 1) == 6 || arr(ind + 1) == 7))
        1 + array667(arr, ind + 1)
      else 0 + array667(arr, ind + 1)
    }

  }
}