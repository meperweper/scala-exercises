
object CodingBatWarmUp1 {

  def main(args: Array[String]) = {
    /*println(sleepIn(false, false))
    println(sleepIn(true, false))
    println(sleepIn(false, true))
    println(sleepIn(true, true))
    println(nearHundred(99))
    println(missingChar("abcdef",2))
    println(backAround("cat"))
    println(startsHi("Hilol"))
    println(startsHi("Helol"))
    println(hasTeen(Array(10, 12, 13, 15, 18, 19, 20)))
    println(mixStart("vix"))
    println(close10(2,3))
    println(close10(11,9))
    println(close10(3,23))
    println(stringE("Heel"))
    println(everyNth("ojhgdsxaz", 2))
    println(monkeyTrouble(aSmile = true,bSmile = true))
    println(parrotTrouble(talking = true, 6))
    println(parrotTrouble(talking = true, 7))
    println(parrotTrouble(talking = false, 6))
    println(posNeg(1, -1, negative = false))
    println(posNeg(-1, 1, negative = false))
    println(posNeg(-1, -1, negative = true))
    println(posNeg(-1, 1, negative = true))
    println(frontBack("abcef"))
    println(or35(15))
    println(or35(7))
    println(icyHot(-2, 101))
    println(loneTeen(Array(10, 12, 13, 15, 18, 19, 20)))
    println(startOz("narkoz", "varh"))
    println(in3050(33, 45))
    println(lastDigit(33, 45))
    println(lastDigit(35, 45))
    println(sumDouble(3, 5))
    println(sumDouble(3, 3))
    println(makes10(3, 7))
    println(notString("abcdef"))
    println(notString("notabcdef"))
    println(front3("abcdef"))
    println(front22("abcdef"))
    println(in1020(11,9))*/
    println(delDel("cdelds"))
    println(delDel("acdelds"))
    println(intMax(11, 9, 44))
    println(max1020(11, 9))
    println(endUp("abcdef"))






    def sleepIn(weekday: Boolean, vacation: Boolean): Boolean = !weekday || vacation

    def nearHundred(num: Int): Boolean = {
      Math.abs(num - 100) <= 10 || Math.abs(num - 200) <= 10
    }

    def missingChar(str: String, index: Int): String = {
      str.substring(0, index).concat(str.substring(index + 1))
    }

    def backAround(str: String): String = {
      val index = str.length
      str(index - 1) + str + str(index - 1)
    }

    def startsHi(str: String): Boolean = str.substring(0, 2).toLowerCase == "hi"

    def hasTeen(args: Array[Int]): Boolean = {
      args.filter(x => Math.abs(x - 16) <= 3).length > 0
    }

    def mixStart(str: String): Boolean = {
      str.substring(1, 3).toLowerCase == "ix"
    }

    def close10(first: Int, second: Int): Int = {
      val diff = Math.abs(first - 10) - Math.abs(second - 10)
      if (diff > 0) second
      else if (diff < 0) first
      else diff
    }

    def stringE(str: String): Boolean = {
      str.filter((x: Char) => x == 'e').length >= 3
    }

    def everyNth(str: String, n: Int): String = {
      def nth(index: Int): String = {
        if (index <= str.length - 1) str(index) + nth(index + n)
        else ""
      }
      nth(0)
    }

    def monkeyTrouble(aSmile: Boolean, bSmile: Boolean): Boolean = aSmile == bSmile
    def parrotTrouble(talking: Boolean, hour: Int): Boolean = talking && (20 < hour || hour < 7)
    def posNeg(aNum: Int, bNum: Int, negative: Boolean): Boolean = ((aNum > 0) == (bNum > 0)) == negative
    def frontBack(str: String): String = {
      val index = str.length
      str(index - 1) + str.substring(1, index - 2) + str(0)
    }
    def or35(num: Int): Boolean = (num % 3 == 0) || (num % 5 == 0)
    def icyHot(aNum: Int, bNum: Int): Boolean = (aNum < 0 && bNum > 100) || (aNum > 100 && bNum < 0)
    def loneTeen(args: Array[Int]): Boolean = {
      val teens = args.filter(x => Math.abs(x - 16) <= 3).length
      teens > 0 && teens < args.length
    }
    def startOz(str: String, look: String): String = {
      def check(index: Int): String = {
        if (index < look.length)
          if (str(index) == look(index))
            str(index) + check(index + 1)
          else
            "" + check(index + 1)
        else
          ""
      }
      "" + check(0)
    }
    def in3050(first: Int, second: Int): Boolean =
      ((first >= 30 && second >= 30) && (first <= 40 && second <= 40)) ||
        ((first >= 40 && second >= 40) && (first <= 50 && second <= 50))

    def lastDigit(aNum: Int, bNum: Int): Boolean = (aNum % 10) == (bNum % 10)
    def sumDouble(aNum: Int, bNum: Int): Int = if (aNum == bNum) aNum * 4 else aNum + bNum
    def makes10(aNum: Int, bNum: Int): Boolean = (aNum == 10) || (bNum == 10) || (aNum + bNum == 10)
    def notString(str: String): String = if (str.substring(0, 3).equals("not")) str else "not" + str
    def front3(str: String): String = str.substring(0, 3) + str.substring(0, 3) + str.substring(0, 3)
    def front22(str: String): String = str.substring(0, 2) + str + str.substring(0, 2)
    def in1020(first: Int, second: Int): Boolean = (first >= 10 && first <= 20) || (second >= 10 && second <= 20)
    def delDel(str: String): String = if (str.indexOf("del") == 1) str(0) + str.substring(4) else str
    def intMax(aNum: Int, bNum: Int, cNum: Int): Int = Math.max(Math.max(aNum, bNum), cNum)

    def max1020(first: Int, second: Int): Int =
      if ((first >= 10 && first <= 20) || (second >= 10 && second <= 20))
        Math.max(first, second)
      else
        0

    def endUp(str: String): String = {
      val num = str.length
      if (num <= 3)
        str.toUpperCase
      else
        str.substring(0, num - 3) + str.substring(num - 3).toUpperCase
    }
  }


}