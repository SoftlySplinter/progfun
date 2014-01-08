package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if(c == 0 || c == r || r == 0) 
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def rfind(chars: List[Char], opens: Int, curIndex: Int): Int = 
        if(chars.isEmpty) 
          -1
        else if(chars.head == ')')
          if(opens == 0)
            curIndex
          else
            rfind(chars.tail, opens - 1, curIndex + 1)
        else
          if(chars.head == '(') rfind(chars.tail, opens + 1, curIndex + 1)
          else rfind(chars.tail, opens, curIndex + 1)

      if(chars.isEmpty) true
      else
        if(chars.head == '(') {
          val r = rfind(chars.tail, 0, 0);
          if(r == -1) false
          else balance(chars.tail take r) && balance(chars.tail drop r + 1)
        }
        else if(chars.head == ')') false
        else balance(chars.tail)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def addAll(target: Int, value: Int, list: List[Int], count: Int): Int = 
      if(target == value)
        count + 1
      else if(target < value || list.isEmpty) count
      else
        addAll(target, value + list.head, list, count) + addAll(target, value, list.tail, count)

    def doChange(money: Int, coins: List[Int], count: Int): Int =
      if(coins.isEmpty) count
      else if(coins.head > money) doChange(money, coins.tail, count)
      else doChange(money, coins.tail, count + addAll(money, coins.head, coins, 0))

    if(money == 0) 0
    else doChange(money, coins, 0)
  }
    
}
