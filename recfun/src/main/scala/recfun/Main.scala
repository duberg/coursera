package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def b(balance: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) balance == 0 else chars.head match {
        case '(' => b(balance + 1, chars.tail)
        case ')' => if (balance - 1 < 0) false else b(balance - 1, chars.tail)
        case _ => b(balance, chars.tail)
      }
    }
    b(0, chars)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def c(money: Int, coins: List[Int], i: Int): Int = {
      money match {
        case m if m == 0 => 1
        case m if m < 0 => 0
        case m if coins.size == i && m > 0 => 0
        case _ => c(money - coins(i), coins, i) + c(money, coins, i + 1)
      }
    }
    c(money, coins, 0)
  }
}
