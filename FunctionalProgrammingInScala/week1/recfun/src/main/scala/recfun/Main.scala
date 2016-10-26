package recfun

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
    def pascal(c: Int, r: Int): Int = {

      def factorial(n:Int):Int = {
        if(n<=1) 1
        else
          n * factorial(n-1)
      }
       factorial(r)/(factorial(c)*factorial(r-c))
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check(list:List[Char],count:Int): Boolean = {
        if (list.isEmpty || count < 0) count == 0
        else {
          list.head match {
            case '(' => check(list.tail, count + 1)
            case ')' => check(list.tail, count - 1)
            case _ => check(list.tail, count)
          }
        }
      }

        check(chars,0);
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else if (money <= 0 && !coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
  }
