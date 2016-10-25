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
      def build(list:List[Char],tmp:String): String = {
        if(!list.isEmpty) {
          list.head match {
            case '(' =>  build(list.tail, tmp + '(')
            case ')' => build(list.tail, tmp +  ')')
            case _ => build(list.tail, tmp)
          }
        } else{
          tmp
        }
      }

      def opened(list:List[Char],count:Int): Int = {
        list.head match {
          case '(' =>  opened(list.tail,count +1)
          case _  => count
        }
      }

      def closed(list:List[Char],count:Int): Int = {
        list.head match {
          case ')' =>  closed(list.tail,count +1)
          case _  => count
        }
      }

      def check(list:List[Char]):Boolean = {
        if(!list.isEmpty)  {
          if(list.head != list.last) {
            check(list.tail.dropRight(1))
          } else {
            false
          }
        } else {true}
      }
      val newList = build(chars,"").toList

    if(newList.length % 2 == 0) check(newList)
    else false
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
