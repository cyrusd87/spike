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
        if (list.isEmpty) count
        else {
          list.head match {
            case '(' =>  opened(list.tail,count +1)
            case _  => count
          }
        }

      }

      def closed(list:List[Char],count:Int): Int = {
        if (list.isEmpty) count
        else {
          list.head match {
            case ')' =>  closed(list.tail,count +1)
            case _  => count
          }
        }
      }

       def check1(list:List[Char], count:Int):Boolean = {
          if(list.isEmpty) count == 0
          list.head match {
            case '(' => check1(list.tail,count+1)
            case ')' => check1(list.tail,count-1)
            case _ => check1(list.tail,count)
          }
       }

      def check(list:List[Char]):Boolean = {
        if (!list.isEmpty) {
          val count = opened(list, 0)

          if (list.length >= count &&  count == closed(list.drop(count), 0)) {
            check(list.drop(count*2))
          } else false

        } else true
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
