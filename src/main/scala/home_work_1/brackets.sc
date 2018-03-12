import scala.annotation.tailrec

def balance(chars: List[Char]):Boolean = {
  def countSum(sum:Int, char:Char): Int = {
    if(char == ')') {
       sum -1
    }else
    if (char == '('){
      sum + 1
    }else{
      sum
    }
  }

  @tailrec
  def recursion(chars: List[Char], sum:Int):Boolean ={
    if(sum < 0){
       false
    }else if(chars.isEmpty && sum == 0){
      true
    }else if (!chars.isEmpty){
        recursion(chars.tail, countSum(sum, chars.head))
    }else{
        false
      }
    }

  recursion(chars, 0)
}

val str = "dfsdf(fafd) qfewf((cfewfew)dwefe)"

println(balance(str.toList))