
  def sumOfFuncs(f: Double => Double, g: Double => Double): Double => Double={
    def sum(x:Double):Double = f(x) + g(x)
    sum
  }

  def sumOfFuncs2(f: Double => Double, g: Double => Double): Double => Double={
   (x) => f(x) + g(x)
  }

  def func1(x:Double) = x+2
  val func2 = (x: Double) => x*2

  val sum1 = sumOfFuncs2(func1, func2)

  println(sum1(3+ 4))

  def sumOfFuncs3(f: Double => Double): ( Double => Double)=> Double => Double ={

    (g:Double=>Double)=> (x) => g(x) + f(x)
  }

  sumOfFuncs3(x => x+3)(x=>x*8)



  def sumOfInts4(x: Int): Int => Int={z => x + z }

  def sumOfInts5(x:Int)(y:Double):Double = x+y

  def l = sumOfInts5(5)_