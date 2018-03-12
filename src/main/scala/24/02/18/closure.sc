// closure implementation
// we incapsulate the function into environment
// and when call it, we call with all environment
// it gives us a more abstraction during implementation


def sum(f: Int =>  Int): (Int, Int)=> Int   = {
  def sumF(a: Int, b:Int): Int = {
    if (a > b) 0 else f(a) + sumF(a+1, b)
  }
  sumF
}


val squareSum = sum(x => x*x)

val cubeSum = sum(x => x*x*x)

val a = 1
val b = 10

squareSum(1 , 10)
cubeSum(1, 10)


def closureFunc(a: Int): (Int)=> Int = {
  val c = 30
  def sum(b: Int): Int={
    a+ b +c
  }
sum
}


// we will evaluate some sum 30 + b + a

val closureF = closureFunc(20)
closureF(10)
closureF(20)

// currying