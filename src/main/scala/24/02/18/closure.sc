// closure implementation
// we incapsulate the function into environment
// and when call it, we call with all environment
// it gives us a more abstraction during implementation

// sum function with closure
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
closureFunc(25)(33)
// currying

// the possibility to translate the evaluation of the function that takes
// multiple arguments into evaluation of the sequence of functions,
// each with a single (or less the original) argument.

// sequence of anonymous functions calls
// instead of having specified very specific functional type of our result,
// we can split the list of arguments into two sets and set
// the result with expecting type

// it  can be useful when you get some of the arguments later


// sum: (Z _-> Z) -> (ZxZ -> Z)
// the same resulting type as in the sum above
def sum_c(f: Int => Int)(a: Int, b: Int):Int ={
  if (a>b) 0 else f(a) + sum_c(f)(a+1, b)
}

// two ways of avoiding the error:
// def error = sum_c(x => x+x)

// 1) to set explicit the return type of the function
// without some arguments lists

val passed: (Int, Int)=> Int = sum_c(x =>  x*x)

passed(2, 4)

// use notation '_'

val passed2 = sum_c(x => 2*x)_
passed2(1, 10)

// we can use scala's curring
def f = (a: Int, b: Int, c:Int) => a + b +c

val curry = f.curried

val curry1 = curry(1)
curry1  (2)(3)


def fibNumber = (n: Int)=>{
  def f(a: Int, b: Int, k: Int): Int = {
    if (k == 0) b + a
    else f(b, b+a, k-1)
  }
  f(1, 1, n)
}

def factorial = (n: Int) =>{

}

