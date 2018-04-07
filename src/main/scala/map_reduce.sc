def prod(f: Int => Int)(a: Int, b: Int): Int = {
  if (a> b) 1 else f(a) * prod(f)(a+1, b)
}

def factorial(n: Int): Int = {
  prod(x => x)(1, n)
}

factorial(5)

val fact = prod(x => x)_
fact(1, 5)

def mapReduce(map: Int => Int, combine: (Int, Int)=> Int, unit: Int)
             (a: Int, b: Int): Int = {
  if (a > b) unit
  else combine(map(a), mapReduce(map, combine, unit)(a+1, b))
}