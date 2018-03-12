type Set = Int => Boolean

def contains(set: Set, element: Int): Boolean = {set(element)}

def singletonSet(elem: Int):Set =  (x: Int) => x == elem

def union(s: Set, t:Set):Set = (x: Int) => contains(s, x) || contains(t, x)

def intersect(s: Set, t:Set): Set = (x:Int) => contains(s, x) && contains(t, x)

def diff(s: Set, t:Set): Set =  (x: Int) => contains(s, x) && !contains(t, x)

def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)


var s1 = singletonSet(4)

var s2 = singletonSet(6)

var uni = union(s1, s2)


var uni2 = union(singletonSet(12), singletonSet(6))
var uni3 = union(singletonSet(8), singletonSet(10))

var inters = intersect(uni2, uni)

contains(inters, 6)
contains(inters, 5)
contains(inters, 4)

contains(uni, 10)

var superUnion = union(union(uni, uni2), uni3)


def forall(s: Set, p: Int => Boolean): Boolean = {


  def iter(a: Int): Boolean = {
    if (contains(s, a) && !p(a)) p(a)
    else if (a == 1000) true
    else iter(a + 1)
  }

  iter(-1000)
}


var l = forall(superUnion, (x: Int) => x % 2 == 0)


def exists(s: Set, p: Int => Boolean): Boolean = {
  !forall(s, (x: Int) => !p(x))
}

exists(superUnion, (x: Int) => x % 21 == 0)

def map2(s: Set, f: Int => Int): Set =  {
// lambdas are functional literals or anonymous function
  (x: Int) => {
    def iter(a: Int):Boolean ={
      if (contains(s, a) && f(a) == x ) true
      else if (a == 1000) false
      else iter(a+ 1)
    }
    iter(-1000)
  }
}

var quad = map2(superUnion, (x: Int) => x*x)

contains(quad,36)

def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s,  (y: Int)=> f(y) == x)

var quad2 = map(superUnion, (x: Int) => x*x)

contains(quad2, 64)




val one = singletonSet(1)
val two = singletonSet(2)

def positiveIntegers(): Set = elem => elem > 0

def evenIntegers(): Set = elem => elem % 2 == 0

def positiveIntegersDivisibleByFour(): Set = elem => elem > 0 && elem % 4 == 0

def multiplyBySix(): Int => Int = elem => elem * 6
