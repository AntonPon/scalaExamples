class Rational(n: Int, d: Int){
  require(d != 0, "zero  denominator is not permitted")
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a%b)
  }

  val nom = n
  val denom = d
  private val g = gcd(nom, denom)
  def + (other: Rational): Rational ={
    new Rational(this.nom*other.denom + other.nom*this.denom, this.denom*other.denom)
  }
  override def toString = n/g + "/" + d/g
}


val a = new Rational(1, 3)
val b = new Rational(5, 3)

// the same as a.+(b)
// the possibility of the functions with the one argument
a + b
a.+(b)
