class Interval(left: Double, right: Double) {
  require(left < right,
    s"Lower bound $left is greater than upper bound $right")

  def lowerBound = left
  def upperBound = right

  override def toString =
    "[" + lowerBound + "; " + upperBound + ")"

  def +(that: Interval): Interval =
    new Interval(
      this.lowerBound + that.lowerBound,
      this.upperBound + that.upperBound
    )

  def unary_- : Interval =
    new Interval(-upperBound, -lowerBound)

  def *(that: Interval): Interval = {
    def min(a: Double, b: Double, c: Double, d: Double) = {
      val min1 = math.min(a, b)
      val min2 = math.min(min1, c)
      math.min(min2, d)
    }
    def max(a: Double, b: Double, c: Double, d: Double) = {
      val max1 = math.max(a, b)
      val max2 = math.max(max1, c)
      math.max(max2, d)
    }
    val p1 = this.lowerBound * that.lowerBound
    val p2 = this.lowerBound * that.upperBound
    val p3 = this.upperBound * that.lowerBound
    val p4 = this.upperBound * that.upperBound

    new Interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))
  }

  def /(that: Interval): Interval =
    this * new Interval(1.0 / that.upperBound, 1.0 / that.lowerBound)

  // only for intersected intervals
  def union(that: Interval): Interval =
    new Interval(
      math.min(this.lowerBound, that.lowerBound),
      math.max(this.upperBound, that.upperBound)
    )
  def intersect(that: Interval): Interval =
    new Interval(
      math.max(this.lowerBound, that.lowerBound),
      math.min(this.upperBound, that.upperBound)
    )

  // ∪ ∩

  // lowerBound < upperBound
  // -lowerBound > -upperBound

}
//////////////////////////////
//package intervals_18

object IntervalMain {

  def main(args: Array[String]): Unit = {
    val i1 = new Interval(2.3, 5.6)
    println(i1)

    //    val i2 = new Interval(2.3, -5.6)

    val i3 = new Interval(2, 5)
    val i4 = new Interval(3, 9)
    println( i3 + i4 )
    println( i3.+(i4) )

    println(-i3)
    println(i3 intersect i4)
    println(i3 union i4)

    println(i4 intersect i3)
    println(i4 union i3)

    val i5 = new Interval(7, 12)
    println( (i3 union i4) union i5 )
    println( i3 union (i4 union i5) )
  }

}
///////////////////////////////////////
//package intervals_18
/*
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
  * ScalaCheck property-based test suite.
  */
object Interval18Specification extends Properties("Interval") {

  lazy val genInterval: Gen[Interval] = for {
    k <- arbitrary[Double].suchThat(x => x < 1000 || x > -1000)
    m <- arbitrary[Double].suchThat(y => y > k && (y < 1000 || y > -1000))
  } yield new Interval(k, m)

  implicit lazy val arbInterval: Arbitrary[Interval] = Arbitrary(genInterval)

  val intersectedIntervals: Gen[(Interval, Interval)] =
    for {
      i1 <- arbitrary[Interval]
      i2 <- arbitrary[Interval].filter(i =>
        math.max(i.lowerBound, i1.lowerBound)
          < math.min(i.upperBound, i1.upperBound))
    } yield (i1, i2)

  property("union commutativity") = forAll(intersectedIntervals) { pair =>
    (pair._1 union pair._2) == (pair._2 union pair._1)

  }

}
////////////////////////////////
package intervals_18

import org.scalatest.FunSuite

class Interval18Test extends FunSuite {

  test("Interval boudaries are correct") {
    val i1 = new Interval(2, 3)
    assert(i1.lowerBound == 2)
    assert(i1.upperBound == 3)
  }

  test("Interval boudaries coul not be reverted") {
    intercept[IllegalArgumentException] {
      new Interval(3, 2)
    }
  }

  test("Union of two intervals is an interval with min of lowerBounds " +
    "and max of upperBounds") {
    val union1 = new Interval(2, 5) union new Interval(4, 7)
    assert(union1.lowerBound == 2)
    assert(union1.upperBound == 7)
  }

}*/