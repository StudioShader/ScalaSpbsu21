package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sum, sort}

class IntListTest extends AnyFunSuite {

  test("sorting") {
    assert(sort(fromSeq(Seq(3, 2, 1))) == fromSeq(Seq(1, 2, 3)))
    assert(sort(FPNil) == FPNil)
    assert(sort(fromSeq(Seq(3, 4, 5, 6, 1, 2))) == fromSeq(Seq(1, 2, 3, 4, 5, 6)))
    assert(sort(fromSeq(Seq("aaa", "c", "bb"))) == fromSeq(Seq("aaa", "bb", "c")))
    assert(sort(fromSeq(Seq(2, 2, 3))) == fromSeq(Seq(2, 2, 3)))

    case class Person(name:String, age:Int)
    val people = FPNil.::(Person("bob", 30)).::(Person("ann", 32)).::(Person("carl", 19))

    implicit object AgeOrdering extends Ordering[Person] {
      def compare(a:Person, b:Person): Int = b.age compare a.age
    }

    assert(sort(people) == FPNil.::(Person("carl", 19)).::(Person("bob", 30)).::(Person("ann", 32)))
  }

  test("construct") {
    assert(size(FPList("Some", fromSeq(Seq(1, 2, 3)))) == 4)
    assert(FPList("Some", fromSeq(Seq(1, 2, 3))).head == "Some")
    assert(FPList("Some", fromSeq(Seq(1, 2, 3))).take(2) == FPList("Some", FPList(1, FPNil)))
    assert(FPList("Some", fromSeq(Seq(1))).map(_ + "s") == FPList("Somes", FPList("1s", FPNil)))
    assert(FPList("Some", fromSeq(Seq(Some(1)))).map(_ + "s") == FPList("Somes", FPList("Some(1)s", FPNil)))
    // i don't really understand why this is happening, but i guess this could be interpreted as a correct solution
  }

  test("head") {
    assert(fromSeq(Seq(1, 2, 3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)

    // String
    assert(fromSeq(Seq("ab", "cd", "ef")).head == "ab")
    assert(fromSeq(Seq("some")).head == "some")
    //assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1, 2, 3)).tail == fromSeq(Seq(2, 3)))
    assert(fromSeq(Seq(1)).tail == FPNil)

    assert(fromSeq(Seq("ab", "cd", "ef")).tail == fromSeq(Seq("cd", "ef")))
    assert(fromSeq(Seq("some")).tail == FPNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1, 2, 3)).drop(0) == fromSeq(Seq(1, 2, 3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(3) == FPNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).drop(10))

    assert(fromSeq(Seq("ab", "cd", "ef")).drop(0) == fromSeq(Seq("ab", "cd", "ef")))
    assert(fromSeq(Seq("ab", "cd", "ef")).drop(2) == fromSeq(Seq("ef")))
    assert(fromSeq(Seq("ab", "cd", "ef")).drop(3) == FPNil)
  }

  test("take") {
    assert(fromSeq(Seq(1, 2, 3)).take(0) == FPNil)
    assert(fromSeq(Seq(1, 2, 3)).take(2) == fromSeq(Seq(1, 2)))
    assert(fromSeq(Seq(1, 2, 3)).take(3) == fromSeq(Seq(1, 2, 3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).take(10))

    assert(fromSeq(Seq("ab", "cd", "ef")).take(0) == FPNil)
    assert(fromSeq(Seq("ab", "cd", "ef")).take(2) == fromSeq(Seq("ab", "cd")))
    assert(fromSeq(Seq("ab", "cd", "ef")).take(3) == fromSeq(Seq("ab", "cd", "ef")))
  }

  test("map") {
    assert(FPNil.map((_: Int) * 2) == FPNil)
    assert(fromSeq(Seq(1, 2, 3)).map(_ * 2) == fromSeq(Seq(2, 4, 6)))
    assert(fromSeq(Seq(1, 2, 3)).map(identity) == fromSeq(Seq(1, 2, 3)))

    assert(FPNil.map((_: String) + "some") == FPNil)
    assert(fromSeq(Seq("ab", "cd", "ef")).map(_ + "123") == fromSeq(Seq("ab123", "cd123", "ef123")))
    assert(fromSeq(Seq("ab", "cd", "ef")).map(identity) == fromSeq(Seq("ab", "cd", "ef")))
  }

  test("size") {
    assert(size(FPNil) == 0)
    assert(size(fromSeq(Seq(1, 2, 3))) == 3)
    assert(size(fromSeq(Seq("ab", "cd", "ef"))) == 3)
  }

  //  test("sum") {
  //    assertThrows[UnsupportedOperationException](sum(FPNil))
  //    assert(sum(fromSeq(Seq(1,2,3))) == 6)
  //    assert(sum(fromSeq(Seq(1))) == 1)
  //  }

}
