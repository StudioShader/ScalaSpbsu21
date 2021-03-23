package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sum}

class MyGenericListTest extends AnyFunSuite {

  test("construct"){
    assert(size(MyList("Some", fromSeq(Seq(1,2,3))))==4)
    assert(MyList("Some", fromSeq(Seq(1,2,3))).head=="Some")
    assert(MyList("Some", fromSeq(Seq(1,2,3))).take(2) == MyList("Some",MyList(1,MyNil)))
    assert(MyList("Some", fromSeq(Seq(1))).map(_ + "s") == MyList("Somes",MyList("1s",MyNil)))
    assert(MyList("Some", fromSeq(Seq(Some(1)))).map(_ + "s") == MyList("Somes",MyList("Some(1)s",MyNil)))
    // i don't really understand why this is happening, but i guess this could be interpreted as a correct solution
  }

  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)

    assert(fromSeq(Seq("ab","cd","ef")).head == "ab")
    assert(fromSeq(Seq("some")).head == "some")
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyNil)

    assert(fromSeq(Seq("ab","cd","ef")).tail == fromSeq(Seq("cd","ef")))
    assert(fromSeq(Seq("some")).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))

    assert(fromSeq(Seq("ab","cd","ef")).drop(0) == fromSeq(Seq("ab","cd","ef")))
    assert(fromSeq(Seq("ab","cd","ef")).drop(2) == fromSeq(Seq("ef")))
    assert(fromSeq(Seq("ab","cd","ef")).drop(3) == MyNil)
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))

    assert(fromSeq(Seq("ab","cd","ef")).take(0) == MyNil)
    assert(fromSeq(Seq("ab","cd","ef")).take(2) == fromSeq(Seq("ab","cd")))
    assert(fromSeq(Seq("ab","cd","ef")).take(3) == fromSeq(Seq("ab","cd","ef")))
  }

  test("map") {
    assert(MyNil.map((_: Int) * 2) == MyNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))

    assert(MyNil.map((_: String) + "some") == MyNil)
    assert(fromSeq(Seq("ab","cd","ef")).map(_ + "123") == fromSeq(Seq("ab123","cd123","ef123")))
    assert(fromSeq(Seq("ab","cd","ef")).map(identity) == fromSeq(Seq("ab","cd","ef")))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
    assert(size(fromSeq(Seq("ab","cd","ef"))) == 3)
  }

//  test("sum") {
//    assertThrows[UnsupportedOperationException](sum(MyNil))
//    assert(sum(fromSeq(Seq(1,2,3))) == 6)
//    assert(sum(fromSeq(Seq(1))) == 1)
//  }

}