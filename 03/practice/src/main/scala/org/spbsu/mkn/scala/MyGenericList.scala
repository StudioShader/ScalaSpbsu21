
package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+A] {
  def head: A

  def tail: MyGenericList[A]

  def drop(n: Int): MyGenericList[A]

  def take(n: Int): MyGenericList[A]

  def map[B](f: A => B): MyGenericList[B]

  //def ::(elem: A): List[A]
}

case class MyList[A](head: A, tail: MyGenericList[A]) extends MyGenericList[A] {

  override def drop(n: Int): MyGenericList[A] = {
    if (n <= 0) {
      return this
    }
    tail.drop(n - 1)
  }

  override def take(n: Int): MyGenericList[A] = {
    if (n <= 0) {
      return MyNil
    }
    MyList[A](head, tail.take(n-1))
  }

  override def map[B](f: A => B): MyGenericList[B] = {
    MyList[B](f(head), tail.map(f))
  }

  //  override def ::(elem: A): List[A] = {
  //    FPList[A](elem, this)
  //  }
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = {
    throw new UnsupportedOperationException("trying to take non existing element")
  }

  override def tail: MyGenericList[Nothing] = {
    throw new UnsupportedOperationException("trying to take non existing element")
  }

  override def drop(n: Int): MyGenericList[Nothing] = {
    if (n != 0) {
      throw new UnsupportedOperationException("trying to drop more elements than exists")
    }
    this
  }

  override def take(n: Int): MyGenericList[Nothing] = {
    if (n > 0) {
      throw new UnsupportedOperationException("trying to take more elements than exists")
    }
    this
  }

  override def map[B](f: Nothing => B): MyGenericList[B] = {
    this
  }
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = {
    if (seq.isEmpty) {
      return MyNil
    }
    MyList(seq.head, fromSeq(seq.tail))
  }
  def size[A](list: MyGenericList[A]): Int = foldLeft[A, Int]((a, _) => a + 1, 0, list)
  @tailrec
  def foldLeft[A, B](f: (B, A) => B, start: B, list: MyGenericList[A]): B = {
    if (list.equals(MyNil)) return start
    foldLeft(f, f(start, list.head), list.tail)
  }
  def sum[A](list: MyGenericList[A]): A = ???
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}