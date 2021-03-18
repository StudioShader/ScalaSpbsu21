package org.spbsu.mkn.scala

import org.graalvm.compiler.core.common.util.IntList
import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec

sealed trait List[+A] {
  def head: A

  def tail: List[A]

  def drop(n: Int): List[A]

  def take(n: Int): List[A]

  def map[B](f: A => B): List[B]

  def ::[B >: A](elem: B): List[B]
}

case class FPList[A](head: A, tail: List[A]) extends List[A] {

  override def drop(n: Int): List[A] = {
    if (n <= 0) {
      return this
    }
    tail.drop(n - 1)
  }

  override def take(n: Int): List[A] = {
    if (n <= 0) {
      return FPNil
    }
    FPList[A](head, tail.take(n - 1))
  }

  override def map[B](f: A => B): List[B] = {
    FPList[B](f(head), tail.map(f))
  }

  override def ::[B >: A](elem: B): List[B] = {
    FPList(elem, this)
  }
}


case object FPNil extends List[Nothing] {
  override def head: Nothing = {
    throw new UnsupportedOperationException("trying to take non existing element")
  }

  override def tail: List[Nothing] = {
    throw new UnsupportedOperationException("trying to take non existing element")
  }

  override def drop(n: Int): List[Nothing] = {
    if (n != 0) {
      throw new UnsupportedOperationException("trying to drop more elements than exists")
    }
    this
  }

  override def take(n: Int): List[Nothing] = {
    if (n > 0) {
      throw new UnsupportedOperationException("trying to take more elements than exists")
    }
    this
  }

  override def map[B](f: Nothing => B): List[B] = {
    this
  }

  override def ::[A](elem: A): List[A] = {
    FPList[A](elem, this)
  }
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): List[A] = {
    if (seq.isEmpty) {
      return FPNil
    }
    FPList(seq.head, fromSeq(seq.tail))
  }

  def sum[A](list: List[A]): A = ???

  def size[A](list: List[A]): Int = foldLeft[A, Int]((a, _) => a + 1, 0, list)

  @tailrec
  def foldLeft[A, B](f: (B, A) => B, start: B, list: List[A]): B = {
    if (list.equals(FPNil)) return start
    foldLeft(f, f(start, list.head), list.tail)
  }

  def sort[T](list: List[T])(implicit comparator: Ordering[T]): List[T] = list match {
    case FPNil => FPNil
    case other =>
      val sorted: List[T] = sort(other.tail)
      push(sorted, other.head)
  }
  def sort(list: List[Nothing]): List[Nothing] ={
    FPNil
  }

  def push[T](list: List[T], some: T)(implicit comparator: Ordering[T]): List[T] = list match {
    case FPNil => FPList(some, FPNil)
    case other =>
      if (comparator.compare(some, other.head) > 0) {
        FPList(other.head, push(other.tail, some))
      } else {
        FPList(some, other)
      }
  }
}
