package com.ivansaprykin.scalatesttaskcom.ivansaprykin.scalatesttask

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// `List` companion object. Contains functions for creating and working with lists.
object List { 

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // EXERCISE
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // EXERCISE
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  // EXERCISE
  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(xs: List[A], currentPos: Int): List[A] = xs match {
      case Nil => Nil
      case Cons(_, xs) if currentPos == n => xs
      case Cons(_, xs) => loop(xs, currentPos + 1)
    }

    loop(l, 0)
  }

  // EXERCISE
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case Cons(_, xs) => xs

  }


  // EXERCISE
  // given List(1,2,3,4) , init will return List(1,2,3)
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil // at start we got an empty list
    case Cons(_, Nil) => Nil // at start we got list with one element
    case Cons(x, Cons(_, Nil)) => Cons(x, l) // stop recursion to not include last element
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // EXERCISE
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => 1 + z)

  // EXERCISE
  // general list-recursion function, that is tail-recursive
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // EXERCISE
  // Write sum , product , and a function to compute the length of a list using foldLeft .
  def sumFl(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productFl(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthFl[A](l: List[A]): Int = foldLeft(l, 0)((_, _) => 1 + 0)

  // EXERCISE
  // Write a function that returns the reverse of a list (given List(1,2,3) it returns
  // List(3,2,1) ). See if you can write it using a fold.
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, x) => Cons(x, z))

}