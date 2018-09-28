package com.ivansaprykin.scalatesttask

import scala.annotation.tailrec

object PolymorphicFunctions {

  // Exercise: Implement `curry`.
  // Implement a polymorphic function to check whether an `Array[A]` is sorted
  def isSorted[A](arr: Array[A], gt: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if(i == arr.length - 1) true
      else if (gt(arr(i), arr(i + 1))) loop(i + 1) // greater elements at the start, i.e 5,4,3,... considered sorted
      else false
    }
    loop(0)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise: Implement `curry`.
  //converts a function f of two arguments into a function of one argument that partially applies f
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)


  // Exercise: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  // Exercise 5: Implement `compose`
  // function composition, which feeds the output of one function to the input of another function
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
