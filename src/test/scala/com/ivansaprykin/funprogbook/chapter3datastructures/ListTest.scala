package com.ivansaprykin.funprogbook.chapter3datastructures

import com.ivansaprykin.funprogbook.TestSpec


class ListTest extends TestSpec {

  "A length function" should "return length of provided list" in {
    assertResult(0, "length of empty list should be 0") {
      List.length(Nil)
    }
    assertResult(1, "should return 1 for list of one element") {
      List.length(Cons(1, Nil))
    }
    assertResult(2, "should return 2 for list of 2 elements") {
      List.length(Cons(1, Cons(1, Nil)))
    }
  }


  "A foldLeft function" should "apply the given function to all elements of the list, starting from head" in {
    assertResult(3, "should return sum of elements of the list in case foldLeft(list, 0)(_ + _)") {
      List.foldLeft(Cons(1, Cons(1, Cons(1, Nil))), 0)(_ + _)
    }
    assertResult(5, "should return the initial value in case of empty list") {
      List.foldLeft(Nil, 5)((_, _) => 7)
    }
  }

  "A reverse function" should "return the reverse of the given list" in {
    assertResult(Cons(1, Cons(2, Cons(3, Nil))), "should return List(3,2,1) given: List(1,2,3) ") {
      List.reverse(Cons(3, Cons(2, Cons(1, Nil))))
    }
  }

  "A appendViaFoldRight function" should "append one given list after the other given list" in {
    val list1 = Cons(1, Cons(2, Nil))
    val list2 = Cons(3, Cons(4, Cons(5, Nil)))
    assertResult(List.append(list1, list2)) {
      List.appendViaFoldRight(list1, list2)
    }
  }
}
