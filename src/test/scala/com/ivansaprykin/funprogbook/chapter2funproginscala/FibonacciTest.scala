package com.ivansaprykin.funprogbook.chapter2funproginscala

import com.ivansaprykin.funprogbook.TestSpec
import com.ivansaprykin.funprogbook.chapter2funproginscala.Fibonacci.fib

class FibonacciTest extends TestSpec {

  "A fib function" should "return n-th number in Fibonacci sequence" in {
    assertResult(0) {
      fib(1)
    }
    assertResult(1) {
      fib(2)
    }
    assertResult(1) {
      fib(3)
    }
    assertResult(2) {
      fib(4)
    }
    assertResult(3) {
      fib(5)
    }
    assertResult(5) {
      fib(6)
    }
  }

}
