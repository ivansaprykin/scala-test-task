package com.ivansaprykin.scalatesttask

import com.ivansaprykin.funprogbook.TestSpec
import package com.ivansaprykin.scalatesttask.PolymorphicFunctions._

class PolymorphicFunctionsTest extends TestSpec {

  "An isSorted function" should "check if the given array is sorted" in {
    assertResult(true, "should return true in case of sorted array") {
      isSorted(Array(5, 4, 3, 2, 2, 0), (x: Int, y: Int) => x >= y)
    }
    assertResult(false, "should return false in case of unsorted array") {
      isSorted(Array(4, 5, 3, 2, 2, 0), (x: Int, y: Int) => x >= y)
    }
  }

}
