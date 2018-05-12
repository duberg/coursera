package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1: Set = singletonSet(1)
    val s2: Set = singletonSet(2)
    val s3: Set = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s: Set = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains same elements of two sets") {
    new TestSets {
      val s: Set = intersect(union(s1, s3), s3)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains all elements of first set that are not in second set") {
    new TestSets {
      val s: Set = diff(union(s1, s3), s3)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains subset elements of set that are accepted by a given predicate p") {
    new TestSets {
      val s: Set = filter(union(s1, s3), (el: Int) => el > 1)
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
    }
  }

  test("forall tests whether all integers within set are accepted by a given predicate p") {
    new TestSets {
      val s: Set = union(s2, s3)
      assert(forall(s, (el: Int) => el > 1), "Forall greater then 1")
      assert(!forall(s, (el: Int) => el < 1), "Forall less then 1")
      assert(forall(s1, (el: Int) => el == 1), "Forall equal 1")
      assert(forall(singletonSet(1001), (el: Int) => el == 1001), "Forall equal 1001")
      assert(forall(singletonSet(1000), (el: Int) => el == 1000), "Forall equal 1000")
      assert(forall(singletonSet(-1000), (el: Int) => el == -1000), "Forall equal -1000")
      assert(forall(singletonSet(0), (el: Int) => el == 0), "Forall equal 0")
    }
  }

  test("exists tests whether there is integer within set that is accepted by a given predicate p") {
    new TestSets {
      val s: Set = union(s2, s3)
      assert(!exists(s, (el: Int) => el == 1), "Exists 1")
      assert(exists(s, (el: Int) => el == 2), "Exists 2")
      assert(exists(s, (el: Int) => el == 3), "Exists 3")
      assert(exists(s, (el: Int) => el < 3), "Exists less than 3")
      assert(exists(s, (el: Int) => el > -1), "Exists greater than -1")
      assert(!exists(s, (el: Int) => el > 4), "Exists greater than 4")
    }
  }

  test("map transforms set by applying p to each element") {
    new TestSets {
      val s: Set = union(s2, s3)
      val mappedS1: Set = map(s, (el: Int) => el + 1)
      val mappedS2: Set = map(s, (el: Int) => el * el)
      val mappedS3: Set = map(s, (el: Int) => el - 10)
      val mappedS4: Set = map(union(s, singletonSet(1)), (el: Int) => el - 1)
      assert(contains(mappedS1, 3), "MappedS1 contains 3")
      assert(contains(mappedS1, 4), "MappedS1 contains 4")
      assert(!contains(mappedS1, 2), "MappedS1 does not contain 2")
      assert(contains(mappedS2, 9), "MappedS2 contains 9")
      assert(contains(mappedS2, 4), "MappedS2 contains 4")
      assert(!contains(mappedS2, 3), "MappedS2 does not contain 3")
      assert(contains(mappedS3, -8), "MappedS2 contains -8")
      assert(contains(mappedS3, -7), "MappedS2 contains -7")
      assert(!contains(mappedS3, -3), "MappedS2 does not contain -3")
      assert(!contains(mappedS3, 0), "MappedS2 does not contain 0")
      assert(contains(mappedS4, 0), "MappedS4 contains 0")
      assert(contains(mappedS4, 1), "MappedS4 contain 1")
      assert(contains(mappedS4, 2), "MappedS4 contain 2")
    }
  }

  test("map transforms {1,3,4,5,7,1000} to {0,2,3,4,6,999}") {
    new TestSets {
      val s: Set =
        union(
          union(
            union(
              union(
                union(s1, s3), singletonSet(4)
              ), singletonSet(5)
            ), singletonSet(7)
          ), singletonSet(1000))
      val mappedS1: Set = map(s, (el: Int) => el - 1)
      assert(contains(mappedS1, 0), "MappedS1 contains 0")
      assert(contains(mappedS1, 2), "MappedS1 contains 2")
      assert(contains(mappedS1, 3), "MappedS1 contains 3")
      assert(contains(mappedS1, 4), "MappedS1 contains 4")
      assert(contains(mappedS1, 6), "MappedS1 contains 6")
      assert(contains(mappedS1, 999), "MappedS1 contains 999")
    }
  }
}
