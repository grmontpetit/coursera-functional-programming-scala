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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
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
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains same elements") {
    new TestSets {
      val set1 = Set(1, 2)
      val set2 = Set(2, 3)
      val set3 = Set(4, 5)
      val intersect1 = intersect(s3, s3)
      val intersect2 = intersect(s2, s2)
      val intersect3 = intersect(set1, set2)
      val notIntersect = intersect(s2, s3)
      assert(contains(intersect1, 3), s"The intersection between $s3 and $s3 should be 3")
      assert(contains(intersect2, 2), s"The intersection between $s2 and $s2 should be 2")
      assert(contains(intersect3, 2), s"The intersection between $set1 and $set2 should be 2")
      assert(!contains(notIntersect, 0), s"The intersection between $set2 and $set3 should not exist")
    }
  }

  test("difference between sets") {
    new TestSets {
      val set1 = Set(1, 2)
      val set2 = Set(2, 3)
      val ds = diff(set1, set2)
      assert(contains(ds, 1), s"The difference between $set1 and $set2 should be 1 and 3")
      assert(!contains(ds, 2), s"The difference between $set1 and $set2 should not contain 2")
    }
  }

  test("filter a given set with a predicate") {
    new TestSets {
      val set1 = Set(1, 2, 3, 4)
      val resultSet3 = Set(1, 2, 3)
      val pred1 = (x: Int) => x != 1
      val pred2 = (x: Int) => x == 1 || x == 2
      val filter1 = filter(set1, pred1)
      val filter2 = filter(set1, pred2)
      assert(!contains(filter1, 1), s"The filtering of $set1 with predicate $pred1 should not contain 1")
      assert(contains(filter2, 1), s"The filtering of $set1 with predicate $pred2 should contain 1")
      assert(contains(filter2, 2), s"The filtering of $set1 with predicate $pred2 should contain 2")
    }
  }

}
