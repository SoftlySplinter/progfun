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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements") {
    new TestSets {
      val s = intersect(s1, s2)
      val t = intersect(s1, s1)
      val u = intersect(union(s1, s2), union(s1, s3))
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(contains(t, 1), "Intersect 3")
      assert(!contains(t, 2), "Intersect 4")
      assert(contains(u, 1), "Intersect 5")
      assert(!contains(u, 2), "Intersect 6")
      assert(!contains(u, 3), "Intersect 7")
    }
  }

  test("diff contains all elements") {
    new TestSets{
      val s = union(s1, s2)
      val d = diff(s, s1)
      assert(!contains(d, 1), "diff 1")
      assert(contains(d, 2), "diff 2")
    }
  }

  test("filter x < 0") {
    new TestSets {
      val s4 = singletonSet(-1)
      val su = union(union(s1, s4), s2)
      val f = filter(su, x => x > 0)
      assert(contains(f, 1), "Filter 1")
      assert(contains(f, 2), "Filter 2")
      assert(!contains(f, -1), "Filter -1")
    }
  }

  test("forall") {
    new TestSets {
      val su = union(s1, union(s2, s3))
      assert(forall(su, x => x > 0), "All positive")
      assert(!forall(su, x => x > 1), "Not all above 1")
    }
  }

  test("exists") {
    new TestSets {
      val su = union(s1, union(s2, s3))
      assert(exists(su, x => x==1), "1 is in set.")
      assert(exists(su, x => x==2), "2 is in set.")
      assert(exists(su, x => x > 2), "x > 2 is in set")
      assert(!exists(su, x => x < 0), "x < 0 is not in set")
    }
  }

  test("map") {
    new TestSets {
      val su = union(s1, union(s2, s3))
      val isu = map(su, x => -x)
      assert(forall(isu, x => x < 0), "All negative")
    }
  }

  test("exists & filter: even and 3") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = singletonSet(6)
      val s8 = singletonSet(8)
      val evenAnd3 = union(s1, union(s5, union(s4, union(s6, union(s8, s3)))))
      val filtered = filter(evenAnd3, (x => x%2 == 0 || x == 3))
      printSet(intersect(filtered, x => x%2 != 0))
      assert(exists(filtered, x => x%2 != 0), "The set of all even numbers and 3 should contain an odd element, namely 3.")
    }
  }

  test("forall: {1,3,4,5,7,1000}") {
    val set = union(singletonSet(1), union(singletonSet(3), union(singletonSet(4), union(singletonSet(5), union(singletonSet(7), singletonSet(1000))))))
    assert(!forall(set, x => x < 5), "All elements in the set are not strictly less than 5.")
  }
}
