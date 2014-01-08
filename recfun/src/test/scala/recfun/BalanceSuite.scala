package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: '(if (zero? x) max (/ 1 x)' is inbalanced") {
    assert(!balance("(if (zero? x) max (/ 1 x)".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  test("balance: '(just an) example' is balanced") {
    assert(balance("(just an) example".toList))
  }

  test("balance: '()' is balanced") {
    assert(balance("()".toList))
  }

  test("balance: '(())' is balanced") {
    assert(balance("(())".toList))
  }

  test("balance: '())(' is inbalanced") {
    assert(!balance("())(".toList))
  }
}
