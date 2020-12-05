import akorda
import strutils
import unittest


suite "test case 1: simple calculator":
  setup:
    proc parseInt(prev: ParseResult[int], next: ParseResult[int]): ParseResult[int] =
      if isFailed(next):
        return next
      result.shallowCopy(next)
      result.parseObj = parseInt(next.val)

    proc compute(handle: proc (a, b: int): int): ParserHandler[int] =
      (
        proc (prev: ParseResult[int], next: ParseResult[int]): ParseResult[int] =
          if isFailed(next):
            return next
          result.shallowCopy(next)
          result.parseObj = handle(prev.parseObj, next.parseObj)
      )

    let intLit = repeat[int](acceptChar[int]({'0'..'9'}), 1) $- parseInt
    let opAdd = acceptChar[int]({'+'})
    let opSub = acceptChar[int]({'-'})
    let opMul = acceptChar[int]({'*'})
    let opDiv = acceptChar[int]({'/'})

    let litAdd = intLit & opAdd & intLit $- compute(proc(a,b: int): int = a + b)
    let litSub = intLit & opSub & intLit $- compute(proc(a,b: int): int = a - b)
    let litMul = intLit & opMul & intLit $- compute(proc(a,b: int): int = a * b)
    let litDiv = intLit & opDiv & intLit $- compute(proc(a,b: int): int = a div b)

    let parser = litMul | litDiv | litAdd | litSub | intLit

  test "add success":
    let expectSuccess = parser.parse("123+456")

    check(isAccepted(expectSuccess))
    check(expectSuccess.parseObj == 579)

  test "add failed":
    let expectFailed = parser.parse("a123+542")

    check(isFailed(expectFailed))

  test "subtract success":
    let expectSuccess = parser.parse("456-123")

    check(isAccepted(expectSuccess))
    check(expectSuccess.parseObj == 333)

  test "subtract failed":
    let expectFailed = parser.parse("542-a542")

    check(isFailed(expectFailed))

  test "multiply success":
    let expectSuccess = parser.parse("456*123")

    check(isAccepted(expectSuccess))
    check(expectSuccess.parseObj == 56088)

  test "multiply failed":
    let expectFailed = parser.parse("5a42*123")

    check(isFailed(expectFailed))

  test "divide success":
    let expectSuccess = parser.parse("456/123")

    check(isAccepted(expectSuccess))
    check(expectSuccess.parseObj == 3)

  test "divide failed":
    let expectFailed = parser.parse("1213/5a42")

    check(isFailed(expectFailed))
