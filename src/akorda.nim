type
  ParseResult*[T] = object of RootObj
    success: bool
    pos*: Natural
    length*: Natural
    val*: string
    parseObj*: T

  Parser*[T] = proc(source: string, index: Natural, prev: ParseResult[T]): ParseResult[T]
  ParserHandler*[T] = proc(prev: ParseResult[T], next: ParseResult[T]): ParseResult[T]

proc isFailed*[T](res: ParseResult[T]): bool = not res.success
proc isAccepted*[T](res: ParseResult[T]): bool = res.success

template log*(name: string): ParserHandler[int] =
  (
    proc(prev: ParseResult[int], next: ParseResult[int]): ParseResult[int] =
      echo "parser: ", name
      echo "  pos: ", $next.pos
      echo "  success: ", $next.success
      echo "  length: ", $next.length
      echo "  text: ", next.val
      echo "  parse object: ", $next.parseObj
      next
  )

template acceptChar*[T](acceptables: set[char]): Parser[T] =
  (
    proc (source: string, index: Natural, prev: ParseResult[T]): ParseResult[T] =
      if source.len-1 < index:
        ParseResult[T](success: false, pos: index)
      elif source[index] in acceptables:
        ParseResult[T](success: true, pos: index, length: 1, val: source[index..index], parseObj: prev.parseObj)
      else:
        ParseResult[T](success: false, pos: index)
  )

template `$-`*[T](parser: Parser[T], handler: ParserHandler[T]): Parser[T] =
  (proc (source: string, index: Natural, prev: ParseResult[T]): ParseResult[T] = handler(prev, parser(source, index, prev)))

template `|`*[T](left: Parser[T], right: Parser[T]): Parser[T] =
  (
    proc (source: string, index: Natural, prev: ParseResult[T]): ParseResult[T] =
      let leftResult = left(source, index, prev)
      if isAccepted(leftResult):
        return leftResult
      right(source, index, prev)
  )

template `&`*[T](left: Parser[T], right: Parser[T]): Parser[T] =
  (
    proc (source: string, index: Natural, prev: ParseResult[T]): ParseResult[T] =
      let res = left(source, index, prev)
      if isFailed(res):
        return res
      result = right(source, index + res.length, res)
      result.length += res.length
  )

template repeat*[T](target: Parser[T], least: Natural = 0, maximum: Natural = high(int)): Parser[T] =
  (
    proc (source: string, index: Natural, prev: ParseResult[T]): ParseResult[T] =
      var idx = index
      var parsed = ParseResult[T](
        success: true,
        pos: index)
      var prevR: ParseResult[T] = prev

      for i in 0..maximum:
        let res = target(source, idx, prevR)
        if isFailed(res):
          if i >= least:
            return parsed
          return res
        idx += res.length
        parsed.length = idx - index
        parsed.val = source[index..idx-1]
        parsed.parseObj = res.parseObj
        prevR = res
  )

proc parse*[T](parser: Parser[T], source: string): ParseResult[T] =
  result = parser(source, 0, ParseResult[T](success: true, pos: 0, length: 0, val: ""))
  if result.length != source.len:
    result = ParseResult[T](success: false, pos: result.length)
