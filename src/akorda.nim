type
  ParseResult*[T] = object of RootObj
    success: bool
    pos: Natural
    length: Natural
    val: string
    parseObj: T

  Parser*[T] = proc(source: string, index: Natural): ParseResult[T]
  ParserHandler*[T] = proc(res: ParseResult[T]): ParseResult[T]

proc isFailed*[T](res: ParseResult[T]): bool = not res.success
proc isAccepted*[T](res: ParseResult[T]): bool = res.success

proc acceptChar*[T](acceptables: set[char]): Parser[T] =
  (
    proc (source: string, index: Natural): ParseResult[T] =
      if source.len-1 <= index:
        ParseResult[T](success: false, pos: index)
      elif source[index] in acceptables:
        ParseResult[T](success: true, pos: index, length: 1, val: source[index..index+1])
      else:
        ParseResult[T](success: false, pos: index)
  )

template `<<`*[T](parser: Parser[T], handler: ParserHandler[T]): Parser[T] =
  (proc (source: string, index: Natural): ParseResult[T] = handler(parser(source, index)))

template `|`*[T](left: Parser[T], right: Parser[T]): Parser[T] =
  (
    proc (source: string, index: Natural): ParseResult[T] =
      let leftResult = left(source, index)
      if isAccepted(leftResult):
        return leftResult
      right(source, index)
  )

template `&`*[T](left: Parser[T], right: Parser[T]): Parser[T] =
  (
    proc (source: string, index: Natural): ParseResult[T] =
      let res = left(source, index)
      if isFailed(res):
        return res
      right(source, index + res.length)
  )

template repeat*[T](target: Parser[T], least: Natural = 0, maximum: Natural = high(int)): Parser[T] =
  (
    proc (source: string, index: Natural): ParseResult[T] =
      var idx = index
      var parsed = ParseResult[T](
        success: true,
        pos: index)

      for i in 0..maximum:
        let res = target(source, idx)
        if isFailed(res):
          if i >= least:
            return parsed
          return res
        parsed.length = idx - index
        parsed.val = source[index..idx+1]
        parsed.parseObj = res.parseObj
        idx += res.length
  )


when isMainModule:
  import strutils
  proc parseInt(res: ParseResult[int]): ParseResult[int] =
    result.shallowCopy(res)
    result.parseObj = parseHexInt(res.val)

  let parser = repeat[int](acceptChar[int]({'a'..'f', '0'..'9'}), 1) << parseInt
  let res = parser("123", 0)
  echo $isAccepted(res)
  echo $res.parseObj
