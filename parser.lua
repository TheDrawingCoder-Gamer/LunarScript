local lunarp = {}

local expr = require("expr")
local parser = { expr = {} }
--- @class Parser<T>: { run: fun(self: Parser<T>, input: string): (T | boolean, string | nil) }
parser.prototype = {}
local parsermt = { __index = parser.prototype }
local LazyParser = {}
--- @class LazyParser<T>: { get: fun(): Parser<T> } & Parser<T>
LazyParser.prototype = {}
-- Prototypes only include functions
LazyParser.mt = { __index = function (_, name) return LazyParser.prototype[name] or LazyParser.thunkify(parser.prototype[name]) end }
parser.lazy = LazyParser
local helpers = require 'helpers'
local internals = {}
local Either = require'either'
local unpack = table.unpack or unpack
--- @generic T
--- @param fun fun(input: string): (T | boolean, string | nil) 
--- @return Parser<T>
function parser.new(fun) --> Parser<T> 
  local parse = { run = function (_, i) return fun(i) end }
  setmetatable(parse, parsermt)
  return parse
end


--- @generic T 
--- @param lazy fun(): Parser<T>
--- @return LazyParser<T> 
function LazyParser.new(lazy)
  return setmetatable({ get = lazy }, LazyParser.mt)
end
--- @generic T 
--- @param self Parser<T> | LazyParser<T> 
--- @return Parser<T>
function LazyParser.asStrict(self)
  if getmetatable(self) == parsermt then
    --- @cast self Parser<unknown>
    return self
  else
    return self.get()
  end
end
function LazyParser.asLazy(self)
  if getmetatable(self) == parsermt then
    return LazyParser.now(self)
  else
    return self
  end
end
--- @generic T 
--- @param strict Parser<T> 
--- @return LazyParser<T> 
-- A parser wrapped in a lazy parser. This means it is not lazy.
function LazyParser.now(strict)
  return setmetatable({ get = function() return strict end }, LazyParser.mt)
end
--- @generic T 
--- @param fun fun(...: any): Parser<T>
--- @return fun( ...: any): LazyParser<T>
function LazyParser.thunkify(fun)
  return function (...)
    local args = {...}
    return LazyParser.new(
      function ()
	return fun(unpack(args))
      end
    )
  end
end
function LazyParser.prototype:run(input)
  return self.get():run(input)
end
--- @generic T
--- @generic U
--- @param l Parser<T>
--- @param r Parser<U> | fun(): Parser<U>
--- @return Parser<{ left: T, right: U }>
function parser.combined(l, r)
  return parser.new(
    function (input)
      local r1, rest = l:run(input)
      if rest then
	if type(r) == "function" then
	  r = r()
	end
	local r2, newRest = r:run(rest)
	if newRest then
	  return {left = r1, right = r2}, newRest
	end
	return r2
      end
      return r1
    end
  )
end
parser.prototype.combined = parser.combined
--- @generic T
--- @generic U
--- @param l Parser<T>
--- @param r Parser<U> | fun(): Parser<U>
--- @return Parser<U>
function parser.andThen(l, r)
  return parser.combined(l, r):map(
    function (i)
      local v = i.right
      return v
    end
  )
end
parser.prototype.andThen = parser.andThen
--- @generic T
--- @generic U 
--- @param l Parser<T>
--- @param r Parser<U> | fun(): Parser<U>
--- @return Parser<T>
function parser.thenDiscard(l, r)
  return parser.combined(l, r):map(
    function (i)
      -- Map never gives me a nil
      local v = i.left
      return v
    end
  )
end
parser.prototype.thenDiscard = parser.thenDiscard
--- @generic T 
--- @generic R 
--- @param self Parser<T> | LazyParser<T>
--- @param fun fun(T): R 
--- @return Parser<R>
function parser:map(fun)
  return parser.new(
    function (input)

      local v, rest = self:run(input)
      if rest then
	local nv = fun(v)
	return nv, rest
      end
      return v
    end
  )
end
parser.prototype.map = parser.map

LazyParser.prototype.map = function (self, fun)
  return LazyParser.new(
    function ()
      return parser.map(self, fun)
    end
  )
end
--- @generic T 
--- @generic R 
--- @param self Parser<T>
--- @param fun fun(T): Parser<R>
--- @return Parser<R>
function parser:flatMap(fun)
  return parser.new(
    function (input)
      local v, rest = LazyParser.asStrict(self):run(input)
      if rest then
	local np = fun(v)
	return LazyParser.asStrict(np):run(rest)
      end
      return v
    end
  )
end
parser.prototype.flatMap = parser.flatMap
--- @generic T 
--- @generic V 
--- @param self Parser<T> 
--- @param p Parser<V> 
--- @return Parser<T[]>
-- FIXME: will obviously break if parser returns false 
function parser:untilParse(p)
  return parser.new(
    function (input)
      local cur = input
      local values = {}
      while p:run(cur) == false do
	local r, rest = self:run(cur)
	if rest then
	  table.insert(values, r)
	  cur = rest
	else
	  return false
	end
      end
      if #values ~= 0 then
	return values, cur
      end
      return false
    end
  )
end

parser.prototype.untilParser = parser.untilParse
--- @generic T 
--- @param self Parser<T>
--- @return Parser<T[]>
-- remember to debug this stinky function
function parser:rep()
  return parser.new(
    function (input)
      local cur = input
      local v = nil
      local values = {}
      while true do
	local res
	v, res = self:run(cur)
	if not res then
	  break
	end
	cur = res
	table.insert(values, v)
      end
      if #values ~= 0 then
	return values, cur
      end
      return false
    end
  )
end
parser.prototype.rep = parser.rep
--- @generic T 
--- @param self Parser<T> 
--- @return Parser<T[]> 
function parser:rep0()
  return self:rep():orElse(parser.pure({}))
end
parser.prototype.many = parser.rep0
parser.prototype.rep0 = parser.rep0
--- @generic T 
--- @param self Parser<T> 
--- @param sep Parser<any>
--- @return Parser<T[]>
function parser:sepBy1(sep)
  return self:flatMap(
    function (x)
      return sep:andThen(self):many():map(function (r) return helpers.cons(x, r) end)
    end
  )
end
function parser:sepBy(sep)
  return self:sepBy1(sep):orElse(parser.pure {})
end
function parser:sepByEnd(sep)
  return self:sepBy(sep):thenDiscard(sep)
end
parser.prototype.sepBy = parser.sepBy
parser.prototype.sepBy1 = parser.sepBy1
parser.prototype.sepByEnd = parser.sepByEnd

--- @generic T 
--- @generic R 
--- @param self Parser<fun(i: T): R>
--- @param fa Parser<T> 
--- @return Parser<R>
function parser:ap(fa)
  return self:flatMap(
    function (f)
      return fa:map(f)
    end
  )
end
parser.prototype.ap = parser.ap

--- @generic T 
--- @generic R
--- @param self Parser<T>
--- @param ff Parser<fun(i: T): R> 
--- @return Parser<R>
function parser:reverseAp(ff)
  return self:flatMap(
    function (v)
      return ff:map(function (f) return f(v) end)
    end
  )
end
parser.prototype.reverseAp = parser.reverseAp
--- @generic L, R 
--- @param l Parser<L>
--- @param r Parser<R>
--- @return Parser<Either<L, R>>
function parser.sum(l, r)
  return parser.new(
    function (input)
      local res, rest = l:run(input)
      if rest == nil then
	if res then
	  return true
	else
	  local res2, rest2 = r:run(input)
	  return Either.Right(res2), rest2
	end
      else
	return Either.Left(res), rest
      end

    end
  )
end
parser.prototype.sum = parser.sum
--- @generic T 
--- @param self Parser<T> 
--- @param start Parser<any>
--- @param ending Parser<any>
--- @return Parser<T>
function parser:between(start, ending)
  return start:andThen(self):thenDiscard(ending)
end
parser.prototype.between = parser.between
--- @generic T 
--- @param start Parser<any> 
--- @param p fun(): Parser<T>  
--- @param ending Parser<any> 
--- @return fun(): Parser<T> 
function parser.lazyBetween(start, p, ending)
  return function()
    return start:andThen(p()):thenDiscard(ending)
  end
end
--- @generic T 
--- @generic U 
--- @param self Parser<T>
--- @param p Parser<U> | fun(): Parser<U>
--- @return Parser<T | U>
function parser:orElse(p)
  return parser.new(
    function (input)
      local r, rest = self:run(input)
      if rest == nil then
	if r then
	  return true
	else
	  if type(p) == "function" then
	    p = p()
	  end
	  return p:run(input)
	end
      else
	return r, rest
      end
    end
  )
end
parser.prototype.orElse = parser.orElse
function LazyParser.prototype:orElse(p)
  return LazyParser.new(
    function ()
      return self.get():orElse(p)
    end
  )
end
--- @generic T 
--- @param self Parser<T>
--- @return Parser<T>
-- Enables backtracking on this parser.
function parser:attempted() --> Parser<T>
  return parser.new(
    function (input)
      local r, rest = self:run(input)
      if not rest then
	return false
      else
	return r, rest
      end
    end
  )
end
parser.prototype.attempted = parser.attempted
--- @generic T 
--- @param self Parser<T> 
--- @return Parser<T | nil>
function parser:optional() --> Parser<nil>
  return parser.new(
    function (input)
      local r, rest = self:run(input)
      if not rest then
	if not r then
	  return nil, input
	end
      else
	return r, rest
      end
      return true
    end
  )
end
parser.prototype.optional = parser.optional
--- @generic T 
--- @generic R 
--- @param self Parser<T>
--- @param value R 
--- @return Parser<R>
function parser:as(value)
  return self:map(function (_) return value end)
end
parser.prototype.as = parser.as
--- @generic T 
--- @generic A 
--- @param self Parser<T>
--- @param k A 
--- @param f fun(accum: A, elem: T): A
function parser:foldLeft(k, f)
  return self:rep0():map(function (s) return helpers.foldLeft(s, k, f) end)
end
parser.prototype.foldLeft = parser.foldLeft
--- @generic T
--- @param value T 
--- @return Parser<T>
local function pure(value)
  return parser.new(
    function (input)
      return value, input
    end
  )
end
parser.pure = pure
--- @generic T 
--- @param op Parser<fun(i: T): T>
--- @param p Parser<T>
--- @return Parser<T>
local function prefix(op, p)
  return op:rep0():map(
    function (ops)
      return function(x)
	return helpers.foldRight(ops, x, function (f, v) return f(v) end)
      end
    end
  ):ap(p)
end
parser.prefix = prefix
--- @param fun fun(i: string): boolean 
--- @return Parser<string>
local function takeWhile(fun)
  return parser.new(
    function (input)
      local res = ""
      local cur = input
      while cur ~= "" and fun(string.sub(cur, 1, 1)) do
	res = res .. string.sub(cur, 1, 1)
	cur = string.sub(cur, 2, -1)
      end
      if res ~= "" then
	return res, cur
      end
      return false
    end
  )
end
parser.takeWhile = takeWhile
function parser.takeWhile0(fun)
  return takeWhile(fun):orElse(pure"")
end
local anyChar =
  parser.new(
    function (input)
      if input ~= "" then
	return string.sub(input, 1, 1), string.sub(input, 2, -1)
      end
      return false
    end
  )
parser.anyChar = anyChar
local function charWhere(fun)
  return parser.new(
    function (input)
      if input ~= "" and fun(string.sub(input, 1, 1)) then
	return string.sub(input, 1, 1), string.sub(input, 2, -1)
      end
      return false
    end
  )
end
parser.charWhere = charWhere
local function charIn(str)
  return charWhere(function (c2) return string.find(str, c2, 1, true) ~= nil end)
end
parser.charIn = charIn
local function char(c)
  return charWhere(function (c2) return c2 == c end)
end
parser.char = char
--- @param s string 
--- @return string
local function ltrim(s) --> string
  return s:match'^%s*(.*)' or ''
end

internals.ltrim = ltrim
--- @param input string 
--- @param n integer 
--- @return string, string
local function stake(input, n) --> string, string
  return string.sub(input, 1, n), string.sub(input, n + 1, -1)
end
internals.stake = stake

--- @generic T 
--- @param p Parser<T>
--- @return Parser<T>
local function lexeme(p)
  return p:attempted():thenDiscard(takeWhile(function (c) return string.find(c, "%s") ~= nil end):optional())
end
parser.expr.lexeme = lexeme
--- @param subOnes Parser<Expr>
--- @param symbol Parser<string>
--- @return Parser<Expr>
-- Parser<Expr>, Parser<String> -> Parser<Expr>
local function operatorFn(subOnes, symbol)
  return subOnes:flatMap(
    function (l)
      return symbol:combined(subOnes):foldLeft(l,
	function (accum, elem)
	  local sym = elem.left
	  local ri = elem.right
	  -- Operator, name, left, right
	  return expr.new("operator", sym, accum, ri)
	end
      )
    end
  )
end
internals.operator = operatorFn
local function keyword(word) --> Parser<nil>
  return lexeme(parser.new(function (input)
    local daWord, rest = stake(input, string.len(word))
    if word == daWord then
      return nil, rest
    end
    return false
  end))
end

parser.expr.keyword = keyword

local function softKeyword(word) --> Parser<nil>
  return lexeme(parser.new(function (input)
    local daWord, rest = stake(input, string.len(word))
    if word == daWord and ltrim(rest) == rest then
      return nil, rest
    end
    return false
  end))
end
parser.expr.softKeyword = softKeyword
local function inLetterRange(c)
  local b = string.byte(c)
  return (b >= string.byte('a') and b <= string.byte('z'))
	  or (b >= string.byte('A') and b <= string.byte('Z'))
	  or (b == string.byte('_'))

end

local function isDigit(c)
  local b = string.byte(c)
  return b >= string.byte('0') and b <= string.byte('9')
end
local identifier =
  lexeme(
     charWhere(inLetterRange):combined(parser.takeWhile0(function (c)
	return inLetterRange(c) or isDigit(c)
     end)):map(
      function (i)
	local c = i.left
	local s = i.right
	return c .. s
      end
     )
  )
parser.identifier = identifier
local stringParser =
  lexeme(parser.new(
    function (input)
      local cur = input
      local escaped = false
      local c = ""
      -- string will be a string that can be outputed and be valid lua LOL
      local str = ""
      while true do
	if string.len(cur) == 0 then
	  break
	end
	c = string.sub(cur, 1, 1)
	if c == '"' and not escaped then
	  return str, cur
	end
	cur = string.sub(cur, 2, -1)
	if c == '\\' then
	  escaped = true
	elseif escaped then
	  if c == 'a' then
	    str = str .. '\\a'
	  elseif c == 'b' then
	    str = str .. '\\b'
	  elseif c == 'f'  then
	    str = str .. '\\f'
	  elseif c == 'n' then
	    str = str .. '\\n'
	  elseif c == 'r' then
	    str = str .. '\\r'
	  elseif c == 't' then
	    str = str .. '\\t'
	  elseif c == 'v' then
	    str = str .. '\\v'
	  elseif c == '\\' then
	    str = str .. '\\\\'
	  elseif c == '"' then
	    str = str .. '\\"'
	  elseif c == "'" then
	    str = str .. "\\'"
	  elseif c == '[' then
	    str = str .. '\\['
	  elseif c == ']' then
	    str = str .. '\\]'
	  elseif isDigit(c) then
	    local d = c .. string.sub(cur, 1, 2)
	    cur = string.sub(cur, 3, -1)
	    str = str .. '\\' .. d
	  else
	    return true
	  end
	  escaped = false
	else
	  if c == '\n' then
	    str = str .. '\\n'
	  else
	    str = str .. c
	  end
	end
      end
      return str, cur
    end
  ):between(char('"'), char('"')))
local validChars = "!#$%&*+-/:<=>?@^_|~\\"
local function symbolFun(c)
  return string.find(validChars, c, 1, true) ~= nil
end
local operator =
  lexeme(takeWhile(symbolFun))
parser.expr.operator = operator
--- @param startsWith string | fun(i: string):boolean
--- @return Parser<string>
local function userDefinedOperator(startsWith)
  --- @type Parser<string>
  local startParser
  if type(startsWith) == "function" then
    --- @cast startsWith fun(i: string): boolean
    startParser = charWhere(startsWith)
  else
    --- @cast startsWith string
    startParser = charIn(startsWith)
  end
  return lexeme(startParser:combined(parser.takeWhile0(symbolFun)):map(
    function (c)
      return c.left .. c.right
    end
  ))
end
parser.userDefinedOperator = userDefinedOperator
local symbol = identifier:orElse(operator)
--[==[
local function braces(parser)
  return parser:between(keyword('{'), keyword('}'))
end
--]==]
--- @generic T 
--- @param p LazyParser<T> 
--- @return LazyParser<T>
local function parens(p)
  return p:between(lexeme(char"("), lexeme(char")"))
end
parser.expr.parens = parens
-- Expr : ) 
local exprFun
--- @type Parser<Expr>
lunarp.number =
  lexeme(
    takeWhile(isDigit)
    :combined(char("."):andThen(takeWhile(isDigit)):attempted():orElse(pure("")))
    :map(
      function (i)
	local integral = i.left
	local floating = i.right
	local ending = ""
	if string.len(floating) ~= 0 then
	  ending = "." .. floating
	end
	return expr.new("number", integral .. ending)
    end
  ))
--- @return Parser<Expr>
local function primary()
  return lunarp.number:attempted()
    :orElse(stringParser:attempted())
    :orElse(softKeyword("true"):as(expr.new("boolean", true)))
    :orElse(softKeyword("false"):as(expr.new("boolean", false)))
    :orElse(softKeyword("nil"):as(expr.new("nil")))
    :orElse(symbol:map(function (sym) return expr.new("identifier", sym) end))
    :orElse(parens(LazyParser.new(exprFun)))
  end
local function call()
  return primary():flatMap(
    function (e)
      return parens(
	LazyParser.new(exprFun):sepByEnd(lexeme(char(",")))
      ):sum(
	lexeme(char"."):andThen(symbol)
      ):foldLeft(e,
	function (accum, elem)
	  return elem:condense(
	    function (l)
	      return expr.new("call", accum, l)
	    end,
	    function (r)
	      print(r)
	      return expr.new("field", accum, r)
	    end
	  )
	end
      )
    end
  )
end
local function unary()
  return prefix(charWhere(function (c) return string.find("~-+!", c, 1, true) ~= nil end):map(
    function (s)
      return function(e)
	return expr.new("unary", s, e)
      end
    end
  ), call())
end
parser.unary = unary

local function otherOps()
  return operatorFn(unary(), userDefinedOperator(function (c) return not inLetterRange(c) and symbolFun(c) and
    string.find("*/%+-:<>=!&^|$_", c, 1, true) == nil end))
end
local function mulDivMod()
  return operatorFn(otherOps(), userDefinedOperator("*/%"))
end
local function plusMin()
  return operatorFn(mulDivMod(), userDefinedOperator("+-"))
end
local function colon()
  return operatorFn(plusMin(), userDefinedOperator(":"))
end
local function greatLess()
  return operatorFn(colon(), userDefinedOperator("<>"))
end
local function nequal()
  return operatorFn(greatLess(), userDefinedOperator("!="))
end
local function andOp()
  return operatorFn(nequal(), userDefinedOperator("&"))
end
local function xorOp()
  return operatorFn(andOp(), userDefinedOperator("^"))
end
local function orOp()
  return operatorFn(xorOp(), userDefinedOperator("|"))
end
local function dollar()
  return operatorFn(orOp(), userDefinedOperator("$"))
end
local function letters()
  return operatorFn(dollar(), identifier)
end


exprFun = function() return letters() end
--local parseFunDecl = 
--  softKeyword("fun"):andThen()
-- A class is a metatable with a companion table that has a constructor.
-- No inheritance yet : ) 
--local parseClassDecl = 
--  softKeyword("class"):andThen(identifier):combined(
--  )
-- An object is shorthand for a table with functions. 
--local parseObjectDecl = 
--  softKeyword("object"):andThen(identifier)

function lunarp.parse(input)
  return exprFun():run(input)
end

lunarp.parsec = parser
lunarp.intls = internals
return lunarp
