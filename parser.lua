local lunarp = {}

local expr = require("expr")
--- @class Parser<T>: { run: fun(input: string): (T | boolean, string | nil) }
local parser = { expr = {} }
local parsermt = { __index = parser }

local helpers = require 'helpers'
local internals = {}

--- @generic T
--- @param fun fun(input: string): (T | boolean, string | nil) 
--- @return Parser<T>
function parser:new(fun) --> Parser<T> 
  local parse = { run = fun }
  setmetatable(parse, parsermt)
  return parse
end



--- @generic T
--- @generic U
--- @param self Parser<T> 
--- @param p Parser<U>
--- @return Parser<{ left: T, right: U }>
function parser:combined(p)
  if getmetatable(p) ~= parsermt then
    error("combined only works with parsers")
  end
  return parser:new(
    function (input)
      local r1, rest = self.run(input)
      if rest then
	local r2, newRest = p.run(rest)
	if newRest then
	  return {left = r1, right = r2}, newRest
	end
	return r2
      end
      return r1
    end
  )
end
--- @generic T
--- @generic U
--- @param self Parser<T>
--- @param p Parser<U>
--- @return Parser<U>
function parser:andThen(p)
  return self:combined(p):map(
    function (i)
      local v = i.right
      return v
    end
  )
end
--- @generic T
--- @generic U 
--- @param self Parser<T>
--- @param p Parser<U>
--- @return Parser<T>
function parser:thenDiscard(p)
  return self:combined(p):map(
    function (i)
      -- Map never gives me a nil
      local v = i.left
      return v
    end
  )
end
--- @generic T 
--- @generic R 
--- @param self Parser<T>
--- @param fun fun(T): R 
--- @return Parser<R>
function parser:map(fun)
  return parser:new(
    function (input)
      local v, rest = self.run(input)
      if rest then
	local nv = fun(v)
	return nv, rest
      end
      return v
    end
  )
end
--- @generic T 
--- @generic R 
--- @param self Parser<T>
--- @param fun fun(T): Parser<R>
--- @return Parser<R>
function parser:flatMap(fun)
  return parser:new(
    function (input)
      local v, rest = self.run(input)
      if rest then
	local np = fun(v)
	return np.run(rest)
      end
      return v
    end
  )
end
--- @generic T 
--- @generic V 
--- @param self Parser<T> 
--- @param p Parser<V> 
--- @return Parser<T[]>
function parser:untilParse(p)
  if getmetatable(p) ~= parsermt then
    error("combined only works with parsers")
  end
  return parser:new(
    function (input)
      local cur = input
      local values = {}
      while p.run(cur) == false do
	local r, rest = self.run(cur)
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
--- @generic T 
--- @param self Parser<T>
--- @return Parser<T[]>
-- remember to debug this stinky function
function parser:rep()
  return parser:new(
    function (input)
      local cur = input
      local v = nil
      local values = {}
      while true do
	local res
	v, res = self.run(cur)
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
--- @generic T 
--- @param self Parser<T> 
--- @return Parser<T[]> 
function parser:rep0()
  return self:rep():orElse(parser.pure({}))
end
parser.many = parser.rep0
--- @generic T 
--- @param self Parser<T> 
--- @param sep Parser<any>
--- @return Parser<T[]>
-- this one is smelly too
function parser:repSep0(sep)
  return parser:new(
    function(input)
      local cur = input
      local v = nil
      local values = {}
      while true do
	local res
	v, res = self.run(cur)
	if not res then
	  break
	end
	table.insert(values, v)
	cur = res
	local _, res2 = sep.run(cur)
	if not res2 then
	  return values, cur
	end
      end
      -- allows for empty + ending with an extra seperator
      return values, cur
    end
  )
end
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
--- @generic T 
--- @param self Parser<T>
--- @param start Parser<any>
--- @param ending Parser<any>
--- @return Parser<T>
function parser:between(start, ending)
  if getmetatable(start) ~= parsermt or getmetatable(ending) ~= parsermt then
    error("between only works with parsers, got" .. getmetatable(start))
  end
  return start:andThen(self):thenDiscard(ending)
end
--- @generic T 
--- @generic U 
--- @param self Parser<T>
--- @param p Parser<U>
--- @return Parser<T | U>
function parser:orElse(p)
  if getmetatable(p) ~= parsermt then
    error("orElse only works with parsers")
  end
  return parser:new(
    function (input)
      local r, rest = self.run(input)
      if rest == nil then
	if r then
	  return true
	else
	  return p.run(input)
	end
      else
	return r, rest
      end
    end
  )
end
--- @generic T 
--- @param self Parser<T>
--- @return Parser<T>
-- Enables backtracking on this parser.
function parser:attempted() --> Parser<T>
  return parser:new(
    function (input)
      local r, rest = self.run(input)
      if not rest then
	return false
      else
	return r, rest
      end
    end
  )
end
--- @generic T 
--- @param self Parser<T> 
--- @return Parser<nil>
function parser:optional() --> Parser<nil>
  return parser:new(
    function (input)
      local r, rest = self.run(input)
      if not rest then
	if not r then
	  return nil, input
	end
      else
	return nil, rest
      end
      return true
    end
  )
end
--- @generic T 
--- @generic R 
--- @param self Parser<T>
--- @param value R 
--- @return Parser<R>
function parser:as(value)
  return self:map(function (_) return value end)
end
--- @generic T 
--- @generic A 
--- @param self Parser<T>
--- @param k A 
--- @param f fun(accum: A, elem: T): A
function parser:foldLeft(k, f)
  return self:rep0():map(function (s) return helpers.foldLeft(s, k, f) end)
end
--- @generic T
--- @param value T 
--- @return Parser<T>
local function pure(value)
  return parser:new(
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
    function (ops, x)
      return helpers.foldRight(ops, x, function (f, v) return f(v) end)
    end
  ):ap(p)
end
parser.prefix = prefix
local function takeWhile(fun)
  return parser:new(
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
local anyChar =
  parser:new(
    function (input)
      if input ~= "" then
	return string.sub(input, 1, 1), string.sub(input, 2, -1)
      end
      return false
    end
  )
parser.anyChar = anyChar
local function charWhere(fun)
  return parser:new(
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
  return charWhere(function (c2) return string.find(str, c2) ~= nil end)
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

-- A parser of type T is a function of type `string -> (T, string) or bool`
-- It returns a parsed part and the remainder, or whether it consumed input.
--- @generic T 
--- @param p Parser<T>
--- @return Parser<T>
local function lexeme(p)
  return parser:new(function (input)
    local t, rest = p.run(input)
    if rest then
      return t, ltrim(rest)
    else
      return t
    end
  end)
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
local function keyword(word) --> Parser<nil>
  return lexeme(parser:new(function (input)
    local daWord, rest = stake(input, string.len(word))
    if word == daWord then
      return nil, rest
    end
    return false
  end))
end

parser.expr.keyword = keyword

local function softKeyword(word) --> Parser<nil>
  return lexeme(parser:new(function (input)
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
     charWhere(inLetterRange):combined(takeWhile(function (c)
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
  lexeme(parser:new(
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
  return string.find(validChars, c) ~= nil
end
local operator =
  lexeme(takeWhile(symbolFun))
parser.expr.operator = operator
local function userDefinedOperator(startsWith)
  local startParser = nil
  if type(startsWith) == "function" then
    startParser = charWhere(startsWith)
  else
    startParser = charIn(startsWith)
  end
  return startParser:combined(takeWhile(symbolFun)):map(
    function (c)
      return c.left .. c.right
    end
  )
end
local symbol = identifier:orElse(operator)
--[==[
local function braces(parser)
  return parser:between(keyword('{'), keyword('}'))
end
--]==]
local function parens(p)
  return p:between(keyword('('), keyword(')'))
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
    -- :orElse(parens(exprFun()))
end
-- FIXME LATER
local function unary()
  return prefix(charWhere(function (c) return string.find("~-+!", c) ~= nil end):map(
    function (s)
      return function(e)
	return expr.new("unary", s, e)
      end
    end
  ), primary())
end
parser.unary = unary
-- TODO later
local function call()
  return primary()
end
local function otherOps()
  return operatorFn(call(), userDefinedOperator(function (c) return not inLetterRange(c) and
    string.find("*/%+-:<>=!&^|$_", c) == nil end))
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
  return primary().run(input)
end

lunarp.parsec = parser
lunarp.intls = internals
return lunarp
