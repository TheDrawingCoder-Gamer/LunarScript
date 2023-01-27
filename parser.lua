local lunarp = {}

local Expr = require("expr")
local Stmt = require "stmt"
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
function LazyParser.null()
  return setmetatable({}, LazyParser.mt)
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

--- @param ... Parser<any> 
--- @return Parser<any[]>
function parser.together(...)
  local args = {...}
  return parser.new(
    function (input)
      local out = {}
      local res = nil
      local cur = input
      while #args ~= 0 do
	res, cur = table.remove(args, 1):run(cur)
	if not cur then
	  return res
	end
	table.insert(out, res)
      end
      return out, cur
    end
  )
end
--- @param ...Parser<any> 
--- @return LazyParser<any[]>
LazyParser.together = LazyParser.thunkify(parser.together)
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
  return self:sepBy(sep):thenDiscard(sep:optional())
end
parser.prototype.sepBy = parser.sepBy
parser.prototype.sepBy1 = parser.sepBy1
parser.prototype.sepByEnd = parser.sepByEnd

--- Succeeds when this parser would fail.
--- Note parser:invert():invert() ~= parser
function parser:invert()
  return parser.new(
    function (input)
      local _, rest = self:run(input)
      if not rest then
	return nil, input
      else
	return false
      end
    end
  )
end
parser.prototype.invert = parser.invert
function parser:notFollowedBy(p)
  return self:thenDiscard(p:invert())
end
parser.prototype.notFollowedBy = parser.notFollowedBy
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

function parser.failure(consumed)
  return parser.new(
    function (_)
      return consumed
    end
  )
end
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

--- @param ... Parser<any> | LazyParser<any>
--- you do it :  )
--- @return Parser<{tag: integer, value: any}>
function parser.enum(...)
  local args = {...}
  local function helper(n)
    if n > #args then
      return parser.failure(false)
    end
    return args[n]:map(
      function (i)
	return { tag = n, value = i }
      end
    ):orElse(helper(n + 1))
  end
  return helper(1)
end
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
function parser:optional()
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

function parser.string(str)
  return parser.new(
    function (input)
      local word, rest = internals.stake(input, string.len(str))
      if word == str then
	return nil, rest
      end
      return false
    end
  )
end
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

parser.hardKeywords = { "if", "when", "else", "fun", "class", "object"}
parser.hardOperators = { "->", "=>", "<-", "=", ":", ":=" }
-- TODO: What about colon syntax?
local validChars = "!#$%&*+-/:<=>?@^_|~\\"
local function symbolFun(c)
  return string.find(validChars, c, 1, true) ~= nil
end
local function contains(ls, item)
  for _, v in ipairs(ls) do
    if v == item then
      return true
    end
  end
  return false
end
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
	  return Expr.new("operator", sym, accum, ri)
	end
      )
    end
  )
end
internals.operator = operatorFn
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

local function validLetter(c)
  return isDigit(c) or inLetterRange(c)
end

local function softKeyword(word) --> Parser<nil>
  if (contains(parser.hardKeywords, word)) then
    return parser.failure(false)
  else
    return lexeme(parser.string(word):notFollowedBy(charWhere(validLetter)))
  end
end
local function softOperator(op)
  if (contains(parser.hardOperators, op)) then
    return parser.failure(false)
  else
    return lexeme(parser.string(op):notFollowedBy(charWhere(symbolFun)))
  end
end
parser.expr.softKeyword = softKeyword
parser.expr.softOperator = softOperator


local lameIdentifier =
  lexeme(
     charWhere(inLetterRange):combined(parser.takeWhile0(function (c)
	return inLetterRange(c) or isDigit(c)
     end)):flatMap(
      function (i)
	local c = i.left
	local s = i.right
	local r = c .. s
	if contains(parser.hardKeywords, r) then
	  return parser.failure(false)
	else
	  return parser.pure(r)
	end
      end
     )
  )
parser.lameIdentifier = lameIdentifier
--- Fancy identifier is specifically to allow keywords
parser.fancyIdentifier =
  lexeme(
    parser.takeWhile(
      function (c)
	return c ~= "`" and (inLetterRange(c) or isDigit(c) or symbolFun(c) or c == ' ')
      end
    ):between(char"`", char"`")
  )
parser.identifier = parser.lameIdentifier:orElse(parser.fancyIdentifier)
local identifier = parser.identifier
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
  return lexeme(startParser:combined(parser.takeWhile0(symbolFun)):flatMap(
    function (c)
      local res = c.left .. c.right
      if (contains(parser.hardOperators, res)) then
	return parser.failure(true)
      else
	return parser.pure(c.left .. c.right)
      end
    end
  ))
end
parser.userDefinedOperator = userDefinedOperator
local symbol = identifier:orElse(operator)
local function braces(p)
  return p:between(lexeme(char'{'),lexeme(char'}'))
end
local function brackets(p)
  return p:between(lexeme(char'['), lexeme(char']'))
end 
--- @generic T 
--- @param p LazyParser<T> 
--- @return LazyParser<T>
local function parens(p)
  return p:between(lexeme(char"("), lexeme(char")"))
end
parser.expr.parens = parens

local semicolon = lexeme(char";")
-- Expr : ) 
local expr = LazyParser.null()
--- @type LazyParser<Stmt>
local stmt = LazyParser.null()
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
	return Expr.new(Expr.EXPRS.number, integral .. ending)
    end
  ))
local ifExpr =
  LazyParser.new(function()
  return softKeyword("if"):andThen(LazyParser.together(
    parens(expr),
    expr,
    softKeyword("else"):andThen(expr):optional()
  )):map(
    function (e)
      return Expr.new(Expr.EXPRS.ifExpr, unpack(e))
    end
  )
end)
--- block returns expr or nil : )
local block =
  LazyParser.new(
    function()
      return braces(
	stmt:many():combined(expr:optional())
      ):map(
	function (e)
	  return Expr.new(Expr.EXPRS.block, e.left, e.right)
	end
      )
    end
  )
local tableElem =
  LazyParser.new(
    function()
      return expr:thenDiscard(softOperator "->"):combined(expr):map(
	function (e)
	  return { key = e.left, value = e.right }
	end
      ):attempted():orElse(expr:map(function (e) return { value = e } end))
    end
  )

local tableExpr =
  brackets(
   tableElem:sepByEnd(lexeme(charIn",;"))
 ):map(
    function (ls)
      -- don't unpack
      return Expr.new(Expr.EXPRS.table, ls)
    end
 )
 local lambda =
  parens(lameIdentifier:sepBy(lexeme(char",")))
  :thenDiscard(softOperator "=>"):combined(expr):map(
    function (e)
      return Expr.new(Expr.EXPRS.lambda, e.left, e.right)
    end
  )

--- TODO: Hook into Stmt.Expr(Expr.If(...))
local when =
  LazyParser.new(function()
  return softKeyword("if"):andThen(LazyParser.together(
    parens(expr),
    stmt,
    softKeyword("else"):andThen(stmt):optional()
  )):map(
    function (e)
      return Stmt.new(Stmt.STMTS.whenStmt, unpack(e))
    end
  ):attempted()
end)
local returnStmt =
  LazyParser.new(function ()
    return softKeyword("return"):andThen(expr:sepBy(lexeme(char","))):map(
      function (e)
	return Stmt.new(Stmt.STMTS.returnStmt, e)
      end
    )
  end):thenDiscard(semicolon)
local assignStmt =
  LazyParser.new(function()
    return expr:thenDiscard(softOperator "="):combined(expr):flatMap(
      function (e)
	local target = e.left
	if target.name == Expr.EXPRS.identifier or target.name == Expr.EXPRS.field then
	  return pure(Stmt.new(Stmt.STMTS.assign, target, e.right))
	else
	  return parser.failure(true)
	end
      end
    )
  end):thenDiscard(semicolon)
local declare =
  LazyParser.new(function()
    return identifier:thenDiscard(softOperator ":="):combined(expr):map(
      function (e)
	return Stmt.new(Stmt.STMTS.declare, e.left, e.right)
      end
    )

  end):thenDiscard(semicolon)

local breakStmt = softKeyword("break"):as(Stmt.new(Stmt.STMTS.breakStmt)):thenDiscard(semicolon)
local blockStmt = LazyParser.new(
    function ()
      return block:map(
      function (e)
	  return Stmt.new(Stmt.STMTS.expr, e)
      end)
    end
)
local exprStmt =
  LazyParser.new(
  function()
    return expr:thenDiscard(semicolon):map(
      function(e)
	return Stmt.new(Stmt.STMTS.expr, e)
      end
    )
  end)
stmt = when:orElse(returnStmt):orElse(assignStmt):orElse(breakStmt):orElse(blockStmt):orElse(declare):orElse(exprStmt)
--- @return Parser<Expr>
local function primary()
  return lunarp.number:attempted()
    :orElse(stringParser:attempted():map(function (str) return Expr.new(Expr.EXPRS.string, str) end ))
    :orElse(softKeyword("true"):as(Expr.new(Expr.EXPRS.boolean, true)))
    :orElse(softKeyword("false"):as(Expr.new(Expr.EXPRS.boolean, false)))
    :orElse(softKeyword("nil"):as(Expr.new(Expr.EXPRS.null)))
    :orElse(ifExpr)
    :orElse(tableExpr)
    :orElse(lambda)
    :orElse(block)
    :orElse(symbol:map(function (sym) return Expr.new(Expr.EXPRS.identifier, sym) end))
    :orElse(parens(expr):map(function (e) return Expr.new(Expr.EXPRS.atom, e) end))
end

local function call()
  return primary():flatMap(
    function (e)
      return parser.enum(
      lexeme(char"."):andThen(symbol):attempted(),
      parens(
	 expr:sepBy(lexeme(char(",")))
      ):attempted(),
      softOperator(":"):andThen(symbol):combined(parens(expr:sepBy(lexeme(char(","))))):attempted(),
      brackets(expr)
      ):foldLeft(e,
	function (accum, elem)
	  if elem.tag == 1 then
	    return Expr.new(Expr.EXPRS.field, accum, elem.value)
	  elseif elem.tag == 2 then
	    return Expr.new(Expr.EXPRS.call, accum, elem.value)
	  elseif elem.tag == 3 then
	    -- Instance is a combination of call and field. It's a special field access immediately followed by a call
	    return Expr.new(Expr.EXPRS.instance, accum, elem.value.left, elem.value.right)
	  elseif elem.tag == 4 then
	    return Expr.new(Expr.EXPRS.index, accum, elem.value)
	  end
	end
      )
    end
  )
end
local function unary()
  return prefix(charWhere(function (c) return string.find("#~-+!", c, 1, true) ~= nil end):map(
    function (s)
      return function(e)
	return Expr.new(Expr.EXPRS.unary, s, e)
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

expr.get = letters

--[[
local parseFunDecl =
  softKeyword("fun"):andThen(parser.together(
    identifier,
    --- NO FUNNY BUSINESS!
    parens(parser.lameIdentifier:repSep(lexeme(char","))),

  ))
]]--
-- A class is a metatable with a companion table that has a constructor.
-- No inheritance yet : ) 
--local parseClassDecl = 
--  softKeyword("class"):andThen(identifier):combined(
--  )
-- An object is shorthand for a table with functions. 
--[[
local parseObjectDecl =
  softKeyword("object"):andThen(parser.together(
    identifier,
    -- TODO: DEFS
    braces(anyChar)
  ))
  ]]--
function lunarp.parse(input)
  return stmt:rep():run(input)
end

lunarp.parsec = parser
parser.block = block
lunarp.intls = internals
return lunarp
