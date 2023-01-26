--- @class Parsec<T>: { parse: fun(input: string): (T | boolean, string | nil) }
local Parsec = {}
setmetatable(Parsec, Parsec.mt)
--- @class Parsec<T>
Parsec.mt = { __index = Parsec }
--- @generic T 
--- @param fun fun(input: string): (T | boolean, string | nil)
--- @return Parsec<T> 
function Parsec.new(fun)
  --- @class Parsec<T>
  return setmetatable({parse = fun}, Parsec.mt)
end

--- @generic T 
--- @generic U  
--- @param self Parsec<T> 
--- @param parser Parsec<U> 
--- @return Parsec<{left: T, right: U}>
function Parsec:combined(parser)
  return Parsec.new(
    function (input)
      local r1, rest1 = self.parse(input)
      if rest1 then
	local r2, rest2 = parser.parse(rest1)
	if rest2 then
	  return { left = r1, right = r2 }, rest2
	elseif r2 then
	  return true
	end
      elseif r1 then
	  return true
      end
      return false
    end
  )
end

--- @generic T 
--- @generic R 
--- @param self Parsec<T> 
--- @param f fun(i: T): R 
--- @return Parsec<R>
function Parsec:map(f)
  return Parsec.new(
    function (input)
      local r1, rest = self.parse(input)
      if rest then
	return f(r1), rest
      end
      return r1
    end
  )
end
--- @generic T 
--- @param self Parsec<any> 
--- @param parser Parsec<T> 
--- @return Parsec<T> 
function Parsec:andThen(parser)
  return Parsec.combined(self, parser):map(
    function (i)
      return i.right
    end
  )
end

--- @type Parsec<string>
Parsec.anyChar =
  Parsec.new(
    function (input)
      if input ~= "" then
	return string.sub(input, 1, 1), string.sub(input, 2, -1)
      end
      return false
    end
  )
return Parsec
