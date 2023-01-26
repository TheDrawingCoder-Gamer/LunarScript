
local Either = {}
--- @class Either<L, R>: { isRight: boolean, value: L | R}
Either.prototype = {}
Either.mt = { __index = Either.prototype }
function Either.Left(value)
  return setmetatable({isRight = false, value = value}, Either.mt)
end
function Either.Right(value)
  return setmetatable({isRight = true, value = value}, Either.mt)
end

--- @generic L 
--- @generic R
--- @generic LR
--- @generic RR 
--- @param self Either<L, R> 
--- @param ifLeft fun(l:L): LR 
--- @param ifRight fun(r:R): RR 
--- @return Either<LR, RR>
function Either.prototype:bimap(ifLeft, ifRight)
  if self.isRight then
    return Either.Right(ifRight(self.value))
  else
    return Either.Left(ifLeft(self.value))
  end
end
--- @generic L, R, RR
--- @param self Either<L, R>
--- @param fun fun(r: R): RR 
--- @return Either<L, RR>
function Either.prototype:map(fun)
  return self:bimap(function (s) return s end, fun)
end
-- Constrained bimap that extracts value afterwards
--- @generic L, R, Res
--- @param self Either<L, R> 
--- @param ifLeft fun(l: L): Res 
--- @param ifRight fun(r: R): Res 
--- @return Res 
function Either.prototype:condense(ifLeft, ifRight)
  return self:bimap(ifLeft, ifRight).value
end
return Either

