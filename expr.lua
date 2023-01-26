
local expr = {}
--- @class Expr { name: string, args: any[] }
local exprmt = {}

local function sanePrint(v)
  local t = type(v)
  if t == "boolean" then
    if v then
      return "true"
    else
      return "false"
    end
  elseif t == "nil" then
    return "nil"
  else
    return tostring(v)
  end
end
--- @param name string 
--- @param ... any
--- @return Expr
function expr.new(name, ...)
  local e = { name = name; args = table.pack(...) }
  setmetatable(e, exprmt)
  return e
end

function exprmt.__tostring(e)
  local s = sanePrint(expr.name) .. "("
  local sep = ""
  for _, v in ipairs(e.args) do
    s = s .. sep .. sanePrint(v)
    sep = ", "
  end
  return s .. ")"
end

return expr
