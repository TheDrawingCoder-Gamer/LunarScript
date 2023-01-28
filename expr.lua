
local expr = {}
--- @class Expr { name: string, args: any[] }
local exprmt = {}
--- @enum exprs 
expr.EXPRS = {
  index = "index",
  call = "call",
  field = "field",
  number = "number",
  string = "string",
  boolean = "boolean",
  null = "nil",
  identifier = "identifier",
  operator = "operator",
  unary = "unary",
  table = "table",
  lambda = "lambda",
  ifExpr = "if",
  block = "block",
  -- In lua, parens actually mean something so I have to keep track of them
  atom = "atom",
  instance = "instance"
}
local helpers = require"helpers"
--- @param name exprs
--- @param ... any
--- @return Expr
function expr.new(name, ...)
  local e = { name = name; args = table.pack(...) }
  setmetatable(e, exprmt)
  return e
end

function expr.Index(target, access)
  return expr.new(expr.EXPRS.index, target, access)
end
function expr.Call(target, args)
  return expr.new(expr.EXPRS.call, target, args)
end
function expr.Field(target, name)
  return expr.new(expr.EXPRS.field, target, name)
end
function expr.String(contents)
  return expr.new(expr.EXPRS.string, contents)
end
function expr.Instance(target, name, args)
  return expr.new(expr.EXPRS.instance, target, name, args)
end
function exprmt.__tostring(self)
  return helpers.ADTPrint("Expr", self)
end

return expr
