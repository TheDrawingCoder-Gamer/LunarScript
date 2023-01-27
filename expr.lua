
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

exprmt.__tostring = helpers.ADTPrint

return expr
