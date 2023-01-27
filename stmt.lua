local helpers = require"helpers"

--- @class Stmt
local Stmt = {}
Stmt.mt = { __tostring = helpers.ADTPrint }
Stmt.STMTS = {
  assign = "assign",
  -- future note: Expr in lua CAN'T BE Statements. 
  -- must do `_ = someExpr` to acheive this effect (jank)
  expr = "expr",
  breakStmt = "break",
  returnStmt = "return",
  -- When statements will be like normal lua if statements. 
  -- If _expressions_ will be reserved for actual expressions. 
  -- The reason to use when is to just save on compile 
  -- (if will likely compile funky)
  whenStmt = "when",
  declare = "declare"
}
--- @param name string 
--- @param ... any 
--- @return Stmt
function Stmt.new(name, ...)
  return setmetatable({name = name, args = {...}}, Stmt.mt)
end

return Stmt
