local helpers = require"helpers"

--- @class Stmt: { name: STMTS, args: any[]}
local Stmt = {}
Stmt.mt = {}
--- @enum STMTS
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
--- @param name STMTS
--- @param ... any 
--- @return Stmt
function Stmt.new(name, ...)
  return setmetatable({name = name, args = {...}}, Stmt.mt)
end

function Stmt.Assign(vars, expr)
  return Stmt.new("assign", vars, expr)
end
function Stmt.Expr(expr)
  return Stmt.new("expr", expr)
end
Stmt.Break = Stmt.new("break")
function Stmt.Return(returns)
  return Stmt.new("return", returns)
end

function Stmt.When(cond, ibranch, ebranch)
  return Stmt.new("when", cond, ibranch, ebranch)
end

function Stmt.Declare(names, expr)
  return Stmt.new("declare", names, expr)
end

function Stmt.mt.__tostring(self)
  return helpers.ADTPrint("Stmt", self)
end
return Stmt
