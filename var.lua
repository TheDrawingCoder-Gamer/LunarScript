local Expr = require "expr"

local Var = {}
Var.mt = {}
local KIND = {
  field = "field",
  index = "index",
  name = "name"
}
Var.KIND = KIND
function Var.mt.__tostring(self)
  return require"helpers".ADTPrint("Var", self)
end
local function new(name, ...)
  return setmetatable({name = name, args = {...}}, Var.mt)
end
function Var.Field(target, name)
  return new("field", target, name)
end
function Var.Index(target, expr)
  return new("index", target, expr)
end
function Var.Name(name)
  return new("name", name)
end

function Var.fromField(field, expr)
  return new(field.name, expr, field.args[1])
end
local Field = {}
Field.prototype = {}
Field.mt = { __index = Field.prototype }
Field.KIND = {
  field = "field",
  index = "index"
}
local function newField(name, ...)
  return setmetatable({name = name, args = {...}}, Field.mt)
end
function Field.mt.__tostring(self)
  return require"helpers".ADTPrint("Field", self)
end
function Field.prototype:asExpr(expr)
  if self.name == "field" then
    return Expr.Field(expr, self.args[1])
  elseif self.name == "index" then
    return Expr.Index(expr, self.args[1])
  end
  error(": (")
end
function Field.Field(name)
  return newField("field", name)
end
function Field.Index(expr)
  return newField("index", expr)
end
return {Var, Field}
