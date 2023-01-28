local Expr = require "expr"
local Stmt = require "stmt"
local Renderer = {}
local helpers = require "helpers"

function Renderer.renderOp(name)
  if name == "*" or name == "-" or name == "+" or name == "/"
    or name == "^" or name == "==" or name == ">=" or name == "~=" or
    name == "<=" or name == ">" or name == "<" or name == "%" then
    return true, name
  elseif name == "||" then
    return true, "or"
  elseif name == "&&" then
    return true, "and"
  elseif name == "++" then
    return true, ".."
  else
    return false, name
  end
end
function Renderer.fallbackOpFun(left, right, name)
  return left .. "['".. name .. "']" .. " or " .. right .. "['" .. name .. "']"
end
function Renderer.render(ss)
  local s = ""
  local sep = ""
  for _, v in ipairs(ss) do
    s = s .. sep .. Renderer.renderStmt(v)
    sep = "; "
  end
  return s
end

function Renderer.renderVar(var)
  if var.name == "index" then
    return Renderer.renderExpr(var.args[1]) .. "[" .. Renderer.renderExpr(var.args[2]) .. "]"
  elseif var.name == "field" then
    return Renderer.renderExpr(var.args[1]) .. "['" .. var.args[2] .. "']"
  elseif var.name == "name" then
    --- TODO: May break
    return var.args[1]
  end
end
--- @param s Stmt
function Renderer.renderStmt(s)
  if s.name == Stmt.STMTS.expr then
    return "_ = " .. Renderer.renderExpr(s.args[1])
  elseif s.name == Stmt.STMTS.assign then
    local args = ""
    local sep = ""
    for _, v in ipairs(s.args[1]) do
      args = args .. sep .. Renderer.renderVar(v)
      sep = ", "
    end
    return args .. " = " .. Renderer.renderExpr(s.args[2])
  elseif s.name == Stmt.STMTS.breakStmt then
    return "break;"
  elseif s.name == Stmt.STMTS.declare then
    local args = ""
    local sep = ""
    for _, v in ipairs(s.args[1]) do
      args = args .. sep .. v
      sep = ", "
    end
    return "local " .. args .. " = " .. Renderer.renderExpr(s.args[2])
  elseif s.name == Stmt.STMTS.whenStmt then
    local elseBranch = "end"
    if s.args[3] ~= nil then
      elseBranch = " else " .. Renderer.renderStmt(s.args[3]) .. " end"
    end
    return "if " .. Renderer.renderExpr(s.args[1]) .. " then " .. Renderer.renderStmt(s.args[2]) .. elseBranch
  end
end

function Renderer.renderExpr(e)
  if e.name == Expr.EXPRS.atom then
    return "(" .. Renderer.renderExpr(e.args[1]) .. ")"
  elseif e.name == Expr.EXPRS.block then
    local ret = "return "
    if e.args[2] ~= nil then
      ret = ret .. Renderer.renderExpr(e.args[2])
    end
    return "(function () " .. Renderer.render(e.args[1]) .. ";" .. ret .. "end)()"
  elseif e.name == Expr.EXPRS.boolean then
    return helpers.sanePrint(e.args[1])
  elseif e.name == Expr.EXPRS.call then
    local s = Renderer.renderExpr(e.args[1]) .. "("
    local sep = ""
    for _, v in ipairs(e.args[2]) do
      s = s .. sep .. Renderer.renderExpr(v)
      sep = ", "
    end
    return s .. ")"
  elseif e.name == Expr.EXPRS.field then
    -- Fields get translated to indexes due to special operator names
    return Renderer.renderExpr(e.args[1]) .. "['" .. e.args[2] .. "']"
  elseif e.name == Expr.EXPRS.identifier then
    -- TODO: doesn't work with fancy identifiers
    return e.args[1]
  elseif e.name == Expr.EXPRS.ifExpr then
    local cond = Renderer.renderExpr(e.args[1])
    local ibranch = Renderer.renderExpr(e.args[2])
    local ebranch = Renderer.renderExpr(e.args[3])
    local s = "if " .. cond .. " then "
    -- make sure it's only evaluated when it is needed
    -- I LOVE CIRCUIT FLOW
    s = s .. " return " .. ibranch .. " else "
    s = s .. " return " .. ebranch .. " end"
    return "(function() " .. s .. "end )()"
  elseif e.name == Expr.EXPRS.index then
    return Renderer.renderExpr(e.args[1]) .. "[" .. Renderer.renderExpr(e.args[2]) .. "]"
  elseif e.name == Expr.EXPRS.instance then
    -- NOT NICE DON'T YOU DARE DO IT TWICE
    local s = "(function () local _lunar_it = " .. Renderer.renderExpr(e.args[1]) .. "; return _lunar_it['" .. e.args[2] .. "'](_lunar_it"
    for _, v in ipairs(e.args[3]) do
      s = s .. ", " .. Renderer.renderExpr(v)
    end
    return s .. ") end)()"
  elseif e.name == Expr.EXPRS.lambda then
    local s = "(function ("
    local sep = ""
    for _, v in ipairs(e.args[1]) do
      s = s .. sep .. v
      sep = ", "
    end
    s = s .. ") "
    s = s .. "return " .. Renderer.renderExpr(e.args[2])
    s = s .. " end)"
    return s
  elseif e.name == Expr.EXPRS.null then
    return " nil "
  elseif e.name == Expr.EXPRS.number then
    return e.args[1]
  elseif e.name == Expr.EXPRS.operator then
    -- TODO: MADNESS RENDERING
    -- how this works; if it's a normal lua operator
    -- then just print it out. Otherwise do it ourselves :troll:
    local isVanilla, name = Renderer.renderOp(e.args[1])
    local left = Renderer.renderExpr(e.args[2])
    local right = Renderer.renderExpr(e.args[3])
    if isVanilla then
      return left .. " " .. name .. " " .. right
    else
      return Renderer.fallbackOpFun(left, right, name)
    end
  elseif e.name == Expr.EXPRS.string then
    return '"' .. e.args[1] .. '"'
  elseif e.name == Expr.EXPRS.table then
    local s = "{"
    local sep = ""
    for _, v in ipairs(e.args[1]) do
      if v.key ~= nil then
        s = s .. sep .. "[" .. Renderer.renderExpr(v.key) .. "] = " .. Renderer.renderExpr(v.value)
      else
        s = s .. sep .. Renderer.renderExpr(v.value)
      end
      sep = ", "
    end
    return s .. "}"
  elseif e.name == Expr.EXPRS.unary then
    -- TODO: AGAIN, SANITY LOSS
    --  Operators require conversion. 
  end
end
return Renderer
