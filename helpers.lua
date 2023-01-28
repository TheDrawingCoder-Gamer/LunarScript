local M = {}

--- @param table table 
--- @return table 
function M.shallowCopy(table)
  if type(table) ~= "table" then
    error("expected a table")
  end
  local res = {}
  for k, v in pairs(table) do
    res[k] = v
  end
  return res
end
--- @generic T
--- @generic A
--- @param list T[] 
--- @param starting A 
--- @param fun fun(accum: A, elem: T): A 
--- @return A 
-- array<T>, A, (A, T -> A) -> A
function M.foldLeft(list, starting, fun)
  local goodLs = M.shallowCopy(list)
  local accum = starting
  while #goodLs ~= 0 do
    local i = table.remove(goodLs, 1)
    accum = fun(accum, i)
  end
  return accum
end

--- @generic T
--- @generic A
--- @param list T[] 
--- @param starting A 
--- @param fun fun(elem: T, accum: A): A 
--- @return A 
-- array<T>, A, (T, A-> A) -> A
function M.foldRight(list, starting, fun)
  local goodLs = M.shallowCopy(list)
  local accum = starting
  while #goodLs ~= 0 do
    local i = table.remove(goodLs)
    accum = fun(i, accum)
  end
  return accum
end

--- @param item any 
--- @param rest any[] 
function M.cons(item, rest)
  local r = M.shallowCopy(rest)
  table.insert(r, 1, item)
  return r
end
function M.sanePrint(v)
  local t = type(v)
  if t == "boolean" then
    if v then
      return "true"
    else
      return "false"
    end
  elseif t == "nil" then
    return "nil"
  elseif t == "table" and getmetatable(v) == nil then
    local s = "{"
    local sep = ""
    for k, val in pairs(v) do
      s = s .. sep .. M.sanePrint(k) .. "=" .. M.sanePrint(val)
      sep = ", "
    end
    return s .. "}"
  else
    return tostring(v)
  end
end
function M.ADTPrint(name, e)
  local s = name .. "." .. e.name .. "("
  local sep = ""
  for _, v in ipairs(e.args) do
    s = s .. sep .. M.sanePrint(v)
    sep = ", "
  end
  return s .. ")"
end
return M
