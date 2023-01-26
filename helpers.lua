local M = {}

--- @param table table 
--- @return table 
function M.shallowCopy(table)
  if type(table) ~= "table" then
    error("expected a table")
  end
  local res = {}
  for k, v in ipairs(table) do
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


return M
