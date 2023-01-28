local Parser = require "parser"
local Renderer = require "render"

assert(arg[1] ~= nil and arg[2] ~= nil, "usage: lua run.lua in.lunar out.lua")
local f = assert(io.open(arg[2], "w"))
local input = assert(io.open(arg[1], "r"))
local ADT, rest = Parser.parse(input:read("a"))
input:close()
if rest == nil then
  print(ADT)
  return
end
assert(rest == "", "remainder unparsed: \n" .. rest)
assert(f:write(Renderer.render(ADT)))
