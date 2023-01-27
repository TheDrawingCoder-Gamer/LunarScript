_G.sanePrint = require"helpers".sanePrint

_G.oldPrint = print
_G.print = function (...)
  local s = ""
  local sep = ""
  for _, v in ipairs({...}) do
    s = s .. sep .. sanePrint(v)
    sep = "\t"
  end
  oldPrint(s)
end
_G.parser = require"parser"
_G.parsec = parser.parsec
