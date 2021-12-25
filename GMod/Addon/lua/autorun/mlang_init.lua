MLang = {}

include("mlang/utils.lua")

include("mlang/context.lua")

include("mlang/compiler/objects.lua")
include("mlang/compiler/operators.lua")
include("mlang/compiler/lexer.lua")
--include("mlang/compiler/parser.lua")
--include("mlang/compiler/transpiler.lua")

-- DEBUG CODE

local context = MLang.Context("testing")

local code = [[
	test = 8;
]]

local toks = MLang.Lex(context, code)

if context.error then
	print(string.format("[%i, %i]: %s", context.error.line, context.error.col, context.error.message))
elseif not toks then
	print("Unknown error occured")
end

PrintTable(toks)
