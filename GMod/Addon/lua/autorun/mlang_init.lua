MLang = {}

include("mlang/utils.lua")

include("mlang/context.lua")

include("mlang/compiler/objects.lua")
include("mlang/compiler/operators.lua")
include("mlang/compiler/lexer.lua")
include("mlang/compiler/parser.lua")
include("mlang/compiler/transpiler.lua")

-- DEBUG CODE

if SERVER then return end -- prevent double printing in singleplayer

local context = MLang.Context("testing")

local code = [[
	num double(num n) {
		return n;
	}

	num double(string s) {
		return 4;
	}

	num doubled = double(3);
	num doubled2 = double("hello world");
	num error = double();
]]

print("\nLEXING")
local toks = MLang.Lex(context, code)

if context.error then
	print(string.format("[%i, %i]: %s", context.error.line, context.error.col, context.error.message))
elseif not toks then
	print("Unknown error occured")
end

for i, tok in ipairs(toks) do
	print(tostring(tok))
end

print("\nPARSING")
local ast = MLang.Parse(context, toks)

if context.error then
	print(string.format("[%i, %i]: %s", context.error.line, context.error.col, context.error.message))
elseif not ast then
	print("Unknown error occured")
end

PrintTable(ast) -- TODO: better AST printing

print("\nTRANSPILING")

local lua = MLang.Transpile(context, ast)

if context.error then
	print(string.format("[%i, %i]: %s", context.error.line, context.error.col, context.error.message))
elseif not lua then
	print("Unknown error occured")
end

--print(lua)
