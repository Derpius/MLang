---@class OperatorDesc
---@field precedence integer
---@field unary boolean
---@field funcName string
local operatorDesc = {}

--- Creates an operator description struct
---@param precedence integer
---@param funcName string
---@param unary? boolean
---@return OperatorDesc
local function OperatorDesc(precedence, funcName, unary)
	if unary == nil then unary = false end

	return {
		precedence = precedence,
		unary = unary,
		funcName = funcName
	}
end

MLang.OperatorDesc = OperatorDesc

---@type table<string, OperatorDesc>
MLang.OPERATORS = {
	-- Boolean
	["||"] = OperatorDesc(1, "or"),
	["&&"] = OperatorDesc(2, "and"),
	["!"] =  OperatorDesc(11, "not", true),

	["<"] =  OperatorDesc(3, "lt"),
	["<="] = OperatorDesc(3, "lte"),
	[">"] =  OperatorDesc(3, "gt"),
	[">="] = OperatorDesc(3, "gte"),
	["!="] = OperatorDesc(3, "noeq"),
	["=="] = OperatorDesc(3, "eq"),

	-- Bitwise
	["|"] =  OperatorDesc(4, "bor"),
	["^"] =  OperatorDesc(5, "bxor"),
	["&"] =  OperatorDesc(6, "band"),
	["<<"] = OperatorDesc(7, "lshift"),
	[">>"] = OperatorDesc(7, "rshift"),
	["~"] =  OperatorDesc(11, "bnot", true),

	-- Maths
	["+"] =  OperatorDesc(8, "add"),
	["-"] =  OperatorDesc(8, "sub"),
	["*"] =  OperatorDesc(9, "mul"),
	["/"] =  OperatorDesc(9, "div"),
	["//"] = OperatorDesc(9, "fdiv"),
	["%"] =  OperatorDesc(9, "mod"),
	["#"] =  OperatorDesc(11, "usub", true), -- hash is equivalent to -x, but a different symbol for ease of programming
}
