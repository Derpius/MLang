---@class OperatorDesc
---@field precedence integer
---@field unary boolean
local operatorDesc = {}

--- Creates an operator description struct
---@param precedence integer
---@param unary? boolean
---@return OperatorDesc
function operatorDesc.new(precedence, unary)
	if unary == nil then unary = false end

	return {
		precedence = precedence,
		unary = unary
	}
end

MLang.OperatorDesc = operatorDesc.new
local OperatorDesc = operatorDesc.new

MLang.OPERATORS = {
	-- Boolean
	["||"] = OperatorDesc(1),
	["&&"] = OperatorDesc(2),
	["!"] =  OperatorDesc(11, true),

	["<"] =  OperatorDesc(3),
	["<="] = OperatorDesc(3),
	[">"] =  OperatorDesc(3),
	[">="] = OperatorDesc(3),
	["!="] = OperatorDesc(3),
	["=="] = OperatorDesc(3),

	-- Bitwise
	["|"] =  OperatorDesc(4),
	["^"] =  OperatorDesc(5),
	["&"] =  OperatorDesc(6),
	["<<"] = OperatorDesc(7),
	[">>"] = OperatorDesc(7),
	["~"] =  OperatorDesc(11, true),

	-- Maths
	["+"] =  OperatorDesc(8),
	["-"] =  OperatorDesc(8),
	["*"] =  OperatorDesc(9),
	["/"] =  OperatorDesc(9),
	["//"] = OperatorDesc(9),
	["%"] =  OperatorDesc(9),
	["#"] =  OperatorDesc(11, true), -- hash is equivalent to -x, but a different symbol for ease of programming
}
