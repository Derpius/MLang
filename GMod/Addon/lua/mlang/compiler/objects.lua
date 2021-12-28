MLang.Objects = {}

function MLang.IsObjectOfType(object, type)
	return object._constructor == type
end

---@class BaseObject
---@field line integer
---@field col integer
---@field _constructor function
---@field base? BaseObject
local baseObject = {}

---@class Literal : BaseObject
---@field value boolean|integer|number|string
local literal = {}

--- Literal datatype that will be emitted directly to Lua
---@param line integer
---@param col integer
---@param value? boolean|integer|number|string
---@return Literal
function MLang.Objects.Literal(line, col, value)
	return {line = line, col = col, value = value, _constructor = MLang.Objects.Literal}
end

---@class Type : BaseObject
---@field name string
---@field template Type[]
local type = {}

--- A complete type including template arguments
---@param line integer
---@param col integer
---@param name string
---@param template Type[]
---@return Type
function MLang.Objects.Type(line, col, name, template)
	return {line = line, col = col, name = name, template = template, _constructor = MLang.Objects.Type}
end

---@class Function : BaseObject
---@field type Type
---@field params Variable[]
---@field block BaseObject[]
local func = {}

--- Callable function object
---@param line integer
---@param col integer
---@param type Type
---@param params Variable[]
---@return Function
function MLang.Objects.Function(line, col, type, params)
	return {line = line, col = col, type = type, params = params, block = {}, _constructor = MLang.Objects.Function}
end

---@class Operator : BaseObject
---@field name string
---@field unary boolean
---@field lhs? BaseObject
---@field rhs BaseObject
local operator = {}

--- Unary/binary operator containing the left and right hand side operands
---@param line integer
---@param col integer
---@param name string
---@param unary boolean
---@param lhs? BaseObject
---@param rhs BaseObject
---@return Operator
function MLang.Objects.Operator(line, col, name, unary, lhs, rhs)
	return {line = line, col = col, name = name, unary = unary, lhs = lhs, rhs = rhs, _constructor = MLang.Objects.Operator}
end

---@class Variable : BaseObject
---@field constant boolean
---@field type Type|Function
---@field symbol string
---@field defined boolean
---@field value? BaseObject|BaseObject[]
local variable = {}

--- Any defined symbol
---@param line integer
---@param col integer
---@param constant boolean
---@param type Type|Function
---@param symbol string
---@return Variable
function MLang.Objects.Variable(line, col, constant, type, symbol)
	return {
		line = line, col = col,
		constant = constant, type = type, symbol = symbol,
		defined = false,
		_constructor = MLang.Objects.Variable
	}
end

---@class Get : BaseObject
---@field symbol string
local get = {}

--- Getting the value of a symbol
---@param line integer
---@param col integer
---@param symbol string
---@return Get
function MLang.Objects.Get(line, col, symbol)
	return {line = line, col = col, symbol = symbol, _constructor = MLang.Objects.Get}
end

---@class Set : BaseObject
---@field symbol string
---@param value BaseObject
local set = {}

--- Setting the value of a symbol
---@param line integer
---@param col integer
---@param symbol string
---@param value BaseObject
---@return Set
function MLang.Objects.Set(line, col, symbol, value)
	return {line = line, col = col, symbol = symbol, value = value, _constructor = MLang.Objects.Set}
end

---@class Call : BaseObject
---@field symbol string
---@field args BaseObject[]
---@field template Type[]
local call = {}

--- Calling the value of a symbol
---@param line integer
---@param col integer
---@param symbol string
---@param args BaseObject[]
---@param template? Type[]
---@return Call
function MLang.Objects.Call(line, col, symbol, args, template)
	return {line = line, col = col, symbol = symbol, args = args, template = template or {}, _constructor = MLang.Objects.Call}
end

---@class Index : BaseObject
---@field container Call|Get
---@field key BaseObject
local index = {}

--- Indexing an object
---@param line integer
---@param col integer
---@param container Call|Get
---@param key BaseObject
---@return Get
function MLang.Objects.Index(line, col, container, key)
	return {line = line, col = col, container = container, key = key, _constructor = MLang.Objects.Index}
end

---@class If : BaseObject
---@field condition BaseObject
---@field block BaseObject[]
---@field otherwise? BaseObject[]|If
local _if = {}

--- If and else if statements
---@param line integer
---@param col integer
---@param condition BaseObject
---@param block BaseObject[]
---@return If
function MLang.Objects.If(line, col, condition, block)
	return {line = line, col = col, condition = condition, block = block, _constructor = MLang.Objects.If}
end

---@class While : BaseObject
---@field condition BaseObject
---@field block BaseObject[]
---@field postcondition boolean
local _while = {}

--- While loops
---@param line integer
---@param col integer
---@param condition BaseObject
---@param block BaseObject[]
---@param postcondition boolean
---@return While
function MLang.Objects.While(line, col, condition, block, postcondition)
	return {line = line, col = col, condition = condition, block = block, postcondition = postcondition, _constructor = MLang.Objects.While}
end
