MLang.Objects = {}

function MLang.IsObjectOfType(object, type)
	return object._constructor == type
end

---@class BaseObject
---@field line integer
---@field col integer
---@field _constructor function
---@field base? Get|Call|Index
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
---@field symbol string
---@field template Type[]
---@field classPtr? Class Used when transpiling
---@field signature? string Used when transpiling
local type = {}

--- A complete type including template arguments
---@param line integer
---@param col integer
---@param symbol string
---@param template Type[]
---@return Type
function MLang.Objects.Type(line, col, symbol, template)
	return {line = line, col = col, symbol = symbol, template = template, _constructor = MLang.Objects.Type}
end

---@class Function : BaseObject
---@field retType Type
---@field params Variable[]
---@field template string[]
---@field signature? string Used when transpiling
local func = {}

--- Callable function object
---@param line integer
---@param col integer
---@param retType Type
---@param params Variable[]
---@param template string[]
---@return Function
function MLang.Objects.Function(line, col, retType, params, template)
	return {line = line, col = col, retType = retType, params = params, template = template, _constructor = MLang.Objects.Function}
end

---@class Operator : BaseObject
---@field name string
---@field unary boolean
---@field lhs? Operator|Literal|Get|Call
---@field rhs Operator|Literal|Get|Call
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
---@field value BaseObject
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
---@param template Type[]
---@return Call
function MLang.Objects.Call(line, col, symbol, args, template)
	return {line = line, col = col, symbol = symbol, args = args, template = template, _constructor = MLang.Objects.Call}
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

---@class For : BaseObject
---@field iterator? Variable
---@field condition? BaseObject
---@field incrementor? Set
---@field block BaseObject[]
local _for = {}

--- For loops
---@param line integer
---@param col integer
---@param iterator? Variable
---@param condition? BaseObject
---@param incrementor? Set
---@param block BaseObject[]
---@return For
function MLang.Objects.For(line, col, iterator, condition, incrementor, block)
	return {
		line = line, col = col,
		iterator = iterator, condition = condition, incrementor = incrementor,
		block = block, _constructor = MLang.Objects.For
	}
end

---@class Return : BaseObject
---@field expression BaseObject
local _return = {}

--- Returning from functions
---@param line integer
---@param col integer
---@param expression BaseObject
---@return Return
function MLang.Objects.Return(line, col, expression)
	return {line = line, col = col, expression = expression, _constructor = MLang.Objects.Return}
end

---@class LoopControl : BaseObject
---@field breaking boolean
local loopControl = {}

--- Break and continue
---@param line integer
---@param col integer
---@param breaking boolean
---@return Return
function MLang.Objects.LoopControl(line, col, breaking)
	return {line = line, col = col, breaking = breaking, _constructor = MLang.Objects.LoopControl}
end

---@class Class : BaseObject
---@field symbol string
---@field defined boolean
---@field privates table<string, Variable>
---@field publics table<string, Variable>
---@field constructors table<string, Variable>
---@field operators table<string, Variable>
---@field extends? Type
---@field baseTemplateArgs Type[]
---@field template? string[]

--- Classes
---@param line integer
---@param col integer
---@param symbol string
---@param template string[]
---@return Class
function MLang.Objects.Class(line, col, symbol, template)
	return {
		line = line, col = col,
		symbol = symbol, template = template,
		defined = false,
		privates = {}, publics = {}, constructors = {}, operators = {},
		_constructor = MLang.Objects.Class
	}
end
