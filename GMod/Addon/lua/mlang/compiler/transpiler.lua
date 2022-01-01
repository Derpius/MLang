local Objects, IsObjectOfType = MLang.Objects, MLang.IsObjectOfType

---@type fun(str: string): string
local hashFunc = util.SHA256

---@class Scope
---@field symbols table<string, Variable|Class>
local scope = {}

--- Create a new scope object
---@return Scope
local function Scope()
	return {symbols = {}}
end

--#region Base Types

local BaseClasses = {
	["void"] = Objects.Class(-1, -1, "void", {}),
	["bool"] = Objects.Class(-1, -1, "bool", {}),
	["num"] = Objects.Class(-1, -1, "num", {}),
	["string"] = Objects.Class(-1, -1, "string", {})
}

local BaseTypes = {
	["void"] = Objects.Type(-1, -1, "void", {}),
	["bool"] = Objects.Type(-1, -1, "bool", {}),
	["num"] = Objects.Type(-1, -1, "num", {}),
	["string"] = Objects.Type(-1, -1, "string", {})
}

for k, v in pairs(BaseTypes) do
	v.classPtr = BaseClasses[k]
	v.signature = k
end

local function LuaLiteralToMLangType(literal)
	return ({
		["nil"] = "void",
		["boolean"] = "bool",
		["number"] = "num",
		["string"] = "string"
	})[type(literal)]
end

--- Performs a compile time literal operation
---@param ctx Context
---@param operator Operator
---@param a? boolean|number|string
---@param b? boolean|number|string
---@return boolean|number|string
local function literalOperation(ctx, operator, a, b)
	--- Attempts to cast a literal to a boolean type
	---@param literal? boolean|number|string
	---@return boolean
	local function boolCast(literal)
		if type(literal) == "boolean" then return literal end
		if type(literal) == "number" then return literal ~= 0 end
		if type(literal) == "nil" then return false end
		if type(literal) == "string" then
			ctx:Throw("Cannot implicitly cast string to bool", operator.line, operator.col)
		end

	end
	
	--- Attempts to cast a literal to a numeric type
	---@param literal? boolean|number|string
	local function numericCast(literal)
		if type(literal) == "number" then return literal end
		if type(literal) == "boolean" then return literal and 1 or 0 end
		if type(literal) == "string" then
			ctx:Throw("Cannot implicitly cast string to number", operator.line, operator.col)
		end
		if type(literal) == "nil" then
			ctx:Throw("Cannot implicitly convert null to number", operator.line, operator.col)
		end
	end
	
	local literalOperators = {
		-- Boolean
		["||"] = function()
			a = boolCast(a)
			b = boolCast(b)
			return a or b
		end,
		["&&"] = function()
			a = boolCast(a)
			b = boolCast(b)
			return a and b
		end,
		["!"] = function()
			a = boolCast(a)
			return not a
		end,
	
		["<"] = function()
			a = numericCast(a)
			b = numericCast(b)
			return a < b
		end,
		["<="] = function()
			a = numericCast(a)
			b = numericCast(b)
			return a <= b
		end,
		[">"] = function()
			a = numericCast(a)
			b = numericCast(b)
			return a > b
		end,
		[">="] = function()
			a = numericCast(a)
			b = numericCast(b)
			return a >= b
		end,
		["!="] = function()
			return a ~= b
		end,
		["=="] = function()
			return a == b
		end,
	
		-- Bitwise
		["|"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return bit.bor(a, b)
		end,
		["^"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return bit.bxor(a, b)
		end,
		["&"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return bit.band(a, b)
		end,
		["<<"] = function()
			a = numericCast(a)
			b = numericCast(b)
			return bit.lshift(a, b)
		end,
		[">>"] = function()
			a = numericCast(a)
			b = numericCast(b)
			return bit.rshift(a, b)
		end,
		["~"] =  function()
			a = numericCast(a)
			return bit.bnot(a)
		end,
	
		-- Maths
		["+"] =  function()
			if type(a) == "string" and type(b) == "string" then
				return a .. b
			end

			a = numericCast(a)
			b = numericCast(b)
			return a + b
		end,
		["-"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return a - b
		end,
		["*"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return a * b
		end,
		["/"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return a / b
		end,
		["//"] = function()
			a = numericCast(a)
			b = numericCast(b)
			return math.floor(a / b)
		end,
		["%"] =  function()
			a = numericCast(a)
			b = numericCast(b)
			return a % b
		end,
		["#"] =  function()
			a = numericCast(a)
			return -a
		end,
	}

	return literalOperators[operator.name]()
end

--#endregion

---@param ctx Context
---@param ast BaseObject[]
---@return function
local function transpile(ctx, ast)
	local astSize = #ast
	if astSize == 0 then
		return function() end
	end

	--- Returns true if two types are equal
	---@param a Type
	---@param b Type
	---@return boolean
	local function areTypesEqual(a, b)
		return a.signature == b.signature
	end

	--- Checks that two types are the same class and have the same template arguments
	---@param a Type
	---@param b Type
	---@param origin BaseObject Object to place the error message on
	local function checkType(a, b, origin)
		if not areTypesEqual(a, b) then
			ctx:Throw("Type mismatch", origin.line, origin.col)
		end
	end

	--- Gets the return type of a class's operator, if it exists
	---@param class Class
	---@param operatorName string
	---@param unary boolean
	---@param lhsType Type
	---@param rhsType Type
	---@param origin BaseObject
	---@return Type?
	local function getClassOperator(class, operatorName, unary, lhsType, rhsType, origin)
		for i, operator in ipairs(class.operators) do
			if operator.symbol == operatorName then
				if unary then
					if (
						areTypesEqual(operator.type.params[1].type, rhsType)
					) then
						return operator.type.retType
					end
				else
					if (
						areTypesEqual(operator.type.params[1].type, lhsType) and
						areTypesEqual(operator.type.params[2].type, rhsType)
					) then
						return operator.type.retType
					end
				end
			end
		end
	end

	--#region Emitting

	local code, codePtr = {}, 1
	local indent = 0

	--- Emits a line with indentation to the output
	---@param str string
	local function emitLine(str)
		code[codePtr] = string.rep("\t", indent) .. str .. "\n"
		codePtr = codePtr + 1
	end

	--#endregion

	--#region Scope Management

	local scopes, scopePtr = {Scope()}, 1

	--- Pushes a new scope to the stack
	local function pushScope()
		scopePtr = scopePtr + 1
		scopes[scopePtr] = Scope()
		indent = indent + 1
	end

	--- Pops the current scope from the stack
	local function popScope()
		scopes[scopePtr] = nil
		scopePtr = scopePtr - 1
		indent = indent - 1

		if scopePtr < 1 then error("Attempted to pop global scope") end
	end

	--- Declares a symbol in the current scope
	---@param object Variable|Class
	local function declareSymbol(object)
		if scopes[scopePtr].symbols[object.symbol] then
			ctx:Throw("Attempted to redeclare symbol", object.line, object.col)
		end

		scopes[scopePtr].symbols[object.symbol] = object
	end

	--- Looks for a symbol in the current and all parent scopes, returns nil if not found
	---@param name string
	---@return Variable|Class?
	local function lookupSymbol(name)
		if BaseClasses[name] then
			return BaseClasses[name]
		end

		for i = scopePtr, 1, -1 do
			if scopes[i].symbols[name] then
				return scopes[i].symbols[name]
			end
		end
	end

	--- Requires a symbol to be declared (only useful with Set operations and Types, see requireDefinedSymbol)
	---@param operation Get|Set|Call|Type
	---@return Variable|Class
	local function requireSymbol(operation)
		local symbol = lookupSymbol(operation.symbol)
		if not symbol then
			ctx:Throw("Undeclared identifier '" .. operation.symbol .. "'", operation.line, operation.col)
		end
		return symbol
	end

	--- Requires a variable to be declared
	---@param operation Get|Set|Call
	---@return Variable
	local function requireVariable(operation)
		local symbol = requireSymbol(operation)
		if not IsObjectOfType(symbol, Objects.Variable) then
			ctx:Throw("Type name not allowed", operation.line, operation.col)
		end
		return symbol
	end

	--- Requires a variable to be defined
	---@param operation Get|Set|Call
	---@return Variable
	local function requireDefinedVariable(operation)
		local var = requireVariable(operation)
		if not var.defined then
			ctx:Throw("'" .. operation.symbol .. "' is undefined", operation.line, operation.col)
		end
		return var
	end

	--- Requires a type to be declared
	---@param operation Type
	---@return Class
	local function requireType(operation)
		local symbol = requireSymbol(operation)
		if not IsObjectOfType(symbol, Objects.Class) then
			ctx:Throw("Variable name not allowed", operation.line, operation.col)
		end
		return symbol
	end

	--- Requires a type to be defined
	---@param operation Type
	---@return Class
	local function requireDefinedType(operation)
		local class = requireType(operation)
		if not class.defined then
			ctx:Throw("Incomplete type not allowed", operation.line, operation.col)
		end
		return class
	end

	--#endregion

	--#region Compilers

	--- Compiles a type into a hash
	---@param type Type
	local function compileType(type)
		local class = requireType(type)
		type.classPtr = class

		if BaseClasses[class.symbol] then
			type.signature = class.symbol
		else
			type.signature = hashFunc(tostring(class))

			if #class.template > 0 then
				-- TODO: check that the type's template arguments match the class, and that the class can be compiled with said args
				-- compileClass(class, templateArgs)
			end
		end
	end

	local function compileFunction()

	end

	local function compileClass()

	end

	--- Compiles an expression from the root node
	---@param root BaseObject
	---@return string
	---@return Type
	---@return boolean isLiteral Whether or not the result of the operation was a literal
	local function compileExpression(root)
		--- Compiles an operand
		---@type fun(operand: Literal|Operator): boolean|number|string?, Type
		local compileOperand

		--- Compiles an operator (unary and binary)
		---@param operator Operator
		---@return string
		---@return Type
		local function compileOperator(operator)
			if operator.unary then
				local operand, operandType, isLiteral = compileOperand(operator.rhs)

				if isLiteral then
					local result = literalOperation(ctx, operator, operand)
					return result, BaseTypes[LuaLiteralToMLangType(result)], true
				end
				
				local retType = getClassOperator(operandType.classPtr, operator.name, true, nil, operandType, operator)

				if not retType then
					ctx:Throw("Invalid operands", operator.line, operator.col)
				end

				return ("%s:%s()"):format(operand, MLang.OPERATORS[operator.name].funcName), retType
			else
				local lhs, lhsType, lhsIsLiteral = compileOperand(operator.lhs)
				local rhs, rhsType, rhsIsLiteral = compileOperand(operator.rhs)

				if lhsIsLiteral and rhsIsLiteral then
					local result = literalOperation(ctx, operator, lhs, rhs)
					return result, BaseTypes[LuaLiteralToMLangType(result)], true
				end

				local retType = getClassOperator(lhsType.classPtr, operator.name, false, lhsType, rhsType)
				if not retType then
					retType = getClassOperator(rhsType.classPtr, operator.name, false, rhsType, lhsType)
					lhs, rhs = rhs, lhs
				end

				if not retType then
					ctx:Throw("Invalid operands", operator.line, operator.col)
				end

				return ("%s:%s(%s)"):format(lhs, MLang.OPERATORS[operator.name].funcName, rhs), retType
			end
		end

		---@param operand Literal|Operator
		---@return boolean|number|string?
		---@return Type
		---@return boolean isLiteral Whether or not the operand was a literal
		function compileOperand(operand)
			if IsObjectOfType(operand, Objects.Operator) then
				return compileOperator(operand)
			elseif IsObjectOfType(operand, Objects.Literal) then
				return operand.value, BaseTypes[LuaLiteralToMLangType(operand.value)], true
			end
		end

		local compiled, type = compileOperand(root)
		return tostring(compiled), type
	end

	--- Compiles variable declaration
	---@param declaration Variable
	---@return string
	local function compileDeclaration(declaration)
		compileType(declaration.type)
		declareSymbol(declaration)

		local ret = "nil"
		if declaration.defined then
			if IsObjectOfType(declaration.type, Objects.Type) then
				local expression, resolvedType = compileExpression(declaration.value)
				checkType(declaration.type, resolvedType, declaration)
				ret = expression
			else -- TODO: Function declaration

			end
		end

		return string.format("local MLANGVAR_%s = %s", declaration.symbol, ret)
	end

	--- Compiles a standard line
	---@param object BaseObject
	local function compileLine(object)
		if IsObjectOfType(object, Objects.Variable) then
			emitLine(compileDeclaration(object))
		end
	end

	--#endregion

	for i = 1, astSize do
		compileLine(ast[i])
	end

	local lua = table.concat(code)
	local func = CompileString(lua, ctx.name, false)
	if type(func) == "string" then
		ctx:Throw("Lua Syntax Error: " .. func, -1, -1)
	end
	return lua, func
end

--- Transpile an abstract syntax tree outputted by MLang.Parse to Lua
---@param ctx Context
---@param ast BaseObject[]
---@return function
function MLang.Transpile(ctx, ast)
	--[[local lua, func
	pcall(function() lua, func = transpile(ctx, ast) end)
	return ret]]
	return transpile(ctx, ast)
end
