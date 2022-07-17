local Objects, IsObjectOfType = MLang.Objects, MLang.IsObjectOfType

---@type fun(str: string): string
local hashFunc = util.SHA256

---@class Scope
---@field symbols table<string, Variable|Function|Class>
---@field returnType? Type Expected return type of the current scope
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
		error("Invalid type passed to boolCast")
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
	if #ast == 0 then
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
	---@param msg? string Optional custom error message
	local function checkType(a, b, origin, msg)
		if not areTypesEqual(a, b) then
			ctx:Throw(msg or "Type mismatch", origin.line, origin.col)
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

	--#region Scope Management

	local scopes, scopePtr = {Scope()}, 1

	--- Gets the current scope
	---@return Scope
	local function getScope()
		return scopes[scopePtr]
	end

	--- Pushes a new scope to the stack and returns it
	---@param scope Scope
	local function pushScope(scope)
		scopePtr = scopePtr + 1
		scopes[scopePtr] = scope
	end

	--- Pops the current scope from the stack
	local function popScope()
		scopes[scopePtr] = nil
		scopePtr = scopePtr - 1

		if scopePtr < 1 then error("Attempted to pop global scope") end
	end

	--- Declares a symbol in the current scope
	---@param object Variable|Function|Class
	local function declareSymbol(object)
		if BaseClasses[object.symbol] or scopes[scopePtr].symbols[object.symbol] then
			ctx:Throw("Attempted to redeclare symbol", object.line, object.col)
		end

		scopes[scopePtr].symbols[object.symbol] = object
	end

	--- Looks for a symbol in the current and all parent scopes, returns nil if not found
	---@param name string
	---@return Variable|Function|Class?
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

	--- Requires a symbol to be declared
	---@param operation Get|Set|Call|Index|Type
	---@return Variable|Function|Class
	local function requireSymbol(operation)
		local symbol = lookupSymbol(operation.symbol)
		if not symbol then
			ctx:Throw("Undeclared identifier '" .. operation.symbol .. "'", operation.line, operation.col)
		end
		return symbol
	end

	--- Requires a variable to be declared
	---@param operation Get|Set|Index
	---@return Variable|Function
	local function requireVariable(operation)
		local symbol = requireSymbol(operation)
		if not IsObjectOfType(symbol, Objects.Variable) and not IsObjectOfType(symbol, Objects.Function) then
			ctx:Throw("Type name not allowed", operation.line, operation.col)
		end
		return symbol
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

	--#endregion

	--#region Compilers

	--- Compiles a standard line
	---@param object Variable|Class|Set|Call
	---@return string
	local function compileLine(object) end

	--- Compiles a type into a hash
	---@param type Type
	local function compileType(type)
		local class = requireType(type)
		local signature = tostring(class)
		type.classPtr = class

		if #type.template ~= #class.template then
			ctx:Throw(("Incorrect number of template arguments, %i expected, got %i"):format(#class.template, #type.template))
		end

		for i, arg in ipairs(type.template) do
			compileType(arg)
			signature = signature .. arg.signature
		end

		if #class.template > 0 then
			-- TODO: check that the type's template arguments match the class, and that the class can be compiled with said args
			-- compileClass(class, templateArgs)
		end

		-- Hash final signature
		type.signature = hashFunc(signature)
	end

	local BaseTypes = {}
	for k, _ in pairs(BaseClasses) do
		BaseTypes[k] = Objects.Type(-1, -1, k, {})
		compileType(BaseTypes[k])
	end

	--- Compiles a variable into its Lua name
	---@param var Variable|Function
	---@return string
	local function compileVarName(var)
		return ("%s_%s"):format(var.symbol, var.type.signature)
	end

	--- Compiles an expression from the root node
	---@type fun(root: BaseObject): string, Type
	local compileExpression

	--- Compiles function or variable lookup
	---@param action Get|Call|Index
	---@return string code Compiled code to get this variable
	---@return Type type Type of the symbol
	local function compileLookup(action)
		---@type Variable|Function
		local var

		local accessor = ""

		if action.base then
			local code, type = compileLookup(action.base)

			if not type.classPtr.publics[action.symbol] then
				ctx:Throw(("'%s' is not a member of class '%s'"):format(action.symbol, type.symbol))
			end

			var = type.classPtr.publics[action.symbol]
			accessor = code .. IsObjectOfType(action, Objects.Call) and ":" or "."
		else
			var = requireVariable(action)
		end

		if IsObjectOfType(action, Objects.Get) then
			return accessor .. compileVarName(var), var.type
		elseif IsObjectOfType(action, Objects.Call) then -- TODO: templating
			if not IsObjectOfType(var, Objects.Function) then
				ctx:Throw("Attempted to call non-function", action.line, action.col)
			end

			local argStr, argStrLen, paramSignature = {}, 0, ""
			for i, arg in ipairs(action.args) do
				local expression, type = compileExpression(arg)
				paramSignature = paramSignature .. type.signature

				argStrLen = argStrLen + 1
				argStr[argStrLen] = expression
			end
			paramSignature = hashFunc(paramSignature)

			if not var.overloads[paramSignature] then
				ctx:Throw("Function '" .. var.symbol .. "' has no matching overload", action.line, action.col)
			end

			return ("%s%s_%s(%s)"):format(accessor, compileVarName(var), paramSignature, table.concat(argStr, ", ")), var.type
		else
			-- TODO: indexing
		end
	end

	--- Compiles variable definition
	---@param definition Set
	---@return string
	local function compileSet(definition)
		---@type Variable
		local var

		local accessor = ""

		if definition.base then
			local code, type = compileLookup(definition.base)

			if not type.classPtr.publics[definition.symbol] then
				ctx:Throw(("'%s' is not a member of class '%s'"):format(definition.symbol, type.symbol), definition.line, definition.col)
			end

			var = type.classPtr.publics[definition.symbol]
			accessor = code .. "."
		else
			var = requireVariable(definition)
		end

		if var.value and var.constant then
			ctx:Throw("Attempted to redefine a constant variable", definition.line, definition.col)
		end

		local expression, expType = compileExpression(definition.value)
		checkType(var.type, expType, definition)

		var.value = definition.value
		return ("%s%s = %s"):format(accessor, compileVarName(var), expression)
	end

	local function compileClass()

	end

	---@param root BaseObject
	---@return string
	---@return Type
	function compileExpression(root)
		--- Compiles an operand
		---@type fun(operand: Literal|Operator): boolean|number|string?, Type, boolean
		local compileOperand

		--- Compiles an operator (unary and binary)
		---@param operator Operator
		---@return string
		---@return Type
		---@return boolean isLiteral
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
				if type(operand.value) == "string" then
					operand.value = '"' .. operand.value .. '"'
				end
				
				return operand.value, BaseTypes[LuaLiteralToMLangType(operand.value)], true
			elseif IsObjectOfType(operand, Objects.Get) or IsObjectOfType(operand, Objects.Call) then
				return compileLookup(operand)
			end
		end

		local compiled, type = compileOperand(root)
		return tostring(compiled), type
	end

	--- Compiles variable declaration
	---@param declaration Variable|Function
	---@return string
	local function compileDeclaration(declaration)
		if IsObjectOfType(declaration, Objects.Function) then
			local functionScope = Scope()

			-- TODO: add template symbols here

			compileType(declaration.type)
			functionScope.returnType = declaration.type

			local params, paramSignature = {}, ""
			pushScope(functionScope)
			for i, param in ipairs(declaration.params) do
				params[i] = compileDeclaration(param)
				paramSignature = paramSignature .. param.type.signature
			end
			popScope()
			paramSignature = hashFunc(paramSignature)

			local existingSymbol = getScope().symbols[declaration.symbol] -- Only look in the current scope for overloadables
			if (
				existingSymbol and
				IsObjectOfType(existingSymbol, Objects.Function)
			) then
				if existingSymbol.overloads[paramSignature] then
					ctx:Throw("Function overload already defined", declaration.line, declaration.col)
				end
				existingSymbol.overloads[paramSignature] = true
			else
				declaration.overloads = {[paramSignature] = true}
				declareSymbol(declaration)
			end

			local lines = {}
			if declaration.value then -- TODO: Efficiently handle when a symbol is undefined at runtime
				pushScope(functionScope)
				for i, node in ipairs(declaration.value) do
					lines[i] = compileLine(node)
				end
				popScope()
			end

			return ("function %s_%s(%s)\n%s\nend"):format(
				compileVarName(declaration), paramSignature,
				table.concat(params, ", "),
				table.concat(lines, "\n")
			)
		end

		declareSymbol(declaration)
		compileType(declaration.type)

		if not declaration.value then -- TODO: Efficiently handle when a symbol is undefined at runtime
			return compileVarName(declaration)
		end

		local expression, resolvedType = compileExpression(declaration.value)
		checkType(declaration.type, resolvedType, declaration)
		return ("%s = %s"):format(compileVarName(declaration), expression)
	end

	--- Compiles a standard line
	---@param object Variable|Class|Set|Call|Return
	---@return string
	function compileLine(object)
		if IsObjectOfType(object, Objects.Variable) or IsObjectOfType(object, Objects.Function) then
			return "local " .. compileDeclaration(object)
		elseif IsObjectOfType(object, Objects.Set) then
			return compileSet(object)
		elseif IsObjectOfType(object, Objects.Call) then
			return compileLookup(object)
		elseif IsObjectOfType(object, Objects.Return) then
			local functionScope = getScope()
			if not functionScope.returnType then
				error("Invalid AST, parser allowed return outside function")
			end

			local expression, type = compileExpression(object.expression)
			checkType(functionScope.returnType, type, object, "Invalid return type")
			return ("do return %s end"):format(expression)
		end
	end

	--#endregion

	local code, codePtr = {}, 1
	for _, node in ipairs(ast) do
		code[codePtr] = compileLine(node) .. "\n"
		codePtr = codePtr + 1
	end

	local lua = table.concat(code)
	print(lua)
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
