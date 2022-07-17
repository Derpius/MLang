local Objects, Keyword = MLang.Objects, MLang.Keyword

---@param ctx Context
---@param tokens table<integer, Token>
---@return table<integer, BaseObject>
local function parse(ctx, tokens)
	local tree = {}
	local tokPtr, numToks = 1, #tokens

	local funcDepth, loopDepth = 0, 0
	local isInRealm = false

	--- Returns the next token if it's of the specified category (doesn't advance the token pointer)
	---@param category string
	---@return Token?
	local function peekTok(category)
		if (
			tokPtr <= numToks and
			tokens[tokPtr].category == category
		) then
			return tokens[tokPtr]
		end
	end

	--- Gets the next token in sequence and advances the pointer without performing any checks
	---@return Token
	local function getTok()
		tokPtr = tokPtr + 1
		return tokens[tokPtr - 1] or MLang.Token("eof", nil, -1, -1)
	end

	--- Gets the next token and returns whether the token was of the type specified  
	--- Only advances the token pointer if the next token is of the type specified
	---@param category string
	---@return boolean
	---@return Token
	local function acceptTok(category)
		if peekTok(category) then
			return true, getTok()
		else
			return false, tokens[tokPtr] or MLang.Token("eof", nil, -1, -1)
		end
	end

	--- Gets the next token if it's of the specified category, errors if not
	---@param category string
	---@param msg? string Custom error message to use
	---@return Token
	local function requireTok(category, msg)
		local found, tok = acceptTok(category)
		if not found then
			ctx:Throw(
				msg or ("Expected '%s', got '%s'"):format(category, tok.category),
				tok.line, tok.col
			)
		end
		return tok
	end

	--- Parses template arguments used with type names and function calls
	---@param requireFunction? boolean
	---@return Type[]
	local function parseTemplateArguments(requireFunction)
		if not peekTok("<") then return {} end

		if requireFunction then
			local nest, ptr = 1, tokPtr + 1
			while nest > 0 do
				if not tokens[ptr] then
					return {}
				end
				
				if tokens[ptr].category == "<" then
					nest = nest + 1
				elseif tokens[ptr].category == ">" then
					nest = nest - 1
				end

				ptr = ptr + 1
			end
			if tokens[ptr].category ~= "(" then return {} end
		end
		
		getTok() -- consume opening angle bracket

		local args, numArgs = {}, 0
		repeat
			local typeName = requireTok("symbol", "Type name expected")
			local type = Objects.Type(typeName.line, typeName.col, typeName.value, parseTemplateArguments())

			numArgs = numArgs + 1
			args[numArgs] = type
		until not acceptTok(",")

		requireTok(">")
		return args
	end

	--- Parses template parameters used when declaring a class or function (`template<TypeName,...>`)
	---@return string[]
	local function parseTemplateParams()
		if not acceptTok("<") then return {} end

		local template, i = {}, 1
		repeat
			local symbol = requireTok("symbol", "Expected type name in template parameters")
			template[i] = symbol.value
			i = i + 1
		until not acceptTok(",")

		requireTok(">")
		return template
	end

	--[[
		Forward declarations
	]]

	--- Parses parameters defined inside `()`  
	--- Not to be confused with arguments passed in `()`
	---@type fun(): table
	local parseParams

	--- Parses an expression
	---@type fun(): BaseObject
	local parseExpression

	--- Parses a code block inside `{}`
	---@type fun(): BaseObject[]
	local parseBlock

	--- Parses a complete type
	---@return Type
	local function parseType()
		local typeName = requireTok("symbol", "Type name expected")
		return MLang.Objects.Type(typeName.line, typeName.col, typeName.value, parseTemplateArguments())
	end

	--- Parses variable declaration
	---@param ignoreSemicolon? boolean Whether or not to require a semicolon after the declaration
	---@return Variable
	local function parseVariable(ignoreSemicolon)
		local constant = false
		local cmdPresent, cmd = acceptTok("keyword")
		if cmdPresent then
			if cmd.value ~= Keyword.Const then
				ctx:Throw("Expected const or type name", cmd.line, cmd.col)
			end
			constant = true
		end

		local type, name = parseType(), requireTok("symbol", "Variable name expected")
		local var = Objects.Variable(
			name.line, name.col,
			constant, type, name.value
		)

		local templateParams = parseTemplateParams()
		if peekTok("(") then
			var = Objects.Function(
				name.line, name.col,
				constant, type, name.value,
				parseParams(), templateParams
			)

			if peekTok("{") then
				funcDepth = funcDepth + 1
				var.value = parseBlock()
				funcDepth = funcDepth - 1
				return var
			end
		elseif #templateParams > 0 then
			ctx:Throw("Template parameters can only be used with functions and type names", name.line, name.col)
		end

		local isAssignment, tok = acceptTok("assignment")
		if isAssignment then
			if not tok.value then
				var.value = parseExpression()
			else
				ctx:Throw("Assignment operator not allowed in definition", tok.line, tok.col)
			end
		end

		if not ignoreSemicolon then
			requireTok(";")
		end
		return var
	end

	function parseParams()
		requireTok("(")
		local params, numParams = {}, 0
		if acceptTok(")") then return {} end

		repeat
			numParams = numParams + 1
			params[numParams] = parseVariable(true)
		until not acceptTok(",")

		requireTok(")")
		return params
	end

	--- Parses arguments passed to a function inside `()`  
	--- Not to be confused with parameters defined inside `()`
	---@return BaseObject[]
	local function parseArgs()
		requireTok("(")
		local args, numArgs = {}, 0
		if acceptTok(")") then return {} end
	
		repeat
			numArgs = numArgs + 1
			args[numArgs] = parseExpression()
		until not acceptTok(",")

		requireTok(")")
		return args
	end

	---Parses a condition inside `()`
	---@return BaseObject
	local function parseCondition()
		local tok = requireTok("(")
		local cond = parseExpression()
		requireTok(")")
		return cond
	end

	--- Parses symbol lookup
	---@param base? BaseObject
	---@return Get|Call|Index
	local function parseLookup(base)
		local symbol = requireTok("symbol", "Expected variable or namespace name")
		local ret

		local templateArgs = parseTemplateArguments(true)

		if peekTok("(") then --- Function call
			ret = MLang.Objects.Call(
				symbol.line, symbol.col,
				symbol.value,
				parseArgs(), templateArgs
			)
		else --- Variable value lookup
			ret = MLang.Objects.Get(symbol.line, symbol.col, symbol.value)
		end

		local indexing, openBracket = acceptTok("[")
		if indexing then
			ret = MLang.Objects.Index(
				openBracket.line, openBracket.col,
				ret, parseExpression()
			)

			if not acceptTok("]") then
				ctx:Throw("Missing closing bracket", openBracket.line, openBracket.col)
			end
		end

		ret.base = base
		if acceptTok(".") then
			return parseLookup(ret)
		end

		return ret
	end

	--- Parses assigning to a declared symbol
	---@param getter Get|Index Base object to assign to
	---@return Set
	local function parseSet(getter)
		local assignmentTok = requireTok("assignment", "Expected variable assignment")
		if assignmentTok.value then
			local setter = Objects.Set(
				assignmentTok.line, assignmentTok.col, getter.symbol,
				Objects.Operator(
					assignmentTok.line, assignmentTok.col,
					assignmentTok.value, false,
					getter, parseExpression()
				)
			)
			setter.base = getter.base

			return setter
		end

		local setter = Objects.Set(assignmentTok.line, assignmentTok.col, getter.symbol, parseExpression())
		setter.base = getter.base

		return setter
	end

	function parseExpression()
		--- Parses an operator
		---@return Token? operator
		---@return integer offset Amount to increment tokPtr by after peeking
		local function parseOperator()
			local opTok = peekTok("operator")
			if opTok then
				return opTok, 1
			end

			if peekTok("<") or peekTok(">") then
				local angleBracket = tokens[tokPtr]
				local op = angleBracket.category
				
				if tokens[tokPtr + 1] and tokens[tokPtr + 1].category == op then
					return MLang.Token("operator", op .. op, angleBracket.line, angleBracket.col), 2
				end
				return MLang.Token("operator", op, angleBracket.line, angleBracket.col), 1
			end
		end

		local parseSubExp

		--- Parses an operand and unary operator
		---@return BaseObject
		local function parseOperand()
			local ret

			local op, offset = parseOperator()
			if op and not MLang.OPERATORS[op.value].unary then
				ctx:Throw("Binary operator cannot be part of an operand", op.line, op.col)
			end
			if op then tokPtr = tokPtr + offset end

			if acceptTok("(") then
				ret = parseSubExp(0)
				requireTok(")", "Missing closing bracket")

				if acceptTok(".") then
					ret = parseLookup(ret)
				end
			elseif peekTok("literal") then
				local literal = getTok()
				ret = MLang.Objects.Literal(literal.line, literal.col, literal.value)
			elseif peekTok("symbol") then
				ret = parseLookup()
			else
				local tok = getTok()
				if tok.category == "eof" then
					ctx:Throw("Expression ran off the end of the program", tok.line, tok.col)
				else
					ctx:Throw("Invalid operand", tok.line, tok.col)
				end
			end

			if op then -- Operator validated as being unary at the start of the function
				ret = MLang.Objects.Operator(
					op.line, op.col,
					op.value, true,
					nil, ret
				)
			end

			return ret
		end

		function parseSubExp(minPrecedence)
			local lhs = parseOperand()

			while true do
				local op, offset = parseOperator()
				if not op or MLang.OPERATORS[op.value].precedence < minPrecedence then
					break
				end

				if MLang.OPERATORS[op.value].unary then
					ctx:Throw("Expected binary operator after operand", op.line, op.col)
				end

				tokPtr = tokPtr + offset
				local rhs = parseSubExp(MLang.OPERATORS[op.value].precedence + 1)

				lhs = MLang.Objects.Operator(
					op.line, op.col,
					op.value, false,
					lhs, rhs
				)
			end

			return lhs
		end

		return parseSubExp(0)
	end

	--- Parse a class definition/declaration
	---@return Class
	local function parseClass()
		local name = requireTok("symbol", "Expected class name")
		local template = parseTemplateParams()
		local class = Objects.Class(name.line, name.col, name.value, template)

		local numConstructors = 0

		if acceptTok(":") then
			class.extends = parseType()
		end

		requireTok("{")
		while not acceptTok("}") do
			if peekTok("symbol") and peekTok("symbol").value == name.value then -- Constructors
				local symbol = getTok()

				local constructor = Objects.Function(
					symbol.line, symbol.col,
					true, Objects.Type(name.line, name.col, name.value, {}), symbol.value,
					parseParams(), template
				)

				funcDepth = funcDepth + 1
				constructor.value = parseBlock()
				funcDepth = funcDepth - 1

				numConstructors = numConstructors + 1
				class.constructors[numConstructors] = constructor
			else
				local keyword = requireTok("keyword", "Expected public/private/operator keyword or a constructor")
				if keyword.value == Keyword.Operator then
					-- TODO: Operator parsing
				else
					if not MLang.Utils.MakeLUT({Keyword.Private, Keyword.Public})[keyword.value] then
						ctx:Throw("Expected public/private/operator or a constructor", keyword.line, keyword.col)
					end

					local public = keyword.value == Keyword.Public
					local var = parseVariable()

					if class.publics[var.symbol] or class.privates[var.symbol] then
						ctx:Throw("Redeclaration of class member", var.line, var.col)
					end

					if public then
						class.publics[var.symbol] = var
					else
						class.privates[var.symbol] = var
					end
				end
			end
		end

		return class
	end

	--- Parses a line of code
	---@return BaseObject
	local function parseLine()
		if peekTok("symbol") then
			--[[
				A line starting with a symbol can either be variable declaration/definition, or a function call
				<symbol><template?><symbol> Declaration, where the first symbol is the type
				<symbol><template?>(        Function call
				<symbol>...                 Definition
			]]

			local oldPtr = tokPtr
			getTok()
			parseTemplateArguments()

			if peekTok("symbol") then -- Must be a declaration, anything else would have an opening bracket or assignment/.
				tokPtr = oldPtr
				return parseVariable()
			else
				tokPtr = oldPtr

				local ret = parseLookup()
				if not MLang.IsObjectOfType(ret, Objects.Call) then
					ret = parseSet(ret)
				end

				requireTok(";")
				return ret
			end
		elseif peekTok("keyword") then
			local keyword = getTok()

			if keyword.value == Keyword.Const then
				tokPtr = tokPtr - 1 -- parseVariable needs to get the const token itself
				return parseVariable()
			elseif keyword.value == Keyword.If then
				local ret = Objects.If(keyword.line, keyword.col, parseCondition(), parseBlock())

				local deepestNode = ret
				while peekTok("keyword") and peekTok("keyword").value == Keyword.Else do
					local elseTok = getTok()
					if not peekTok("keyword") or peekTok("keyword").value ~= Keyword.If then
						deepestNode.otherwise = parseBlock()
						return ret
					end
					getTok() -- consume if keyword

					deepestNode.otherwise = Objects.If(elseTok.line, elseTok.col, parseCondition(), parseBlock())
					deepestNode = deepestNode.otherwise
				end

				return ret
			elseif keyword.value == Keyword.While then
				loopDepth = loopDepth + 1
				local ret = Objects.While(keyword.line, keyword.col, parseCondition(), parseBlock(), false)
				loopDepth = loopDepth - 1

				return ret
			elseif keyword.value == Keyword.Do then
				loopDepth = loopDepth + 1
				local block = parseBlock()
				loopDepth = loopDepth - 1

				local whileKeyword = requireTok("keyword", "Expected while after do block")
				if whileKeyword.value ~= Keyword.While then
					ctx:Throw("Expected while after do block", whileKeyword.line, whileKeyword.col)
				end

				local ret = Objects.While(keyword.line, keyword.col, parseCondition(), block, true)
				requireTok(";")
				return ret
			elseif keyword.value == Keyword.For then
				requireTok("(")

				local iterator
				if not peekTok(";") then -- allow skipping the definition of a loop variable
					iterator = parseVariable()
				end
				requireTok(";")

				local condition
				if not peekTok(";") then
					condition = parseExpression()
				end
				requireTok(";")

				local incrementor
				if not peekTok(")") then
					local symbol = parseLookup()
					if MLang.IsObjectOfType(symbol, Objects.Call) then
						ctx:Throw("Expected variable assignment", symbol.line, symbol.col)
					end
					incrementor = parseSet(symbol)
				end

				requireTok(")")

				loopDepth = loopDepth + 1
				local block = parseBlock()
				loopDepth = loopDepth - 1

				return Objects.For(keyword.line, keyword.col, iterator, condition, incrementor, block)
			elseif keyword.value == Keyword.Foreach then
				ctx:Throw("Not implemented yet", -1, -1)
			elseif keyword.value == Keyword.Return then
				if funcDepth < 1 then
					ctx:Throw("Return can only be used inside a function", keyword.line, keyword.col)
				end

				local ret = Objects.Return(
					keyword.line, keyword.col,
					peekTok(";") and MLang.Objects.Literal(keyword.line, keyword.col) or parseExpression()
				)
				requireTok(";")
				return ret
			elseif keyword.value == Keyword.Break or keyword.value == Keyword.Continue then
				if loopDepth < 1 then
					ctx:Throw("Break and continue can only be used inside a loop", keyword.line, keyword.col)
				end

				local ret = Objects.LoopControl(keyword.line, keyword.col, keyword.value == Keyword.Break)
				requireTok(";")
				return ret
			elseif keyword.value == Keyword.Class then
				return parseClass()
			elseif keyword.value == Keyword.Namespace then
				ctx:Throw("Not implemented yet", -1, -1)
			elseif keyword.value == Keyword.Try then
				ctx:Throw("Not implemented yet", -1, -1)
			elseif keyword.value == Keyword.Server or keyword.value == Keyword.Client then
				if isInRealm then
					ctx:Throw("Can't nest a realm block inside another realm block", keyword.line, keyword.col)
				end

				isInRealm = true
				local ret = Objects.Realm(keyword.line, keyword.col, keyword.value == Keyword.Server, parseBlock())
				isInRealm = false

				return ret
			else
				ctx:Throw("Invalid keyword to start line", keyword.line, keyword.col)
			end
		else
			local tok = getTok()
			ctx:Throw("Unexpected '" .. tok.category .. "' to start line", tok.line, tok.col)
		end
	end

	function parseBlock()
		local openBracket = requireTok("{")

		local block, blockLen = {}, 0
		while not acceptTok("}") do
			if peekTok("eof") then
				ctx:Throw("Missing closing bracket", openBracket.line, openBracket.col)
			end

			blockLen = blockLen + 1
			block[blockLen] = parseLine()
		end
		return block
	end

	local ast, astLen = {}, 0
	while tokPtr <= numToks do
		astLen = astLen + 1
		ast[astLen] = parseLine()
	end
	return ast
end

--- Parses MLang tokens into an abstract syntax tree
---@param ctx Context
---@param tokens table<integer, Token>
---@return table<integer, BaseObject>?
function MLang.Parse(ctx, tokens)
	--[[local ret
	pcall(function() ret = parse(ctx, tokens) end)
	return ret]]
	return parse(ctx, tokens)
end