--- Gets a character from a string at the specified index
---@param str string
---@param idx integer
---@return string
local function getChar(str, idx)
	return str:sub(idx, idx)
end

local MakeLUT, MakeStringLUT = MLang.Utils.MakeLUT, MLang.Utils.MakeStringLUT

local KEYWORD = MLang.KEYWORD
local Tokens = MLang.Tokens

local ESCAPE_CHARS = {
	b = "\\b", -- The string literal escape sequences are stored unescaped as they'll be emitted to raw lua
	f = "\\f",
	n = "\\n",
	r = "\\r",
	t = "\\t",
	["\\"] = "\\\\",
	['"'] = '\\"',
	["'"] = "\\'"
}

local WHITESPACE = MakeStringLUT(" \t\r")
local CONTROL = {
	[";"] = Tokens.Semicolon,
	[":"] = Tokens.Colon,
	[","] = Tokens.Comma,
	["."] = Tokens.FullStop,
	["("] = Tokens.OpenBracket,
	[")"] = Tokens.ClosedBracket,
	["{"] = Tokens.OpenCurly,
	["}"] = Tokens.ClosedCurly,
	["["] = Tokens.OpenSquare,
	["]"] = Tokens.ClosedSquare,
	["<"] = Tokens.OpenAngle,
	[">"] = Tokens.ClosedAngle
}
local STRING = MakeStringLUT("'\"")
local NUMBER = MakeStringLUT("0123456789.")

-- All the chars that could ever possibly be an operator either by themselves or along side more
local OPERATOR_CHARS = MakeStringLUT("|&!=^~+-*/%")

-- All operators with a valid assignment operator counterpart
local ASSIGNMENT_OPS = MakeLUT({"|", "&", "+", "-", "*", "/", "//", "%"})

-- All tokens which, when preceding a minus, will force that minus to be binary
local NON_UNARY_TOKS = MakeLUT({Tokens.Literal, Tokens.Symbol, Tokens.ClosedBracket, Tokens.ClosedSquare})

---@param ctx Context
---@param code string
---@return table<integer, Tokens.Base>
local function lex(ctx, code)
	local tokens, numTokens, tokenStartCol = {}, 0, 0
	local line, col = 1, 0
	local codeLen = #code
	local charPtr = 1
	local lastTokType

	--- Helper to append a token to the table
	---@param type Tokens.Base
	---@param value? string|number|boolean
	local function appendTok(type, value)
		lastTokType = type
		numTokens = numTokens + 1
		tokens[numTokens] = type.new(line, tokenStartCol, value)
	end

	--- Get the next char in sequence and advance the pointer
	---@return string
	local function nextChar()
		charPtr = charPtr + 1
		col = col + 1
		return getChar(code, charPtr - 1)
	end

	while charPtr <= codeLen do
		tokenStartCol = col
		local char = nextChar()

		if not WHITESPACE[char] then
			if char == "\n" then -- Newline
				line = line + 1
				col = 0
			elseif OPERATOR_CHARS[char] then -- Operators
				 -- All ops need an operand after them
				if charPtr >= codeLen then
					ctx:Throw("Expected characters after operator", line, tokenStartCol)
				end

				local op = char

				-- Handle 2 char ops
				if MLang.OPERATORS[op .. getChar(code, charPtr)] then
					op = op .. nextChar()
				end

				if ASSIGNMENT_OPS[op] and getChar(code, charPtr) == "=" then
					nextChar()
					appendTok(Tokens.Assignment, op)
				elseif op == "=" then
					appendTok(Tokens.Assignment)
				elseif op == "-" and (not lastTokType or not NON_UNARY_TOKS[lastTokType]) then
					appendTok(Tokens.Operator, "#") -- Special unary negative operator
				elseif MLang.OPERATORS[op] then
					appendTok(Tokens.Operator, op)
				else
					ctx:Throw("Invalid operator '" .. op .. "'", line, tokenStartCol)
				end
			elseif CONTROL[char] then -- Control characters
				if char == "<" or char == ">" then -- Special condition to handle <> as a guaranteed operator
					if code[charPtr] == char and code[charPtr + 1] == "=" then
						charPtr = charPtr + 2
						col = col + 2
						appendTok(Tokens.Assignment, char .. char .. "=")
					elseif code[charPtr] == "=" then
						nextChar()
						appendTok(Tokens.Operator, char .. "=")
					else
						appendTok(CONTROL[char])
					end
				else
					appendTok(CONTROL[char])
				end
			elseif STRING[char] then -- Strings
				local str, delim = "", char

				while true do
					if charPtr > codeLen then
						ctx:Throw("A string ran off the end of the program", line, col)
					end
					char = nextChar()

					if char == delim then
						break
					elseif char == "'" or char == '"' then -- Prevent escaping out of a string ('"' would compile to lua as """)
						str = str .. "\\" .. char
					elseif char == "\\" then
						char = nextChar()

						if ESCAPE_CHARS[char] then
							str = str .. ESCAPE_CHARS[char]
						elseif char ~= "\n" then -- multiline strings via C macro style newline escapes
							ctx:Throw("Invalid escape sequence", line, col)
						end
					elseif char == "\n" then
						ctx:Throw("A string ran off the end of a line (escape newline with \\ if this was intentional)", line, col)
					elseif char ~= "\r" then -- Completely ignore CR to avoid escaping issues with Windows line endings
						str = str .. char
					end
				end

				appendTok(Tokens.Literal, str)
			elseif NUMBER[char] then -- Numbers
				local numString, decimalPlaceCount = char, char == "." and 1 or 0

				while NUMBER[getChar(code, charPtr)] do
					char = nextChar()

					if char == "." then
						if decimalPlaceCount > 0 then ctx:Throw("Too many decimal points", line, col) end
						decimalPlaceCount = decimalPlaceCount + 1
					end

					numString = numString .. char
				end
				appendTok(Tokens.Literal, tonumber(numString))
			elseif char:match("[_%a]") then -- Symbols
				local symbol = char

				while getChar(code, charPtr):match("[_%w]") do
					symbol = symbol .. nextChar()
				end

				if KEYWORD[symbol] then
					appendTok(Tokens.Keyword, KEYWORD[symbol])
				elseif symbol == "true" or symbol == "false" then
					appendTok(Tokens.Literal, symbol == "true")
				elseif symbol == "null" then
					appendTok(Tokens.Literal)
				else
					appendTok(Tokens.Symbol, symbol)
				end
			else
				ctx:Throw("Unrecognized character '" .. char .. "'", line, col)
			end
		end
	end

	return tokens
end

--- Lexes MLang code into tokens
---@param ctx Context
---@param code string
---@return table<integer, Tokens.Base>?
function MLang.Lex(ctx, code)
	--[[local ret
	pcall(function() ret = lex(ctx, code) end)
	return ret]]
	return lex(ctx, code)
end
