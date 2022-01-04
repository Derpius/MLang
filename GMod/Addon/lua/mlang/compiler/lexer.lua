---@class Token
---@field category string
---@field value? string|number|boolean
---@field line integer
---@field col integer
local token = {}
token.__index = token

--- Creates a new Token
---@param category string
---@param value? string|number|boolean
---@param line integer
---@param col integer
---@return Token
local function Token(category, value, line, col)
	return setmetatable({category = category, value = value, line = line, col = col}, token)
end

function token:__tostring()
	return string.format("Token<%s> [%i, %i] = %s", self.category, self.line, self.col, tostring(self.value))
end

MLang.Token = Token

--- Gets a character from a string at the specified index
---@param str string
---@param idx integer
---@return string
local function getChar(str, idx)
	return str:sub(idx, idx)
end

local MakeLUT, MakeStringLUT = MLang.Utils.MakeLUT, MLang.Utils.MakeStringLUT

--- Enum of all MLang keywords
---@type table<string, integer>
local Keyword = {
	Const = 0,
	If = 1,
	Else = 2,
	While = 3,
	Do = 4,
	For = 5,
	Foreach = 6,
	Return = 7,
	Break = 8,
	Continue = 9,
	Class = 10,
	Private = 11,
	Public = 12,
	Operator = 13,
	Namespace = 14,
	Try = 15,
	Catch = 16
}
MLang.Keyword = Keyword

local KEYWORDS = {
	["const"] = Keyword.Const,
	["if"] = Keyword.If,
	["else"] = Keyword.Else,
	["while"] = Keyword.While,
	["do"] = Keyword.Do,
	["for"] = Keyword.For,
	["foreach"] = Keyword.Foreach,
	["return"] = Keyword.Return,
	["break"] = Keyword.Break,
	["continue"] = Keyword.Continue,
	["class"] = Keyword.Class,
	["private"] = Keyword.Private,
	["public"] = Keyword.Public,
	["operator"] = Keyword.Operator,
	["namespace"] = Keyword.Namespace,
	["try"] = Keyword.Try,
	["catch"] = Keyword.Catch
}



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
local CONTROL = MakeStringLUT("(){}[],;.:<>")
local STRING = MakeStringLUT("'\"")
local NUMBER = MakeStringLUT("0123456789.")

-- All the chars that could ever possibly be an operator either by themselves or along side more
local OPERATOR_CHARS = MakeStringLUT("|&!=^~+-*/%")

-- All operators with a valid assignment operator counterpart
local ASSIGNMENT_OPS = MakeLUT({"|", "&", "+", "-", "*", "/", "//", "%"})

---@param ctx Context
---@param code string
---@return table<integer, Token>
local function lex(ctx, code)
	local tokens, numTokens, tokenStartCol = {}, 0, 0
	local line, col = 1, 0
	local codeLen = #code
	local charPtr = 1

	--- Helper to append a token to the table
	---@param category string
	---@param value? string|number|boolean
	local function appendTok(category, value)
		numTokens = numTokens + 1
		tokens[numTokens] = Token(category, value, line, tokenStartCol)
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
					appendTok("assignment", op)
				elseif op == "=" then
					appendTok("assignment")
				elseif op == "-" and getChar(code, charPtr):match("[%(_%w]") and (charPtr - 2 <= 0 or not getChar(code, charPtr - 2):match("[%)_%w]")) then
					appendTok("operator", "#") -- Special unary negative operator
				elseif MLang.OPERATORS[op] then
					appendTok("operator", op)
				else
					ctx:Throw("Invalid operator '" .. op .. "'", line, tokenStartCol)
				end
			elseif CONTROL[char] then -- Control characters
				if char == "<" or char == ">" then -- Special condition to handle <> as a guaranteed operator
					if code[charPtr] == char and code[charPtr + 1] == "=" then
						charPtr = charPtr + 2
						col = col + 2
						appendTok("assignment", char .. char .. "=")
					elseif code[charPtr] == "=" then
						nextChar()
						appendTok("operator", char .. "=")
					else
						appendTok(char)
					end
				else
					appendTok(char)
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

				appendTok("literal", str)
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
				appendTok("literal", tonumber(numString))
			elseif char:match("[_%a]") then -- Symbols
				local symbol = char

				while getChar(code, charPtr):match("[_%w]") do
					symbol = symbol .. nextChar()
				end

				if KEYWORDS[symbol] then
					appendTok("keyword", KEYWORDS[symbol])
				elseif symbol == "true" or symbol == "false" then
					appendTok("literal", symbol == "true")
				elseif symbol == "null" then
					appendTok("literal")
				else
					appendTok("symbol", symbol)
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
---@return table<integer, Token>?
function MLang.Lex(ctx, code)
	--[[local ret
	pcall(function() ret = lex(ctx, code) end)
	return ret]]
	return lex(ctx, code)
end
