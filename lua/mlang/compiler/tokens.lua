--- Enum of all MLang keywords
MLang.KEYWORD = {
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
	Catch = 16,
	Server = 17,
	Client = 18
}

---@alias Keyword
---| `MLang.KEYWORD.Const`
---| `MLang.KEYWORD.If`
---| `MLang.KEYWORD.Else`
---| `MLang.KEYWORD.While`
---| `MLang.KEYWORD.Do`
---| `MLang.KEYWORD.For`
---| `MLang.KEYWORD.Foreach`
---| `MLang.KEYWORD.Return`
---| `MLang.KEYWORD.Break`
---| `MLang.KEYWORD.Continue`
---| `MLang.KEYWORD.Class`
---| `MLang.KEYWORD.Private`
---| `MLang.KEYWORD.Public`
---| `MLang.KEYWORD.Operator`
---| `MLang.KEYWORD.Namespace`
---| `MLang.KEYWORD.Try`
---| `MLang.KEYWORD.Catch`
---| `MLang.KEYWORD.Server`
---| `MLang.KEYWORD.Client`

---@class Tokens.Base
---@field line integer
---@field col integer
local token = {}

--- Base constructor for IntelliSense
---@param line integer
---@param col integer
---@param value? string|number|boolean
---@return Tokens.Base
function token.new(line, col, value) end

Tokens = {}

--- Returns true if the token is of the type specified
---@param token Tokens.Base
---@param type table
---@return boolean
function MLang.IsTokenOfType(token, type)
	return getmetatable(token) == type
end

--#region Syntax Elements

--[[
	Literal
]]
---@class Tokens.Literal : Tokens.Base
---@field value? string|number|boolean
Tokens.Literal = {}
Tokens.Literal.__index = Tokens.Literal

function Tokens.Literal:__tostring()
	return string.format("Literal<%s>", tostring(self.value))
end

---@param line integer
---@param col integer
---@param value? string|number|boolean
---@return Tokens.Literal
function Tokens.Literal.new(line, col, value)
	return setmetatable({line = line, col = col, value = value}, Tokens.Literal)
end

--[[
	Symbol
]]
---@class Tokens.Symbol : Tokens.Base
---@field value string
Tokens.Symbol = {}
Tokens.Symbol.__index = Tokens.Symbol

function Tokens.Symbol:__tostring()
	return string.format("Symbol<%s>", self.value)
end

---@param line integer
---@param col integer
---@param value string
---@return Tokens.Symbol
function Tokens.Symbol.new(line, col, value)
	return setmetatable({line = line, col = col, value = value}, Tokens.Symbol)
end

--[[
	Keyword
]]
---@class Tokens.Keyword : Tokens.Base
---@field value Keyword
Tokens.Keyword = {}
Tokens.Keyword.__index = Tokens.Keyword

function Tokens.Keyword:__tostring()
	for name, val in pairs(MLang.KEYWORD) do
		if val == self.value then
			return string.format("Keyword<%s>", name)
		end
	end
	error("Invalid keyword in token")
end

---@param line integer
---@param col integer
---@param value Keyword
---@return Tokens.Keyword
function Tokens.Keyword.new(line, col, value)
	return setmetatable({line = line, col = col, value = value}, Tokens.Keyword)
end

--[[
	Operator
]]
---@class Tokens.Operator : Tokens.Base
---@field value string
Tokens.Operator = {}
Tokens.Operator.__index = Tokens.Operator

function Tokens.Operator:__tostring()
	return string.format("Operator<%s>", self.value)
end

---@param line integer
---@param col integer
---@param value string
---@return Tokens.Operator
function Tokens.Operator.new(line, col, value)
	return setmetatable({line = line, col = col, value = value}, Tokens.Operator)
end

--[[
	Assignment Operator
]]
---@class Tokens.Assignment : Tokens.Base
---@field value? string
Tokens.Assignment = {}
Tokens.Assignment.__index = Tokens.Assignment

function Tokens.Assignment:__tostring()
	return string.format("Assignment<%s>", self.value or "no op")
end

---@param line integer
---@param col integer
---@param value? string
---@return Tokens.Assignment
function Tokens.Assignment.new(line, col, value)
	return setmetatable({line = line, col = col, value = value}, Tokens.Assignment)
end

--#endregion

--#region Control Characters

---@class Tokens.Semicolon : Tokens.Base
Tokens.Semicolon = {}
Tokens.Semicolon.__index = Tokens.Semicolon

function Tokens.Semicolon:__tostring()
	return "Control<;>"
end

---@param line integer
---@param col integer
---@return Tokens.Semicolon
function Tokens.Semicolon.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.Semicolon)
end

---@class Tokens.Colon : Tokens.Base
Tokens.Colon = {}
Tokens.Colon.__index = Tokens.Colon

function Tokens.Colon:__tostring()
	return "Control<:>"
end

---@param line integer
---@param col integer
---@return Tokens.Colon
function Tokens.Colon.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.Colon)
end

---@class Tokens.Comma : Tokens.Base
Tokens.Comma = {}
Tokens.Comma.__index = Tokens.Comma

function Tokens.Comma:__tostring()
	return "Control<,>"
end

---@param line integer
---@param col integer
---@return Tokens.Comma
function Tokens.Comma.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.Comma)
end

---@class Tokens.FullStop : Tokens.Base
Tokens.FullStop = {}
Tokens.FullStop.__index = Tokens.FullStop

function Tokens.FullStop:__tostring()
	return "Control<.>"
end

---@param line integer
---@param col integer
---@return Tokens.FullStop
function Tokens.FullStop.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.FullStop)
end

---@class Tokens.OpenBracket : Tokens.Base
Tokens.OpenBracket = {}
Tokens.OpenBracket.__index = Tokens.OpenBracket

function Tokens.OpenBracket:__tostring()
	return "Control<(>"
end

---@param line integer
---@param col integer
---@return Tokens.OpenBracket
function Tokens.OpenBracket.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.OpenBracket)
end

---@class Tokens.ClosedBracket : Tokens.Base
Tokens.ClosedBracket = {}
Tokens.ClosedBracket.__index = Tokens.ClosedBracket

function Tokens.ClosedBracket:__tostring()
	return "Control<)>"
end

---@param line integer
---@param col integer
---@return Tokens.ClosedBracket
function Tokens.ClosedBracket.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.ClosedBracket)
end

---@class Tokens.OpenCurly : Tokens.Base
Tokens.OpenCurly = {}
Tokens.OpenCurly.__index = Tokens.OpenCurly

function Tokens.OpenCurly:__tostring()
	return "Control<{>"
end

---@param line integer
---@param col integer
---@return Tokens.OpenCurly
function Tokens.OpenCurly.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.OpenCurly)
end

---@class Tokens.ClosedCurly : Tokens.Base
Tokens.ClosedCurly = {}
Tokens.ClosedCurly.__index = Tokens.ClosedCurly

function Tokens.ClosedCurly:__tostring()
	return "Control<}>"
end

---@param line integer
---@param col integer
---@return Tokens.ClosedCurly
function Tokens.ClosedCurly.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.ClosedCurly)
end

---@class Tokens.OpenSquare : Tokens.Base
Tokens.OpenSquare = {}
Tokens.OpenSquare.__index = Tokens.OpenSquare

function Tokens.OpenSquare:__tostring()
	return "Control<[>"
end

---@param line integer
---@param col integer
---@return Tokens.OpenSquare
function Tokens.OpenSquare.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.OpenSquare)
end

---@class Tokens.ClosedSquare : Tokens.Base
Tokens.ClosedSquare = {}
Tokens.ClosedSquare.__index = Tokens.ClosedSquare

function Tokens.ClosedSquare:__tostring()
	return "Control<]>"
end

---@param line integer
---@param col integer
---@return Tokens.ClosedSquare
function Tokens.ClosedSquare.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.ClosedSquare)
end

---@class Tokens.OpenAngle : Tokens.Base
Tokens.OpenAngle = {}
Tokens.OpenAngle.__index = Tokens.OpenAngle

function Tokens.OpenAngle:__tostring()
	return "Control<<>"
end

---@param line integer
---@param col integer
---@return Tokens.OpenAngle
function Tokens.OpenAngle.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.OpenAngle)
end

---@class Tokens.ClosedAngle : Tokens.Base
Tokens.ClosedAngle = {}
Tokens.ClosedAngle.__index = Tokens.ClosedAngle

function Tokens.ClosedAngle:__tostring()
	return "Control<>>"
end

---@param line integer
---@param col integer
---@return Tokens.ClosedAngle
function Tokens.ClosedAngle.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.ClosedAngle)
end

--#endregion

MLang.Tokens = Tokens
