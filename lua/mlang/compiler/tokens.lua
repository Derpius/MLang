--- Enum of all MLang keywords
MLang.KEYWORD = {
	["const"] = 0,
	["if"] = 1,
	["else"] = 2,
	["while"] = 3,
	["do"] = 4,
	["for"] = 5,
	["foreach"] = 6,
	["return"] = 7,
	["break"] = 8,
	["continue"] = 9,
	["class"] = 10,
	["private"] = 11,
	["public"] = 12,
	["operator"] = 13,
	["namespace"] = 14,
	["try"] = 15,
	["catch"] = 16,
	["server"] = 17,
	["client"] = 18
}

---@alias Keyword
---| `MLang.KEYWORD.const`
---| `MLang.KEYWORD.if`
---| `MLang.KEYWORD.else`
---| `MLang.KEYWORD.while`
---| `MLang.KEYWORD.do`
---| `MLang.KEYWORD.for`
---| `MLang.KEYWORD.foreach`
---| `MLang.KEYWORD.return`
---| `MLang.KEYWORD.break`
---| `MLang.KEYWORD.continue`
---| `MLang.KEYWORD.class`
---| `MLang.KEYWORD.private`
---| `MLang.KEYWORD.public`
---| `MLang.KEYWORD.operator`
---| `MLang.KEYWORD.namespace`
---| `MLang.KEYWORD.try`
---| `MLang.KEYWORD.catch`
---| `MLang.KEYWORD.server`
---| `MLang.KEYWORD.client`

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
---@param type Tokens.Base
---@return boolean
function MLang.IsTokenOfType(token, type)
	return getmetatable(token) == type
end

---@class Tokens.EoF : Tokens.Base
Tokens.EoF = {}
Tokens.EoF.__index = Tokens.EoF

function Tokens.EoF:__tostring()
	return "EoF"
end

---@return Tokens.EoF
function Tokens.EoF.new()
	return setmetatable({line = -1, col = -1}, Tokens.EoF)
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
	return "literal"
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
	return "symbol"
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
			return name
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
	return self.value
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
	return self.value and self.value .. "=" or "="
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
	return ";"
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
	return ":"
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
	return ","
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
	return "."
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
	return "("
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
	return ")"
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
	return "{"
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
	return "}"
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
	return "["
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
	return "]"
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
	return "<"
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
	return ">"
end

---@param line integer
---@param col integer
---@return Tokens.ClosedAngle
function Tokens.ClosedAngle.new(line, col)
	return setmetatable({line = line, col = col}, Tokens.ClosedAngle)
end

--#endregion

MLang.Tokens = Tokens
