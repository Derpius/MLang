---@class ErrorDesc
---@field message string
---@field line integer
---@field char number
local errorDesc = {}

--- Creates a new ErrorDesc struct
---@param message string
---@param line integer
---@param char integer
---@return ErrorDesc
function errorDesc.new(message, line, char)
	return {message = message, line = line, char = char}
end

MLang.ErrorDesc = errorDesc.new
local ErrorDesc = errorDesc.new

---@class Context
---@field name string
---@field error? ErrorDesc
local context = {}
context.__index = context

--- Creates a new Context
---@param name string
---@return Context
function context.new(name)
	return setmetatable({name = name}, context)
end

MLang.Context = context.new
local Context = context.new

--- Throws an MLang error and updates the context (should be used only from within a pcalled function)
---@param msg string
---@param line integer
---@param char integer
function context:Throw(msg, line, char)
	self.error = ErrorDesc(msg, line, char)
	error(msg)
end

