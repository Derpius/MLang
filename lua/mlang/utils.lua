MLang.Utils = {}

--- Creates a lookup table from a sequential list
---@param tbl table
---@return table
function MLang.Utils.MakeLUT(tbl)
	local ret = {}
	for _, v in ipairs(tbl) do
		ret[v] = true
	end
	return ret
end

--- Creates a lookup table from a string of characters
---@param str string
---@return table
function MLang.Utils.MakeStringLUT(str)
	local ret = {}
	for i = 1, #str do
		ret[str[i]] = true
	end
	return ret
end

--- Pairs 2 sequential tables together as kv pairs
---@param keys any[]
---@param values any[]
---@return table
function MLang.Utils.PairKV(keys, values)
	local ret = {}
	for i, key in ipairs(keys) do
		ret[key] = values[i]
	end
	return ret
end
