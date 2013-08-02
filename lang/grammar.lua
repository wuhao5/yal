local lpeg = require "lpeg"

local P, S, R, V = lpeg.P, lpeg.S, lpeg.R, lpeg.V
local C, Cp, Cg = lpeg.C, lpeg.Cp, lpeg.Cg

local _09 = R"09"
local space = S" \t" ^ 0
local charset = R("az", "AZ") + S"_"
local charnum = charset + _09
local keywords = {}
local function K(key)
	if not keywords[key] then 
		keywords[key] = P(key) + -charnum
	end
	return keywords[key] 
end

local position, msg
local function E_Num(num, p)
	position = p
	msg = "parsing number error at:" .. p
	return true
end
local mt = {
	__index = function(tbl, name)
		local v = rawget(tbl, name)
		if v == nil then 
			v = V(name)
			tbl[name] = v
		end
		return v
	end
}

local tonumber = tonumber
local build_grammar = function()
	return P {
		Block,
		NumHex = P"0x" * R("09", "af", "AF")^1,
		NumOct = P"0" * R("17") * R("07")^0,
		NumDec = R("19") * _09^0,
		NumFloat = ((NumDec + P"0") * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * NumDec) ^-1,
		Number = P"-"^-1 * space * (NumHex + NumOct + NumFloat + NumDec) * P(E_Num) * -charset,
		Block = Number/tonumber * space
	}
end

local grammar
if setfenv then
	grammar = setfenv(build_grammar, setmetatable({}, mt))()
else
end

local parse = function(str)
	local ast = grammar:match(str)
	if ast == nil then
		return false, msg, position
	else
		return ast
	end
end

return {
	parse = parse
}
