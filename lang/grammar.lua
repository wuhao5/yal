local lpeg = require "lpeg"

local P, S, R, V = lpeg.P, lpeg.S, lpeg.R, lpeg.V
local C, Cp, Cg, Cmt, Cb = lpeg.C, lpeg.Cp, lpeg.Cg, lpeg.Cmt, lpeg.Cb

local _09 = R"09"
local space = S" \t\n"
local charset = R("az", "AZ") + S"_"
local charnum = charset + _09
local separator = S" ;\n"
local shebang = P "#" * (P(1) - P "\n")^0 * P "\n";

local longstring = P { 
	V "open" * ((P(1) - V "closeeq")^0) * V "close"; 
	open = "[" * Cg((P "=")^0, "init") * P "[" * (P "\n")^-1;
	close = "]" * C((P "=")^0) * "]";
	closeeq = Cmt(V "close" * Cb "init", function (s, i, a, b) return a == b end)
};

local booleanLiteral = P"true" + P"false"
local stringLiteral

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

		Space = (space + Comment) ^ 0,
		Separator = (separator + Comment) ^ 0,
		Comment = P"--" * longstring + (P"--" * (-("[" * P"="^0 *"["))) * (1 - P"\n") ^ 0 * (P("\n")-1)^-1,
		NumHex = P"0x" * R("09", "af", "AF")^1,
		NumOct = P"0" * R("17") * R("07")^0,
		NumDec = R("19") * _09^0,
		NumFloat = ((NumDec + P"0") * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * NumDec) ^-1,
		Number = P"-"^-1 * Space * (NumHex + NumOct + NumFloat + NumDec) * P(E_Num) * -charset,

		Expr = Number,

		Id = charset * charnum^0, -- -keywords
		IdList = Id * (Space * ',' * Space * Id)^0,
		Decl = (P'val' + P'var') * Space * IdList * (Space * '=' * Space * ExprList)^-1,
		ExprList = Expr * (Space * ',' * Expr)^0,

		Statement = Decl + ExprList,
		Block = shebang^-1 * Statement * (Separator * Statement)^0 * Separator-- Number/tonumber * space
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
