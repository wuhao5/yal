local lpeg = require"lpeg"

local P, S, R, V = lpeg.P, lpeg.S, lpeg.R, lpeg.V
local C, Cp, Cg, Cmt, Cb, Ct, Cc, Cf = lpeg.C, lpeg.Cp, lpeg.Cg, lpeg.Cmt, lpeg.Cb, lpeg.Ct, lpeg.Cc, lpeg.Cf

local _09 = R"09"
local space = S" \t\r"
local nl = S"\n"
local charset = R("az", "AZ") + S"_"
local charnum = charset + _09
local separator = S";\n"
local shebang = P "#" * (P(1) - P "\n")^0 * P "\n"^-1;
local longstring = P { 
	V "open" * ((P(1) - V "closeeq")^0) * V "close"; 
	open = "[" * Cg((P "=")^0, "init") * P "[" * (P "\n")^-1;
	close = "]" * C((P "=")^0) * "]";
	closeeq = Cmt(V "close" * Cb "init", function (s, i, a, b) return a == b end)
};

local _keywords = {}
local keywords
local function K(key)
	if not _keywords[key] then 
		_keywords[key] = P(key) * (-charnum)
		if not keywords then keywords = _keywords[key]
		else keywords = keywords + _keywords[key] end
	end
	return _keywords[key] 
end

local grammar = setfenv(function()
	lpeg.setmaxstack(10000)
	local Comment = P"--" * longstring + (P"--" * (-("[" * P"="^0 *"["))) * (1 - P"\n") ^ 0 * (P("\n")-1)^-1
	local SS = (space + Comment) ^ 0
	local SL = (space + Comment + nl) ^ 0
	local Separator = (SS * separator)^1
	local SS1 = (space + Comment) ^ 1
	local SL1 = (space + Comment + nl) ^ 1
	local decimal = R("19") * _09^0 + "0"
	return P {
		shebang ^ -1 * Ct(SL * Chunk * SL * Cp()) * P(1)^0;
		Chunk = Block,
		Block = (Statement * SL)^0 * RetStat ^ -1;
		Statement = P";" + Varlist * SL * P'=' * SL * ExpList +
			FunctionCall + Label + K"break" + K"goto" * SL * Name +
			K"do" * SL * Block * K"end" +
			K"while" * SL * Exp * SL * K"do" * SL * Block * SL * K"end" +
			K"repeat"  * SL * Block * SL * K"until" * SL * Exp +
			K"if" * SL * Exp * SL * K"then" * SL * Block * SL * (K"elseif" * SL * Exp * SL * K"then" * SL * Block * SL)^0 
				* (K"else" * SL * Block * SL)^-1 * K"end" +
			K"for" * SL * Name * SL * "=" * SL * Exp * "," * SL * Exp * SL * (',' * SL * Exp * SL)^-1
				* K"do" * SL * Block * SL * K"end" +
			K"for" * SL * Namelist * SL * K"in" * SL * Exp * SL * K"do" * SL * Block * SL * K"end" +
			K"function" * SL * FuncName * SL * FuncBody +
			K"local" * SL * K"function" * SL * Name * SL * FuncBody +
			K"local" * SL * Namelist * (SL * "=" * SL * ExpList)^-1,
		RetStat = K"return" * (SL * ExpList)^-1 * (SL * ";")^-1,
		Label = "::" * SL * Name * SL * "::",
		FuncName = Name * (SL * "." * SL * Name)^0 * (SL * ":" * SL * Name)^-1,
		Varlist = Var * (SL * "," * SL * Var)^0,
		--Var = Name + PrefixExp * SL * P"[" * SL * Exp * SL * "]" + PrefixExp * SL * "." * SL * Name,
		--PrefixExp = Var + FunctionCall + "(" * SL * Exp * SL * ')',
		
		PrefixExp = (FunctionCall + Name + "(" * SL * Exp * SL * ')') * (SL * PostfixExp)^0,
		PostfixExp = "[" * SL * Exp * SL *"]" + "." * SL * Name,
		Var = PrefixExp,
		
		--PrefixExp * SL * P"[" * SL * Exp * SL * "]" + PrefixExp * SL * "." * SL * Name,
		Namelist = Name * (SL * "," * SL * Name)^0,
		ExpList = Exp * (SL * "," * SL * Exp)^0,
		Exp = (unop * SL * Exp + K"nil" + K"true" + K"false" + Number + String + P'...' + FuncDef + PrefixExp + 
			TableConstruct) * (SL * binop * SL * Exp)^0,
		CallTail = Args + ":" * SL * Name * SL * Args,
		FunctionCall = (Name + "(" * SL * Exp * SL * ")") * SL * ((PostfixExp * SL)^0 * CallTail)^1,
		Args = "(" * SL * (ExpList * SL)^-1 * ")" + TableConstruct + String,
		FuncDef = K"function" * SL * FuncBody,
		FuncBody = "(" * SL * (Parlist * SL)^-1 * ")" * SL * Block * SL * K"end",
		Parlist = Namelist * (SL * "," * SL * "...")^-1 + "...",
		TableConstruct = "{" * SL * (FieldList * SL)^-1 * "}",
		Field = "[" * SL * Exp * SL * "]" * SL * "=" * SL * Exp + Name * SL * "=" * SL * Exp + Exp,
		FieldSep = P"," + P";",
		FieldList = (Field * SL * FieldSep * SL)^0 * Field^-1, 

		unop = K"not" + P"#" + P"-",
		binop = P"==" + P"<=" + P"~=" + P".." + P">=" + K"or" + K"and" + S"^=><+-*/%" ,

		Name = C(charset * charnum^0 - keywords),
		numHex = P"0x" * R("09", "af", "AF")^1,
		numOct = P"0" * R("17") * R("07")^0,
		numDec = decimal,
		numFloat = ((P"0" + decimal) * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * decimal) ^-1,
		Number = (numHex + numOct + numFloat + numDec),
		String = P"\'" * C((P"\\" * P(1) + (1-S"'\n"))^0) * P"\'" + P'"' * C((P"\\" * P(1) + (1-S'"\n'))^0) * P'"' + longstring
	}
end, setmetatable({}, {
	__index = function(tbl, name)
		local v = rawget(tbl, name)
		if v == nil then 
			if name:match"^[A-Z]" then
				v = (V(name))
				tbl[name] = v
			else
				v = V(name)
				tbl[name] = v
			end
		end
		return v
	end;
}))()

return function(str)
	local ast = grammar:match(str)
	if ast == nil then
		return false, msg, position
	else
		return ast, #str
	end
end

