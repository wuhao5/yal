local lpeg = require "lpeg"

local P, S, R, V = lpeg.P, lpeg.S, lpeg.R, lpeg.V
local C, Cp, Cg, Cmt, Cb, Ct, Cc = lpeg.C, lpeg.Cp, lpeg.Cg, lpeg.Cmt, lpeg.Cb, lpeg.Ct, lpeg.Cc

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
local _string = P"\"" * (P"\\" * P(1) + (1-P"\""))^0 * P"\""

local booleanLiteral = P"true" + P"false"
local stringLiteral

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
	lpeg.setmaxstack(10000)
	--local W = function(p) return Space0 * p; end
	return P {
		Block,

		SS = (space + Comment) ^ 0,
		SS1 = (space + Comment) ^ 1,
		SL = (space + Comment + nl) ^ 0,
		SL1 = (space + Comment + nl) ^ 1,
		--Space0 = (space + Comment) ^ 0,
		--Space = (space + Comment) ^ 1,
		Separator1 = (separator + Comment),
		Separator = (SS * separator)^1,

		-- comment/number/string literal
		Comment = P"--" * longstring + (P"--" * (-("[" * P"="^0 *"["))) * (1 - P"\n") ^ 0 * (P("\n")-1)^-1,
		Boolean = K"true" + K"false",
		Nil = K"nil",
		NumHex = P"0x" * R("09", "af", "AF")^1,
		NumOct = P"0" * R("17") * R("07")^0,
		NumDec = R("19") * _09^0,
		NumFloat = ((P"0" + NumDec) * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * NumDec) ^-1,
		Number = P"-"^-1 * SS * (NumHex + NumOct + NumFloat + NumDec),
		String = P"\'" * (P"\\" * P(1) + (1-S"'\n"))^0 * P"\'" + P'"' * (P"\\" * P(1) + (1-S'"\n'))^0 * P'"' + longstring,

		--generator
		Guard = K"if" * SL * OrExp,
		Generator = IdList * SL * "<-" * SL * Expr * (SS * Guard)^-1,

		-- other literal and comprehension
		TableField = (String + P"[" * SL * Expr * SL * "]") * SL * ":" * SL * OrExp,	-- partial lua style, "field" : value, ["field"] : value
		TableLit = P"{" * SL * (TableField * SL * "," * SL)^0 * TableField^-1 * SL * "}",
		ListComprehension = P"[" * SL * ListExp * SL * (P"|" * SL * Generator * (SL * ';' * SL * Generator)^0 * SL)^-1 * ']',  -- [x,y | x<-1 to 10; y<-1 to x]
		TableComprehension = P"{" * SL * Id * SL * ":" * SL * OrExp * SL * P"|" * SL * Generator * (SL * ';' * SL * Generator)^0 * SL * '}',

		FuncLit = P"(" * SL * (IdList * SL)^-1 * ")" * SL * "->" * SL * Statement;

		UnOp = K"not" + "#" + "-",
		BinOp = P"=" + "+" + "-" + "*" + "/" + "%" + K"and" + K"or" + "..",
		RelOp = P"<=" + P">=" + P"<" + P">" + P"~=" + P"==",
		AssignOp = P"=" + P"-=" + P"+=" + P"*=" + P"/=" + P"%=",
		RangeGen = Number * SS1 * "to" * SS1 * Number * (SS1 * "by" * SS1 * Number)^-1, -- range must be in the same line

		-- expression: assignment, relational, factoring, indexing and invoking
		Expr = AssignExp,
		AssignExp = ListExp * (SS * AssignOp * SL * AssignExp) ^ 0, -- right associative
		ListExp = OrExp * (SS * "," * SL * ListExp) ^ 0,
		OrExp = AndExp * (SS * K"or" * SL1 * OrExp) ^ 0,
		AndExp = RelExp * (SS * K"and" * SL1 * AndExp) ^ 0,
		RelExp = ConcatExp * (SS * RelOp * SL * RelExp) ^ 0,
		ConcatExp = TermExp * (SS * P".." * SL * ConcatExp) ^ 0,
		TermExp = FactorExp * (SS * S"+-" * SL * TermExp) ^ 0,
		FactorExp = UnaryExp * (SS * S"*/%" * SL *  FactorExp) ^ 0,
		UnaryExp = (UnOp * SS) ^ 0 * ExpoExp,	-- Unary stay in the same line
		ExpoExp = PostExp * (SS * "^" * SL * ExpoExp) ^ 0,
		PostExp = Value * (SS * IndexPostfix + CallPostfix) ^ 0,
		IndexPostfix = '[' * SL * Expr * SL * ']' + S'.:' * SL * Id,
		CallPostfix = SS * '(' * SL * Expr^-1 * SL * ')' + SS1 * Expr,
		Value = Boolean + Nil + String + RangeGen + Number + Id + FuncLit + ListComprehension + TableComprehension + TableLit + ("(" * SL * Expr * SL * ")"),

		-- basic control statement
		Case = K"case" * SL1 * Expr * SL * "{" * SL * (CaseMatch * SL)^0 * "}",
		CaseMatch = Expr * SL * "->" * SL * Statement, 
		IfElse = K"if" * SL * "(" * SL * Expr * SL * ")" * SL * Statement * (SL * K"else" * SL * Statement)^-1,
		For = K"for" * SL * "(" * SL * Generator * SL * ")" * SL * Statement,
		While = K"while" * SL * "(" * SL * Expr * SL * ")" * SL * Statement,
		TryCatch = K"try" * (#P"{"+SL1) * Statement * SL * K"catch" * (#P"{" + SL1) * Statement,
		Return = (K"yield" + K"return") * (SS * Expr)^-1 + K"break";

		-- declaration/trait/class
		Type = K"int" + K"float" + K"string" + K"array" + K"table" + K"bool" + Id,
		FuncType = "(" * SL * TypeList^-1 * SL * ")" * SL * "->" * SL * Typedef,
		Trait = "{" * SL * TypeList * SL * "}",
		Typedef = Type + FuncType + Trait,
		TypeList = Typedef * (SS * "," * SL * Typedef)^0,
		IdDecl = (S".:" * SL)^-1 * Id * (SS * ":" * SL * Typedef)^-1,
		IdList = IdDecl * (SS * ',' * SL * IdDecl)^0, --IdList = Id * (SS * ',' * SS * Id)^0,
		Class = (K"class" + K"trait") * SL * Id * SL * (K"extends" * SL1 * Id * SL)^0 * CompoundStatement,

		Decl = (K'val' + K'var') * SL1 * IdList * SS * ('=' * SL * Expr)^-1,

		SimpleStatement = Decl + For + While + Case + TryCatch + IfElse + Return + Class + Expr, 
		CompoundStatement = P"{" * SL * (SS * Statement * Separator)^0 * SS * Statement^-1 * SS * "}",
		Empty = SS * separator * SS,
		Statement = Empty / "empty" + SimpleStatement / "simple" + CompoundStatement /"compound",
		--Statement1 = SimpleStatement / "simple" + CompoundStatement /"compound",
		Block = shebang^-1 * SL * Ct(( Statement * Separator * SS)^0 * SS * (Statement^-1/"last") * SS * Cp()) * P(1)^0, -- Number/tonumber * space

		-- at this point, it should collect all keywords
		Id = charset * charnum^0 - keywords
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
		return ast, #str
	end
end

return {
	parse = parse
}
