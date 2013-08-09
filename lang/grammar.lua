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

local booleanLiteral = P"true" + P"false"
local stringLiteral

local keywords = {}
local function K(key)
	if not keywords[key] then 
		keywords[key] = P(key) * (-charnum)
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
	--lpeg.setmaxstack(10000)
	local W = function(p) return Space0 * p; end
	return P {
		Block,

		SS = (space + Comment) ^ 0,
		SL = (space + Comment + nl) ^ 0,
		SL1 = (space + Comment + nl) ^ 1,
		Space0 = (space + Comment) ^ 0,
		Space = (space + Comment) ^ 1,
		Separator1 = (separator + Comment),
		Separator = (Space0 * separator)^1,
		Comment = P"--" * longstring + (P"--" * (-("[" * P"="^0 *"["))) * (1 - P"\n") ^ 0 * (P("\n")-1)^-1,
		NumHex = P"0x" * R("09", "af", "AF")^1,
		NumOct = P"0" * R("17") * R("07")^0,
		NumDec = R("19") * _09^0,
		NumFloat = ((P"0" + NumDec) * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * NumDec) ^-1,
		Number = P"-"^-1 * W(NumHex + NumOct + NumFloat + NumDec),

		FuncLit = P"(" * W(IdList^0) * W")" * W"->" * W(Statement);

		UnOp = K"not" + "#" + "-",
		BinOp = P"=" + "+" + "-" + "*" + "/" + "%" + K"and" + K"or" + "..",
		RelOp = P"<=" + P">=" + P"<" + P">" + P"~=" + P"==",
		AssignOp = P"=" + P"-=" + P"+=" + P"*=" + P"/=" + P"%=",
		RangeGen = Number * Space * "to" * Space * Number * (Space * "by" * Space * Number)^-1,
		--ArgList = Expr,
		--FuncCall = Id * ( (W"(" * W(ArgList) * W")")  + (Space * ArgList) ), 
		--Expr = UnOp * W(Value) + Value * (W(BinOp) * W(Value))^0,
		Expr = AssignExp,

		AssignExp = ListExp * (W(AssignOp) * W(AssignExp)) ^ 0, -- right associative
		ListExp = OrExp * (W"," * W(ListExp)) ^ 0,
		OrExp = AndExp * (W(K"or") * W(OrExp)) ^ 0,
		AndExp = RelExp * (W(K"and") * W(AndExp)) ^ 0,
		RelExp = ConcatExp * (W(RelOp) * W(RelExp)) ^ 0,
		ConcatExp = TermExp * (W".." * W(ConcatExp)) ^ 0,
		TermExp = FactorExp * (W(S"+-") * W(TermExp)) ^ 0,
		FactorExp = UnaryExp * (W(S"*/%") * W(FactorExp)) ^ 0,
		UnaryExp = UnOp ^ 0 * W(ExpoExp),
		ExpoExp = W(PostExp) * (W"^" * W(ExpoExp)) ^ 0,
		PostExp = Value * (W(IndexPostfix) + CallPostfix) ^ 0,
		IndexPostfix = '[' * W(Expr) * ']' + S'.:' * W(Id),
		CallPostfix = '(' * W(Expr) * W')' + Space * W(Expr), -- fix spacing/break/nl/semi colon
		Value = RangeGen + Number + Id + FuncLit + ("(" * W(Expr) * W")"), -- + Value + W'[' + W(Expr) + W']'
		--Value = Value * W"(" * W")" + Value * W'[' * W(Expr) * W']' + Value * W(S".:") * Id + W'(' * W(Expr) * W')'
		--SimpleValue = RangeGen + Number + Id + P"(" * W(Expr) * W')',

		Case = K"case" * SL1 * Expr * SL * "{" * SL * (CaseMatch * SL)^0 * "}",
		CaseMatch = Expr * SL * "->" * SL * Statement, 
		For = K"for" * SL * "(" * SL * IdList * SL * "<-" * SL * Expr * SL * ")" * SL * Statement,
		While = K"while" * SL * "(" * SL * Expr * SL * ")" * SL * Statement,
		TryCatch = K"try" * (#P"{"+SL1) * Statement * SL * K"catch" * (#P"{" + SL1) * Statement,

		Id = charset * charnum^0, -- -keywords
		IdList = Id * (W',' * W(Id))^0,
		ExprList = Expr * (W',' * W(Expr))^0,
		Decl = (K'val' + K'var') * SL1 * IdList * SL * ('=' * SL * Expr)^-1,

		SimpleStatement = Decl + For + While + Case + TryCatch + Expr, -- FIXME: ;; => empty clause
		CompoundStatement = P"{" * SL * (SS * Statement * Separator)^0 * SS * Statement^-1 * SS * "}",
		Statement = Separator / "empty" + SimpleStatement / "simple" + CompoundStatement /"compound",
		Block = shebang^-1 * SS * Ct(( Statement * (Separator/"sep") * (SS/"ss1"))^0 * (SS/"ss2") * Statement^-1 * (SS/"ss3") * Cp()) * P(1)^0-- Number/tonumber * space
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
