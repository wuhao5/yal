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
		_keywords[key] = C(P(key) * (-charnum))
		if not keywords then keywords = _keywords[key]
		else keywords = keywords + _keywords[key] end
	end
	return _keywords[key] 
end

local function CS(s) return C(S(s)) end

local function Attr(key, value)
	return Cg(Cc(value), key)
end

local position, msg
local function E_Num(num, p)
	position = p
	msg = "parsing number error at:" .. p
	return true
end

local action
local function get_action(f)
	local t = action
	for w in f:gmatch"[%w_]+" do
		t = t[w]
	end
	return t
end

local mt = {
	__index = function(tbl, name)
		local v = rawget(tbl, name)
		if v == nil then 
			if name:match"^[A-Z]" then
				v = Ct(V(name))
				tbl[name] = v
			elseif name:match"^[a-z]" then
				v = V(name)
				tbl[name] = v
			else
				return get_action(name)
			end
		end
		return v
	end;
	__newindex = function(tbl, k, v)
		rawset(tbl, k, v)
	end
}

local tonumber = tonumber

local function mark(name)
	return function(...)
		return {name, ...}
	end
end

local scope
local build_grammar = function()
	--lpeg.setmaxstack(10000)
	--local W = function(p) return Space0 * p; end
	Comment = P"--" * longstring + (P"--" * (-("[" * P"="^0 *"["))) * (1 - P"\n") ^ 0 * (P("\n")-1)^-1
	SS = (space + Comment) ^ 0
	SL = (space + Comment + nl) ^ 0
	Separator = (SS * separator)^1
	SS1 = (space + Comment) ^ 1
	SL1 = (space + Comment + nl) ^ 1
	decimal = R("19") * _09^0
	return P {
		Block,

		-- comment/number/string literal
		Boolean = (K"true" + K"false") * Attr("type", "bool"),
		Nil = K"nil" * Attr("type", "nil"),	
		numHex = Cg(C(P"0x" * R("09", "af", "AF")^1), "value") * Attr("type", "hex"),
		numOct = Cg(C(P"0" * R("17") * R("07")^0), "value") * Attr("type", "oct"),
		numDec = Cg(C(decimal), "value") * Attr("type", "dec"),
		numFloat = Cg(C(((P"0" + decimal) * P"." * _09^0 + (P"." * _09^1)) * (S"eE" * P"-"^-1 * decimal) ^-1), "value") * Attr("type", "float"),
		Number = Cg(CS"-+", "sign")^-1 * SS * (numHex + numOct + numFloat + numDec),
		String = Cg(P"\'" * C((P"\\" * P(1) + (1-S"'\n"))^0) * P"\'" + P'"' * C((P"\\" * P(1) + (1-S'"\n'))^0) * P'"' + longstring, "value") * Attr("type", "string"),
		IdValue = idName * Attr("type", "id"),

		--generator
		Guard = K"if" * SL * OrExp,
		Generator = IdList * SL * "<-" * SL * Expr * (SS * Guard)^-1,

		-- other literal and comprehension

		-- partial lua style, "field" : value, ["field"] : value
		TableField = (Cc"value" * String + P"[" * SL * Cc"expr" * Expr * SL * "]") * SL * ":" * SL * OrExp,
		TableLit = P"{" * SL * Cg( Ct((TableField * SL * "," * SL)^0 * TableField^-1), "value") * SL * "}" * Attr("type", "table"),
		-- [x,y | x<-1 to 10; y<-1 to x]
		ListComprehension = Cc"list" * P"[" * SL * ListExp * SL * (P"|" * SL * Generator * (SL * ';' * SL * Generator)^0 * SL)^-1 * ']',  
		-- {k:func(v) | k<-1 to 10}
		TableComprehension = Cc"tbCmp" * P"{" * SL * idName * SL * ":" * SL * OrExp * SL * P"|" * SL * Generator * (SL * ';' * SL * Generator)^0 * SL * '}',

		FuncLit = P"(" * SL * Cg(IdList * SL, "parameter")^-1 * ")" * SL * "->" * SL * Cg(statement, "body");

		unOp = C(K"not" + "#" + "-"),
		binOp = P"=" + "+" + "-" + "*" + "/" + "%" + K"and" + K"or" + "..",
		relOp = C(P"<=" + P">=" + P"<" + P">" + P"~=" + P"=="),
		assignOp = C(P"=" + P"-=" + P"+=" + P"*=" + P"/=" + P"%="),
		RangeGen = Number * SS1 * "to" * SS1 * Number * (SS1 * "by" * SS1 * Number)^-1, -- range must be in the same line

		-- expression: assignment, relational, factoring, indexing and invoking
		--Expr = AssignExp,
		Expr = Cg(ListExp, "left") * Ct(SS * Cg(assignOp, "op") * SL * Cg(Expr,"right"))^ 0, -- right associative
		ListExp = Cg(OrExp, "left") * Ct(SS * Cg(C",", "op") * SL * Cg(ListExp, "right")) ^ 0,
		OrExp = Cg(AndExp, "left") * Ct(SS * Cg(K"or", "op") * SL1 * Cg(OrExp, "right")) ^ 0,
		AndExp = Cg(RelExp, "left") * Ct(SS * Cg(K"and", "op") * SL1 * Cg(AndExp, "right")) ^ 0,
		RelExp = Cg(ConcatExp, "left") * Ct(SS * Cg(relOp, "op") * SL * Cg(RelExp, "right")) ^ 0,
		ConcatExp = Cg(TermExp, "left") * Ct(SS * Cg(C"..", "op") * SL * Cg(ConcatExp, "right")) ^ 0,
		TermExp = Cg(FactorExp, "left") * Ct(SS * Cg(CS"+-", "op") * SL * Cg(TermExp, "right")) ^ 0,
		FactorExp = Cg(UnaryExp, "left") * Ct(SS * Cg(CS"*/%", "op") * SL *  Cg(FactorExp, "right")) ^ 0,
		UnaryExp = Cg(Ct((unOp * SS) ^ 0), "op") * Cg(ExpoExp, "left"),	-- Unary stay in the same line
		ExpoExp = Cg(PostExp, "left") * Ct(SS * Cg(C"^", "op") * SL * Cg(ExpoExp, "right")) ^ 0,
		PostExp = Cg(value, "left") * Ct(SS * Cg(IndexPostfix,"right") * Attr("op", "index") + Cg(CallPostfix, "right") * Attr("op","call")) ^ 0,
		IndexPostfix = '[' * SL * Cg(Expr, "expr") * SL * ']' * Attr("op", ".") + Cg(CS'.:', "op") * SL * idName,
		CallPostfix = SS * '(' * SL * Expr^-1 * SL * ')' + SS1 * Expr,
		value = Boolean + Nil + String + RangeGen + Number + IdValue + 
			FuncLit * Attr("type", "func") + ListComprehension + TableComprehension + TableLit + ("(" * SL * Expr * SL * ")"),

		-- basic control statement
		Case = K"case" * SL1 * Expr * SL * "{" * SL * (CaseMatch * SL)^0 * "}",
		CaseMatch = Expr * SL * "->" * SL * statement, 
		IfElse = K"if" * SL * "(" * SL * Expr * SL * ")" * SL * statement * (SL * K"else" * SL * statement)^-1,
		For = K"for" * SL * "(" * SL * Generator * SL * ")" * SL * statement,
		While = K"while" * SL * "(" * SL * Expr * SL * ")" * SL * statement,
		TryCatch = K"try" * (#P"{"+SL1) * statement * SL * K"catch" * (#P"{" + SL1) * statement,
		Return = Cg(K"yield" + K"return", "type") * (SS * Cg(Expr, "expr"))^-1 + Cg(K"break", "type");

		-- declaration/trait/class
		type_ = K"int" + K"float" + K"string" + K"list" + K"table" + K"bool" + id,
		FuncType = "(" * SL * Cg(TypeList, "input")^-1 * SL * ")" * SL * "->" * SL * Cg(typedef, "ret") * Attr("type", "func"),
		Trait = "{" * SL * Cg(TypeList, "list") * SL * "}" * Attr("type", "trait"),
		typedef = type_ + FuncType + Trait, 
		TypeOne = idName * SS * (":" * SL * Cg(typedef, "type"))^-1,
		TypeList = TypeOne * (SS * "," * SL * TypeOne)^0,
		IdDecl = (Cg(S".:", "scope") * SL)^-1 * idName * (SS * ":" * SL * Cg(typedef, "type"))^-1,
		IdList = IdDecl * (SS * ',' * SL * IdDecl)^0,
		Class = Cg(K"class" + K"trait", "type") * SL * idName * SL * 
			Cg(Ct( (K"extends" * SL1 * id * SL)^0 ), "parent") * Cg(CompoundStatement, "stat"),

		Decl = Cg(K'val' + K'var', "const") * SL1 * Cg(IdList, "list") * SS * ('=' * SL * Cg(Expr, "init"))^-1,

		SimpleStatement = Cg(Decl, "stat") * Attr("type", "decl") + 
			Cg(For, "stat") * Attr("type", "for") + Cg(While, "stat") * Attr("type","while") + Cg(Case, "stat") * Attr("type", "case") + 
			Cg(TryCatch, "stat") * Attr("type", "try") + Cg(IfElse, "stat") * Attr("type", "if") + Cg(Return, "stat") * Attr("type", "return") + 
			Cg(Class, "stat") * Attr("type", "class") + Cg(Expr, "stat") * Attr("type", "expr"), 
		CompoundStatement = P"{" * SL * (SS * statement * Separator)^0 * SS * statement^-1 * SS * "}",
		empty = SS * separator * SS,
		statement = empty + SimpleStatement + CompoundStatement,
		--Statement1 = SimpleStatement / "simple" + CompoundStatement /"compound",
		Block = shebang^-1 * SL * Ct(( statement * Separator * SS)^0 * SS * (statement^-1) * SS * Cp()) * P(1)^0, -- Number/tonumber * space

		-- at this point, it should collect all keywords
		id = C(charset * charnum^0 - keywords),
		idName = Cg(id, "name")
	}
end

local grammar
if setfenv then
	grammar = setfenv(build_grammar, setmetatable({}, mt))()
else
end

local action_prototype = {
	create = function(name) return function(...) return name, {...} end end;
	decl_scoped = function(s) return end 
}

local create_state = function()
	action = action_prototype
end

local parse = function(str)
	create_state()
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
