local parser = require "../lang.grammar"
local parse = parser.parse

print(parse[===[
var a , b , c = -1.9
val a , b , c = -1.9;
for ( a, b <- 1.9 to 2 by 0.1 ) ;

---[[
case a {
	b -> 1
	c -> 1
	d -> {1;1;1}
}
--]]

while ( a )
	t;

while ( a ){
	(((a)))
	a 1, 2,2
}
]===])

print(parse[==[
(a[1] b) d c;
a ^ b ^ c + f1 b + ar1.k + ar2:p * f2 c,d,e * 100
]==])

print(parse[==[
(a[1]);
9+(9,9=9=9)-(a c,d,d)
--(x[a 1] 10)
x, y = (x0, y0) + (x1, y1)
try{
 for ( a <- 1 to 2 ) {}
 }
catch{
}
]==])

--[====[
-- comments

--[=[0.8e-1 ]] 
--[[
	--
]]

-- import
[[
import table	--> local table = table --> ideally, search for current global scope and module scope, so it could end up with require "table"
import * from my.module --> require "my.module" --> introduce func1, func2
import func1, func2 --> local func1, func2 = func1, func2

import mod from my.module --> local mod = my.module.mod
import func1, func2 from my.module --> local func1, func2 = my.module.func1, my.module.func2

val lpeg = require "lpeg"  --> local lpeg = require "lpeg"
]]

-- literals
local ret, res = parse([[
9
8.
7.0e-2
6.2e+2
4e2

"A"
"A""B"
"A\"B"
"A\"B"

() => 9
(x) => x*9
(x :Int) => x*10
(x :Nil=>Int) => x
]])

-- expression
ret, res = parse([[
]])

-- control flow
[[
	val r = [ x for x in 1,10 ]
	val m1 = { x=x for x in 1,10 }
	val m2 = { x=[x] for x in 1,10 }
	for(i <- [1,10]) {
	}
	for(i <- r) {
	}
	for(k=v <- m1) {
	}
	for(k=[v] <- m1) {
	}
]]

-- comprehension/decoration
[[
	[ 1,10 ]
	[ 1,10 by 2 ]
	[ x for x in 1,10 ]
	{ x=x for x in 1,10 }
	{ x={x,x} for x in 1,10 }
	{ x=[y for y in 1,x if x%2 == 0] for x in 1,10 }
]]

-- function

-- scope


-- class
[[
	class Base
	class A extends Base
]]

-- trait
[[
	trait T {
	}

	class A extends T {
	}
]]

-- catagory/extend the existing classes
[[
	class A{
	}

	class A(ext) {
	}
]]




--]====]
