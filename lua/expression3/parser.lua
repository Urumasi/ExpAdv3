--[[
	   ____      _  _      ___    ___       ____      ___      ___     __     ____      _  _          _        ___     _  _       ____   
	  F ___J    FJ  LJ    F _ ", F _ ",    F ___J    F __".   F __".   FJ    F __ ]    F L L]        /.\      F __".  FJ  L]     F___ J  
	 J |___:    J \/ F   J `-' |J `-'(|   J |___:   J (___|  J (___|  J  L  J |--| L  J   \| L      //_\\    J |--\ LJ |  | L    `-__| L 
	 | _____|   /    \   |  __/F|  _  L   | _____|  J\___ \  J\___ \  |  |  | |  | |  | |\   |     / ___ \   | |  J |J J  F L     |__  ( 
	 F L____:  /  /\  \  F |__/ F |_\  L  F L____: .--___) \.--___) \ F  J  F L__J J  F L\\  J    / L___J \  F L__J |J\ \/ /F  .-____] J 
	J________LJ__//\\__LJ__|   J__| \\__LJ________LJ\______JJ\______JJ____LJ\______/FJ__L \\__L  J__L   J__LJ______/F \\__//   J\______/F
	|________||__/  \__||__L   |__|  J__||________| J______F J______F|____| J______F |__L  J__|  |__L   J__||______F   \__/     J______F 

	::Base Parser::
	```````````````
	A parser is the logical structure used to turn tokens into instructions that.
	
	:::Syntax Grammar:::
	```````````````````
		I have based this off the one from E2.

		:::Key:::
		* ε is the end-of-file
		* E? matches zero or one occurrences of T (and will always match one if possible)
		* E* matches zero or more occurrences of T (and will always match as many as possible)
		* E F matches E (and then whitespace) and then F
		* E / F tries matching E, if it fails it matches F (from the start location)
		* &E matches E, but does not consume any input.
		* !E matches everything except E, and does not consume any input.
		
		:::Root:::
			Root ← Stmt1(("" / " ") Stmt1)* ε

		:::Statments:::
			Stmt1 ← ("try" Block "(" Var ")" Block)? Stmt2
			Stmt2 ← ("if" Cond Block Stmt3)? Stmt5
			Stmt3 ← ("elseif" Cond Block Stmt3)? Stmt4
			Stmt4 ← ("else" Block)
			Stmt5 ← ("for" "(" Type "=" Expr1 ")" Block)? Stmt6
			Stmt6 ← (("server" / "client") Block)? Stmt7
			Stmt7 ← "global"? (type (Var("," Var)* "="? (Expr1? ("," Expr1)*)))? Stmt8
			Stmt8 ← (type (Var("," Var)* ("=" / "+=" / "-=" / "/=" / "*=")? (Expr1? ("," Expr1)*)))? Stmt9
			Stmt9 ← ("delegate" "(" (Type ((",")?)*)?) ")" ("{")? "return" Num ("}")?)? Stmt10
			Stmt10 ← ("return" (Expr1 ((","")?)*)?)?)?

		:::Expressions:::
			Expr1 ← (Expr1 "?" Expr1 ":" Expr1)? Expr2
			Expr2 ← (Expr3 "||" Expr3)? Expr3
			Expr3 ← (Expr4 "&&" Expr4)? Expr4
			Expr4 ← (Expr5 "^^" Expr5)? Expr5
			Expr5 ← (Expr6 "|" Expr6)? Expr6
			Expr6 ← (Expr7 "&" Expr7)? Expr7
			Expr7 ← (Expr8 ("==" / "!=") (Values / Expr1))? Expr8
			Expr8 ← (Epxr9 (">" / "<" / " >=" / "<=") Expr1)? Expr9
			Expr9 ← (Epxr10 "<<" Expr10)? Expr10
			Expr10 ← (Epxr11 ">>" Expr11)? Expr11
			Expr11 ← (Epxr12 "+" Expr12)? Expr12
			Expr12 ← (Epxr13 "-" Expr13)? Expr13
			Expr13 ← (Epxr14 "/" Expr14)? Expr14
			Expr14 ← (Epxr15 "*" Expr15)? Expr15
			Expr15 ← (Epxr16 "^" Expr16)? Expr16
			Expr16 ← (Epxr17 "%" Expr17)? Expr17
			Expr17 ← ("+" Expr22)? Exp18
			Expr18 ← ("-" Expr22)? Exp19
			Expr19 ← ("!" Expr22)? Expr20
			Expr20 ← ("#" Expr22)? Expr21
			Expr21 ← ("("type")" Expr1)? Expr22
			Expr22 ← ("(" Expr1 ")" (Trailing)?)? Expr23
			Expr23 ← (Library "." Function  "(" (Expr1 ((",")?)*)?) ")")? Expr24
			Expr24 ← (Var (Trailing)?)? Expr25
			Expr25 ← ("new" Type "(" (Expr1 ((","")?)*)?) ")")? Expr25
			Expr26 ← ("Function" Perams Block1)? Expr27
			Expr27 ← Expr28? Error
			Expr28 ← (String / Number / "true" / "false", "void")?

		:::Syntax:::
			Cond 		← "(" Expr1 ")"
			Block 		← "{" (Stmt1 (("" / " ") Stmt1)*)? "}"
			Values 		← "[" Expr1 ("," Expr1)* "]"
			Raw 		← (Str / Num / Bool)
			Trailing 	← (Method / Get /Call)?
			Method 		← (("." Method "(" (Expr1 ((","")?)*)?) ")")
			Get 		← ("[" Expr1 ("," Type)? "]")
			Call 		← ("(" (Expr1 ((","")?)*)?) ")")?
			Perams 		← ("(" (Type Var (("," Type Var)*)?)? ")")

]]

local PARSER = {}
PARSER.__index = PARSER

function PARSER.New()
	return setmetatable({}, PARSER)
end

function PARSER:Initalize(instance)
	self.__pos = 0
	self.__depth = 0
	self.__scope = 0
	self.__instructions = {}

	self.__token = instance.tokens[0]
	self.__next = instance.tokens[1]
	self.__total = #instance.tokens
	self.__tokens = instance.tokens
	self.__script = instance.script

	self.__tasks = {}

	self.__directives = {}
	self.__directives.inport = {}
	self.__directives.outport = {}

end

function PARSER.Run(this)
	--TODO: PcallX for stack traces on internal errors?
	local status, result = pcall(self._Run, this)

	if status then
		return true, result
	end

	if type(result) == "table" then
		return false, result
	end

	local err = {}
	err.state = "internal"
	err.msg = result

	return false, err
end

function PARSER._Run(this)
	local result = {}
	result.instruction = self:Root()
	result.script = self.__script
	result.tasks = self.__tasks
	result.tokens = self.__tokens
	result.directives = self.__directives

	return result
end

function PARSER:Throw(token, msg, fst, ...)
	local err = {}

	if fst then
		msg = string.format(msg, fst, ...)
	end

	err.state = "parser"
	err.char = token.char
	err.line = token.line
	err.msg = msg

	error(err,0)
end

--[[
]]

function PARSER.Next(this)
	self.__pos = self.__pos + 1
	
	self.__token = self.__tokens[self.__pos]
	self.__next = self.__tokens[self.__pos + 1]

	if self.__pos > self.__total then
		return false
	end

	return true
end

function PARSER.HasTokens(this)
	return self.__next ~= nil
end

function PARSER:CheckToken(type, ...)
	if self.__pos < self.__total then
		local tkn = self.__next

		for _, t in pairs({type, ...}) do
			if t == tkn.type then
				return true
			end
		end
	end

	return false
end

function PARSER:Accept(type, ...)
	if self:CheckToken(type, ...) then
		self:Next()
		return true
	end

	return false
end

function PARSER:AcceptWithData(type, data)
	if self:CheckToken(type) and self.__next.data == data then
		self:Next()
		return true
	end

	return false
end

function PARSER.GetTokenData(this)
	return self.__token.data
end

function PARSER:GetToken(pos)
	if pos >= self.__total then
		return self.__tokens[pos]
	end
end

function PARSER:StepBackward(steps)
	
	if not steps then
		steps = 1
	end

	local pos = self.__pos - (steps + 1)

	if pos == 0 then
		self.__pos = 0
		self.__token = self.__tokens[0]
		self.__next = self.__tokens[1]
		return
	end

	if pos > self.__total then
		pos = self.__total
	end

	self.__pos = pos

	self:Next()
end

function PARSER.GetFirstTokenOnLine(this)
	for i = self.__pos, 1, -1 do
		local tkn = self.__tokens[i]

		if tkn.newLine then
			return tkn
		end
	end

	return self.__tokens[1]
end

function PARSER:StatmentContains(token, type)
	local i = self.__pos

	while i < self.__total do
		local tkn = self.__tokens[i]

		if not tkn then
			return
		end

		if tkn.type == "sep" or tkn.line ~= token.line then
			return
		end

		if tkn.type == type then
			return tkn
		end

		i = i + 1
	end
end

function PARSER:LastInStatment(token, type)
	local last
	local i = token.index

	while i <= self.__total do
		local tkn = self.__tokens[i]
		
		if not tkn then
			break
		end

		if tkn.type == "sep" or tkn.line ~= token.line then
			break
		end

		if tkn.type == type then
			last = tkn
		end

		i = i + 1
	end

	return last
end

--[[
]]

function PARSER.Require( this, type, msg, ... )
	if not self:Accept(type) then
		self:Throw( self.__token, msg, ... )
	end
end

function PARSER.Exclude( this, tpye, msg, ... )
	if self:Accept(type) then
		self:Throw( self.__token, msg, ... )
	end
end

function PARSER:ExcludeWhiteSpace(msg, ...)
	if not self:HasTokens() then 
		self:Throw( self.__token, msg, ... )
	end
end

--[[
]]

function PARSER:StartInstruction(_type, token)
	if not type(_type) == "string" then
		debug.Trace()
		error("PARSER:StartInstruction got bad instruction type.", _type)
	elseif not type(token) == "table" then
		debug.Trace()
		error("PARSER:StartInstruction got bad instruction token.", token)
	end

	local inst = {}
	inst.type = _type
	inst.result = "void"
	inst.rCount = 0
	inst.token = token
	inst.char = token.char
	inst.line = token.line
	inst.depth = self.__depth
	inst.scope = self.__scope
	self.__depth = self.__depth + 1

	return inst
end

function PARSER:QueueReplace(inst, token, str)
	local op = {}

	op.token = token
	op.str = str
	op.inst = inst

	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	tasks.replace = op

	return op
end

function PARSER:QueueRemove(inst, token)
	local op = {}

	op.token = token
	op.inst = inst

	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	tasks.remove = op

	return op
end

function PARSER:QueueInjectionBefore(inst, token, str, ...)
	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	if not tasks.prefix then
		tasks.prefix = {}
	end

	local t = {str, ...}

	for i = 1, #t do
		local op = {}
	
		op.token = token
		op.str = t[i]
		op.inst = inst

		tasks.prefix[#tasks.prefix + 1] = op
	end

	return r
end

function PARSER:QueueInjectionAfter(inst, token, str, ...)
	local op = {}
	
	op.token = token
	op.str = str
	op.inst = inst

	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	if not tasks.postfix then
		tasks.postfix = {}
	end

	local r = {}
	local t = {str, ...}

	for i = 1, #t do
		local op = {}
	
		op.token = token
		op.str = t[i]
		op.inst = inst

		r[#r + 1] = op
		tasks.postfix[#tasks.postfix + 1] = op
	end

	return r
end

function PARSER:SetEndResults(inst, type, count)
	inst.type = type
	inst.rCount = count or 1
end

function PARSER:EndInstruction(inst, instructions)
	inst.instructions = instructions

	inst.final = self.__token

	self.__depth = self.__depth - 1

	--print("PARSER->" .. inst.type .. "->#" .. #inst.instructions)

	return inst
end

--[[
]]

function PARSER.Root(this)
	local seq = self:StartInstruction("seq", self.__tokens[1])

	local stmts = self:Statments(false)

	return self:EndInstruction(seq, stmts)
end

function PARSER:Block_1(_end, lcb)
	self:ExcludeWhiteSpace( "Further input required at end of code, incomplete statement" )
	
	if self:Accept("lcb") then
		
		local seq = self:StartInstruction("seq", self.__token)

		if lcb then
			self:QueueRemove(seq, self.__token)
			self:QueueInjectionAfter(seq, self.__token, lcb)
		end

		local stmts = {}

		if not self:CheckToken("rcb") then
			self.__scope = self.__scope + 1

			stmts = self:Statments(true)

			self.__scope = self.__scope - 1
		end

		if not self:Accept("rcb") then
			self:Throw(self.__token, "Right curly bracket (}) missing, to close block")
		end
		
		self:QueueReplace(seq, self.__token, _end and "end" or "")

		return self:EndInstruction(seq, stmts)
	end

	do
		local seq = self:StartInstruction("seq", self.__next)

		if lcb then
			self:QueueInjectionAfter(seq, self.__token, lcb)
		end

		self.__scope = self.__scope + 1

		local stmt = self:Statment_1()

		self.__scope = self.__scope - 1

		if _end then
			self:QueueInjectionAfter(seq, stmt.final, "end")
		end

		return self:EndInstruction(seq, { stmt })
	end
end

--[[

]]

function PARSER:Directive_NAME(token, directive)
	self:Require("str", "String expected to follow directive @name")

	if self.FirstStatment then
		self:Throw(token, "Directive @name must appear at top of your code")
	elseif self.__directives.name then
		self:Throw(token, "Directive @name must not appear twice.")
	end

	self.__directives.name = self.__token.data

	self:QueueRemove({}, self.__token)
end

function PARSER:Directive_MODEL(token, directive)
	self:Require("str", "String expected to follow directive @model")

	if self.FirstStatment then
		self:Throw(token, "Directive @model must appear at top of your code")
	elseif self.__directives.model then
		self:Throw(token, "Directive @model must not appear twice.")
	end

	self.__directives.model = self.__token.data
	
	self:QueueRemove({}, self.__token)
end

function PARSER:Directive_INPUT(token, directive)
	self:Require("typ", "Class expected for inport type, after @input")

	local inst = self:StartInstruction("inport", token)

	inst.class = self.__token.data

	local class_obj = EXPR_LIB.GetClass(inst.class)

	if not class_obj.wire_in_class then
		self:Throw(token, "Invalid wired port, class %s can not be used for wired input.", class_obj.name)
	end

	self:QueueRemove(inst, self.__token)

	local variables = {}

	self:Require("var", "Variable('s) expected after class for inport name.")
	
	variables[1] = self.__token

	self:QueueRemove(inst, self.__token)

	while self:Accept("com") do
		self:QueueRemove(inst, self.__token)

		self:Require("var", "Variable expected after comma (,).")

		variables[#variables + 1] = self.__token

		self:QueueRemove(inst, self.__token)
	end

	inst.variables = variables

	inst.wire_type = class_obj.wire_in_class

	inst.wire_func = class_obj.wire_in_func

	return self:EndInstruction(inst, {})
end

function PARSER:Directive_OUTPUT(token, directive)
	self:Require("typ", "Class expected for outport type, after @input")

	local inst = self:StartInstruction("outport", token)

	inst.class = self.__token.data

	local class_obj = EXPR_LIB.GetClass(inst.class)

	if not class_obj.wire_out_class then
		self:Throw(token, "Invalid wired port, class %s can not be used for wired output.", class_obj.name)
	end

	self:QueueRemove(inst, self.__token)

	local variables = {}

	self:Require("var", "Variable('s) expected after class for outport name.")
	
	variables[1] = self.__token

	self:QueueRemove(inst, self.__token)

	while self:Accept("com") do
		self:QueueRemove(inst, self.__token)

		self:Require("var", "Variable expected after comma (,).")

		variables[#variables + 1] = self.__token

		self:QueueRemove(inst, self.__token)
	end

	inst.variables = variables

	inst.wire_type = class_obj.wire_out_class

	inst.wire_func = class_obj.wire_out_func

	inst.wire_func2 = class_obj.wire_in_func

	return self:EndInstruction(inst, {})
end

--[[
]]

function PARSER:Statments(block, call)
	local sep = false
	local stmts = {}

	call = call or self.Statment_0

		while true do

			local stmt = call(this)

			stmts[#stmts + 1] = stmt

			local seperated = self:Accept("sep")

			if not stmt then
				break
			end

			if block and self:CheckToken("rcb") then
				break
			end

			if not self:HasTokens() then
				break
			end

			local pre = stmts[#stmts - 1]

			if pre then
				if pre.line == stmt.line and not sep then
					self:Throw(stmt.token, "Statements must be separated by semicolon () or newline")
				end
			end

			if stmt.type == "return" then
				self:Throw(stmt.final, "Statement can not appear after return.")
			elseif stmt.type == "continue" then
				self:Throw(stmt.final, "Statement can not appear after continue.")
			elseif stmt.type == "break" then
				self:Throw(stmt.final, "Statement can not appear after break.")
			end

			sep = seperated
		end

 	return stmts
end

--[[
]]

function PARSER.Statment_0(this)
	local dirLine

	while self:Accept("dir") do
		local token = self.__token
		dirLine = self.__token.line

		self:QueueRemove({}, token)

		if not self:Accept("var") then
			self:Throw(token, "Directive name exspected after @")
		end

		local directive = self.__token.data

		self:QueueRemove({}, self.__token)

		local func = this["Directive_" .. string.upper(directive)]

		if  not func then
			self:Throw(token, "No such directive @%s", directive)
		end

		local instr = func(this, token, directive)

		sep = self:Accept("sep")

		if sep then
			self:QueueRemove({}, self.__token)
		end

		if instr then
			return instr
		end

		if !self:HasTokens() then
			return
		end
	end

	if not self.FirstStatment then
		self.FirstStatment = self.__token
	end

	if self:CheckToken("cls") then
		return self:ClassStatment_0()
	end

	local stmt = self:Statment_1()

	if dirLine and (not sep or direLine == stmt.line) then
		self:Throw(stmt.token, "Statements must be separated by semicolon () or newline")
	end

	return stmt
end

--
function PARSER.Statment_1(this)
	if self:Accept("try") then
		local inst = self:StartInstruction("try", self.__token)

		inst.protected = self:Block_1(true, "function()")

		self:Require("cth", "Catch expected after try statment, for try catch")

		inst.__catch = self.__token

		self:Require("lpa", "Left parenthesis (( ) expected after catch.")

		inst.__lpa = self.__token

		self:Require("var", "Variable expected for error object, catch(variable)")

		inst.__var = self.__token

		self:Require("rpa", "Right parenthesis ( )) expected to end catch.")

		inst.__rpa = self.__token

		inst.catch = self:Block_1(false, "then")

		return self:EndInstruction(inst, {})
	end

	return self:Statment_2()
end

function PARSER.Statment_2(this)
	if self:Accept("if") then
		local inst = self:StartInstruction("if", self.__token)

		inst.condition = self:GetCondition()
		
		inst.block = self:Block_1(false, "then")

		inst._else = self:Statment_3()

		self:QueueInjectionAfter(inst, self.__token, "end")

		return self:EndInstruction(inst, {})
	end

	return self:Statment_5()
end

function PARSER.Statment_3(this)
	if self:Accept("eif") then
		local inst = self:StartInstruction("elseif", self.__token)

		inst.condition = self:GetCondition()

		inst.block = self:Block_1(false, "then")

		inst._else = self:Statment_3()

		return self:EndInstruction(inst, {})
	end

	return self:Statment_4()
end

function PARSER.Statment_4(this)
	if self:Accept("els") then
		local inst = self:StartInstruction("else", self.__token)

		inst.block = self:Block_1(false, "")

		return self:EndInstruction(inst, {})
	end
end

--[[
]]


function PARSER.Statment_5(this)
	if self:Accept("for") then
		local inst = self:StartInstruction("for", self.__token)

		self:Require("lpa", "Left parenthesis (( ) expected after for.")

		self:QueueRemove(inst, self.__token)

		self:Require("typ", "Class expected for loop itorator")

		inst.class = self.__token.data

		self:QueueRemove(inst, self.__token)

		self:Require("var", "Assigment expected for loop definition.")

		inst.variable = self.__token

		self:Require("ass", "Assigment expected for loop definition.")

		inst.__ass = self.__token

		local expressions = {}

		expressions[1] = self:Expression_1()

		self:Require("sep", "Seperator expected after loop decleration.")

		self:QueueReplace(inst, self.__token, (","))

		inst.__sep1 = self.__token

		expressions[2] = self:Expression_1()

		if self:Accept("sep") then
			self:QueueReplace(inst, self.__token, (","))
			
			inst.__sep2 = self.__token

			expressions[3] = self:Expression_1()
		end

		self:Require("rpa", "Right parenthesis ( )) expected to close cloop defintion.")

		self:QueueRemove(inst, self.__token)

		inst.stmts = self:Block_1(true, "do")

		return self:EndInstruction(inst, expressions)
	end

	return self:Statment_6()
end

function PARSER.Statment_6(this)
	if self:Accept("sv") then
		local inst = self:StartInstruction("server", self.__token)

		self:QueueInjectionBefore(inst, self.__token, "if")

		self:QueueReplace(inst, self.__token, "(SERVER)")

		inst.block = self:Block_1(true, "then")

		return self:EndInstruction(inst, {})
	end

	if self:Accept("cl") then
		local inst = self:StartInstruction("client", self.__token)

		self:QueueInjectionBefore(inst, self.__token, "if")

		self:QueueReplace(inst, self.__token, "(CLIENT)")

		inst.block = self:Block_1(true, "then")

		return self:EndInstruction(inst, {})
	end

	return self:Statment_7()
end

--[[
]]

function PARSER.Statment_7(this)
	if self:Accept("glo") then
		local inst = self:StartInstruction("global", self.__token)

		self:QueueRemove(inst, self.__token)

		self:Require("typ", "Class expected after global.")
		
		local type = self.__token.data

		inst.class = type

		self:QueueRemove(inst, self.__token)

		local variables = {}

		self:Require("var", "Variable('s) expected after class for global variable.")
		variables[1] = self.__token
		--self:QueueInjectionBefore(inst, self.__token, "GLOBAL", ".")

		while self:Accept("com") do
			self:Require("var", "Variable expected after comma (,).")
			variables[#variables + 1] = self.__token
			--self:QueueInjectionBefore(inst, self.__token, "GLOBAL", ".")
		end

		local expressions = {}

		if self:Accept("ass") then
			self:ExcludeWhiteSpace( "Assignment operator (=), must not be preceded by whitespace." )
			
			expressions[1] = self:Expression_1()

			while self:Accept("com") do
				self:ExcludeWhiteSpace( "comma (,) must not be preceded by whitespace." )
				expressions[#expressions + 1] = self:Expression_1()
			end
		end

		inst.variables = variables

		return self:EndInstruction(inst, expressions)
	end

	if self:Accept("typ") then
		local inst = self:StartInstruction("local", self.__token)
		
		local type = self.__token.data

		if type == "f" and self:CheckToken("typ") then
			self:StepBackward(1)
			return self:Statment_8()
		end

		self:QueueReplace(inst, self.__token, "local")

		inst.class = type
		
		local variables = {}

		self:Require("var", "Variable('s) expected after class for variable.")
		variables[1] = self.__token

		while self:Accept("com") do
			self:Require("var", "Variable expected after comma (,).")
			variables[#variables + 1] = self.__token
		end
		
		local expressions = {}

		if self:Accept("ass") then
			self:ExcludeWhiteSpace( "Assignment operator (=), must not be preceded by whitespace." )
			
			expressions[1] = self:Expression_1()

			while self:Accept("com") do
				self:ExcludeWhiteSpace( "comma (,) must not be preceded by whitespace." )
				expressions[#expressions + 1] = self:Expression_1()
			end
		end

		inst.variables = variables

		return self:EndInstruction(inst, expressions)
	end

	return self:Statment_8()
end

function PARSER.Statment_8(this)
	if self:Accept("var") then
		
		if not self:CheckToken("com", "ass", "aadd", "asub", "adiv", "amul") then
			self:StepBackward(1)
		else
			local inst = self:StartInstruction("ass", self.__token)
			
			local variables = {}
		
			variables[1] = self.__token

			while self:Accept("com") do
				self:Require("var", "Variable expected after comma (,).")
				variables[#variables + 1] = self.__token
			end
			
			inst.variables = variables

			local expressions = {}

			if self:Accept("ass") then
				self:ExcludeWhiteSpace( "Assignment operator (=), must not be preceded by whitespace." )
				
				expressions[1] = self:Expression_1()

				while self:Accept("com") do
					self:ExcludeWhiteSpace( "comma (,) must not be preceeded by whitespace." )
					expressions[#expressions + 1] = self:Expression_1()
				end

				return self:EndInstruction(inst, expressions)
			end

			if self:Accept("aadd", "asub", "amul", "adiv") then
				inst.__operator = self.__token

				inst.type = self.__token.type

				self:ExcludeWhiteSpace("Assignment operator (%s), must not be preceded by whitespace.", self.__token.data)
				
				expressions[1] = self:Expression_1()

				while self:Accept("com") do
					self:ExcludeWhiteSpace( "comma (,) must not be preceeded by whitespace." )
					expressions[#expressions + 1] = self:Expression_1()
				end

				if #expressions ~= #variables then
					-- TODO: Better error message.
					self:ExcludeWhiteSpace("Invalid arithmetic assignment, not all variables are given values.")
				end

				return self:EndInstruction(inst, expressions)
			end

			self:Throw(inst.token, "Variable can not be preceded by whitespace.")
		end
	end

	return self:Statment_9()
end

function PARSER.Statment_8(this)
	if self:Accept("del") then
		local inst = self:StartInstruction("delegate", self.__token)

		self:QueueRemove(inst, self.__token)
		
		self:Require("typ", "Return class expected after delegate.")

		inst.resultClass = self.__token.data

		self:QueueRemove(inst, self.__token)

		self:Require("var", "Delegate name expected after delegate return class.")

		inst.variable = self.__token.data

		self:QueueRemove(inst, self.__token)

		self:Require("lpa", "Left parenthesis (( ) expected to open delegate peramaters.")

		self:QueueRemove(inst, self.__token)

		local classes = {}

		if not self:CheckToken("rpa") then

			while true do
				self:Require("typ", "Peramater type expected for peramater.")

				self:QueueRemove(inst, self.__token)

				classes[#classes + 1] = self.__token.data

				if not self:Accept("com") then
					break
				end

				self:QueueRemove(inst, self.__token)
			end

		end
		
		inst.peramaters = classes

		self:Require("rpa", "Right parenthesis ( ) expected to close delegate peramaters.")

		self:QueueRemove(inst, self.__token)

		local lcb = self:Accept("lcb")

		if lcb then
			self:QueueRemove(inst, self.__token)
		end

		self:Require("ret", "Delegate body must be return followed by return count")
		
		self:QueueRemove(inst, self.__token)

		self:Require("num", "Delegate body must be return followed by return count as number.")

		self:QueueRemove(inst, self.__token)

		inst.resultCount = self.__token.data

		if self:Accept("sep") then
			self:QueueRemove(inst, self.__token)
		end

		if lcb then
			self:Require("rcb", "Right curly bracket ( }) expected to close delegate.")

			self:QueueRemove(inst, self.__token)
		end

		return self:EndInstruction(inst, {})
	end

	return self:Statment_10()
end

function PARSER.Statment_10(this)
	if self:AcceptWithData("typ", "f") then

		local inst = self:StartInstruction("funct", self.__token)

		self:QueueReplace(inst, self.__token, "function")
		
		self:Require("typ", "Return class expected after user function.")

		inst.resultClass = self.__token.data

		self:QueueRemove(inst, self.__token)

		self:Require("var", "Function name expected after user function return class.")

		inst.variable = self.__token.data

		self:QueueRemove(inst, self.__token)

		local perams, signature = self:InputPeramaters(inst)
		
		inst.perams = perams
		
		inst.signature = signature

		inst.stmts = self:Block_1(true, " ")

		inst.__end = self.__token

		return self:EndInstruction(inst, {})
	end

	return self:Statment_11()
end

function PARSER.Statment_11(this)
	if self:Accept("ret") then
		local expressions = {}
		local inst = self:StartInstruction("return", self.__token)

		if not self:CheckToken("sep", "rcb") then
			while true do
				expressions[#expressions + 1] = self:Expression_1()

				if not self:HasTokens() then
					break
				end

				if not self:Accept("com")) then --"sep", "rcb") then
					break
				end

				-- self:Require("com", "Comma (,) expected to seperate return values.")
			end
		end

		return self:EndInstruction(inst, expressions)
	end

	local expr = self:Expression_1()

	if expr and self:CheckToken("lsb") then
		expr = self:Statment_12(expr)
	end

	return expr
end

function PARSER:Statment_12(expr)
	if self:Accept("lsb") then
		local inst = self:StartInstruction("set", self.__token)

		inst.__lsb = self.__token

		local expressions = {}

		expressions[1] = expr

		expressions[2] = self:Expression_1()

		if self:Accept("com") then
			self:QueueRemove(inst, self.__token)

			self:Require("typ", "Class expected for index operator, after coma (,).")

			inst.class = self.__token
		end

		self:Require("rsb", "Right square bracket (]) expected to close index operator.")

		inst.__rsb = self.__token

		self:Require("ass", "Assigment operator (=) expected after index operator.")

		inst.__ass = self.__token

		expressions[3] = self:Expression_1()

		return self:EndInstruction(inst, expressions)
	end
end

--[[
]]

function PARSER.Expression_1(this)
	local expr = self:Expression_2()

	while self:Accept("qsm") do
		local inst = self:StartInstruction("ten", self.__token)

		inst.__and = self.__token

		local expr2 = self:Expression_2()

		self:Require("col", "colon (:) expected for ternary operator.")

		inst.__or = self.__token

		local expr3 = self:Expression_2()

		expr = self:EndInstruction(inst, {expr, expr2, expr3})
	end

	return self:Expression_Trailing(expr)
end

function PARSER.Expression_2(this)
	local expr = self:Expression_3()

	while self:Accept("or") do
		local inst = self:StartInstruction("or", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_3()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_3(this)
	local expr = self:Expression_4()

	while self:Accept("and") do
		local inst = self:StartInstruction("and", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_4()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_4(this)
	local expr = self:Expression_5()

	while self:Accept("bxor") do
		local inst = self:StartInstruction("bxor", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_5()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_5(this)
	local expr = self:Expression_6()

	while self:Accept("bor") do
		local inst = self:StartInstruction("bor", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_6()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_6(this)
	local expr = self:Expression_7()

	while self:Accept("band") do
		local inst = self:StartInstruction("band", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_7()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_7(this)
	local expr = self:Expression_8()

	while self:CheckToken("eq", "neq") do
		if self:Accept("eq") then
			local eqTkn = self.__token

			if self:Accept("lsb") then
				local inst = self:StartInstruction("eq_mul", expr.token)
				
				inst.__operator = eqTkn

				inst.__listStart = self.__token

				local expressions = {}

				expressions[1] = expr

				expressions[2] = self:Expression_1()

				while self:Accept("com") do
					expressions[#expressions + 1] = self:Expression_1()
				end

				expr = self:EndInstruction(ist, expressions)
			else
				local inst = self:StartInstruction("eq", self.__token)

				inst.__operator = self.__token

				local expr2 = self:Expression_8()

				expr = self:EndInstruction(inst, {expr, expr2})
			end
		elseif self:Accept("neq") then
			local eqTkn = self.__token

			if self:Accept("lsb") then
				local inst = self:StartInstruction("neq_mul", expr.token)
				
				inst.__operator = eqTkn

				inst.__listStart = self.__token

				local expressions = {}

				expressions[1] = expr

				expressions[2] = self:Expression_1()

				while self:Accept("com") do
					expressions[#expressions + 1] = self:Expression_1()
				end

				expr = self:EndInstruction(inst, expressions)
			else
				local inst = self:StartInstruction("neq", self.__token)

				inst.__operator = self.__token

				local expr2 = self:Expression_8()

				expr = self:EndInstruction(ist, {expr, expr2})
			end
		end
	end

	return expr
end

function PARSER.Expression_8(this)
	local expr = self:Expression_9()

	while self:CheckToken("lth", "leq", "gth", "geq") do
		if self:Accept("lth") then
			local inst = self:StartInstruction("lth", expr.token)

			inst.__operator = self.__token

			local expr2 = self:Expression_1()

			expr = self:EndInstruction(inst, {expr, expr2})
		elseif self:Accept("leq") then
			local inst = self:StartInstruction("leq", expr.token)

			inst.__operator = self.__token

			local expr2 = self:Expression_1()

			expr = self:EndInstruction(inst, {expr, expr2})
		elseif self:Accept("gth") then
			local inst = self:StartInstruction("gth", expr.token)

			inst.__operator = self.__token

			local expr2 = self:Expression_1()

			expr = self:EndInstruction(inst, {expr, expr2})
		elseif self:Accept("geq") then
			local inst = self:StartInstruction("geq", expr.token)

			inst.__operator = self.__token

			local expr2 = self:Expression_1()

			expr = self:EndInstruction(inst, {expr, expr2})
		end
	end

	return expr
end

function PARSER.Expression_9(this)
	local expr = self:Expression_10()

	while self:Accept("bshl") do
		local inst = self:StartInstruction("bshl", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_10()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_10(this)
	local expr = self:Expression_11()

	while self:Accept("bshr") do
		local inst = self:StartInstruction("bshr", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_11()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_11(this)
	local expr = self:Expression_12()

	while self:Accept("add") do
		local inst = self:StartInstruction("add", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_12()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_12(this)
	local expr = self:Expression_13()

	while self:Accept("sub") do
		local inst = self:StartInstruction("sub", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_13()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_13(this)
	local expr = self:Expression_14()

	while self:Accept("div") do
		local inst = self:StartInstruction("div", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_14()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_14(this)

	local expr = self:Expression_15()

	while self:Accept("mul") do
		local inst = self:StartInstruction("mul", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_15()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_15(this)
	local expr = self:Expression_16()

	while self:Accept("exp") do
		local inst = self:StartInstruction("exp", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_16()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_16(this)
	local expr = self:Expression_17()

	while self:Accept("mod") do
		local inst = self:StartInstruction("mod", expr.token)

		inst.__operator = self.__token

		local expr2 = self:Expression_17()

		expr = self:EndInstruction(inst, {expr, expr2})
	end

	return expr
end

function PARSER.Expression_17(this)
	if self:Accept("add") then
		local tkn = self.__token

		self:ExcludeWhiteSpace("Identity operator (+) must not be succeeded by whitespace")

		local expr = self:Expression_18()

		self:QueueRemove(expr, tkn)

		return expr
	end

	return self:Expression_18()
end

function PARSER.Expression_18(this)
	if self:Accept("neg") then
		local inst = self:StartInstruction("neg", expr.token)

		inst.__operator = self.__token

		self:ExcludeWhiteSpace("Negation operator (-) must not be succeeded by whitespace")

		local expr = self:Expression_23()

		return self:EndInstruction(inst, {expr})
	end

	return self:Expression_19()
end

function PARSER.Expression_19(this)
	if self:Accept("neg") then
		local inst = self:StartInstruction("not", expr.token)

		inst.__operator = self.__token

		self:ExcludeWhiteSpace("Not operator (!) must not be succeeded by whitespace")

		local expr = self:Expression_23()

		return self:EndInstruction(inst, {expr})
	end

	return self:Expression_20()
end

function PARSER.Expression_20(this)
	if self:Accept("len") then
		local inst = self:StartInstruction("len", expr.token)

		inst.__operator = self.__token

		self:ExcludeWhiteSpace("Length operator (#) must not be succeeded by whitespace")

		local expr = self:Expression_23()

		return self:EndInstruction(inst, {expr})
	end

	return self:Expression_21()
end

function PARSER.Expression_21(this)
	if self:Accept("cst") then
		local inst = self:StartInstruction("cast", expr.token)
		
		inst.class = self.__token.data

		self:ExcludeWhiteSpace("Cast operator ( (%s) ) must not be succeeded by whitespace", inst.type)

		local expr = self:Expression_1()

		return self:EndInstruction(inst, {expr})
	end

	return self:Expression_22()
end

function PARSER.Expression_22(this)
	if self:Accept("lpa") then
		local expr = self:Expression_1()

		self:Require("rpa", "Right parenthesis ( )) missing, to close grouped equation.")

		return expr
	end

	return self:Expression_23()
end

function PARSER.Expression_23(this)
	if self:CheckToken("var") then
		local token = self.__token
		local library = self.__next.data
		local lib = EXPR_LIBRARIES[library]

		if lib then
			self:Next()

			local inst = self:StartInstruction("func", token)

			inst.library = self.__token

			if not self:Accept("prd") then
				self:StepBackward(1)
				return self:Expression_24()
			end

			inst.__operator = self.__token

			self:Require("var", "function expected after library name")
			
			inst.__func = self.__token

			inst.name = self.__token.data

			self:Require("lpa", "Left parenthesis (( ) expected to open function parameters.")

			inst.__lpa = self.__token
			
			local expressions = {}

			if not self:CheckToken("rpa") then
				expressions[1] = self:Expression_1()

				while(self:Accept("com")) do
					self:Exclude("rpa", "Expression or value expected after comma (,).")

					expressions[#expressions + 1] = self:Expression_1()
				end

			end  
			
			self:Require("rpa", "Right parenthesis ( )) expected to close function parameters.")

			return self:EndInstruction(inst, expressions)
		end
	end

	return self:Expression_24()
end

function PARSER.Expression_24(this)
	if self:Accept("var") then
		local inst = self:StartInstruction("var", self.__token)

		inst.variable = self.__token.data

		self:EndInstruction(inst, {})

		return self:Expression_Trailing(inst)
	end

	return self:Expression_25()
end

function PARSER.Expression_25(this)

	if self:Accept("new") then
		local inst = self:StartInstruction("new", self.__token)

		inst.__new = self.__token -- self:QueueRemove(inst, self.__token)

		self:Require("typ", "Type expected after new for constructor.")

		inst.class = self.__token.data

		inst.__const = self.__token -- self:QueueRemove(inst, self.__token)
		
		self:Require("lpa", "Left parenthesis (( ) expected to open constructor parameters.")
		
		inst.__lpa = self.__token

		local expressions = {}

		if not self:CheckToken("rpa") then
			expressions[1] = self:Expression_1()

			while(self:Accept("com")) do
				self:Exclude("rpa", "Expression or value expected after comma (,).")

				expressions[#expressions + 1] = self:Expression_1()
			end

		end

		self:Require("rpa", "Right parenthesis ( )) expected to close constructor parameters.")

		return self:EndInstruction(inst, expressions)
	end

	return self:Expression_26()
end

function PARSER.Expression_26(this)
	if self:AcceptWithData("typ", "f") then
		local inst = self:StartInstruction("lambda", self.__token)

		self:QueueInjectionBefore(inst, self.__token, "{op = ")

		self:QueueReplace(inst, self.__token, "function")

		local perams, signature = self:InputPeramaters(inst)

		inst.perams = perams
		
		inst.signature = signature

		inst.stmts = self:Block_1(true, " ")

		self:QueueInjectionAfter(inst, self.__token, ", signature = \"" .. signature .. "\"")
		
		inst.__end = self.__token
		-- We inject the } in the compiler.
		-- self:QueueInjectionAfter(inst, self.__token, "}")

		return self:EndInstruction(inst, {})
	end

	return self:Expression_27()
end

function PARSER:InputPeramaters(inst)
	self:Require("lpa", "Left parenthesis (() ) expected to open function parameters.")

	local signature = {}

	local perams = {}

	if not self:CheckToken("rpa") then
		while true do
			self:Require("typ", "Class expected for new peramater.")

			self:QueueRemove(inst, self.__token)

			local class = self.__token.data

			self:Require("var", "Peramater expected after class.")

			signature[#signature + 1] = class

			perams[#perams + 1] = {class, self.__token.data}

			if self:CheckToken("rpa") then
				break
			end

			if not self:HasTokens() then
				break
			end

			self:Require("com", "Right parenthesis ( )) expected to close function parameters.")
			-- May not look logical, but it is :D
		end
	end

	self:Require("rpa", "Right parenthesis ( )) expected to close function parameters.")

	return perams, table.concat(signature, ",")
end

function PARSER.Expression_27(this)
	expr = self:Expression_28()

	if expr then
		return expr
	end

	self:ExpressionErr()
end

function PARSER.Expression_28(this)
	if self:Accept("tre", "fls") then
		local inst = self:StartInstruction("bool", self.__token)
		inst.value = self.__token.data
		return self:EndInstruction(inst, {})
	elseif self:Accept("void") then
		local inst = self:StartInstruction("void", self.__token)
		
		self:QueueReplace(self.__token, "nil")
		
		return self:EndInstruction(inst, {})
	elseif self:Accept("num") then
		local inst = self:StartInstruction("num", self.__token)
		inst.value = self.__token.data
		return self:EndInstruction(inst, {})
	elseif self:Accept("str") then
		local inst = self:StartInstruction("str", self.__token)
		inst.value = self.__token.data
		return self:EndInstruction(inst, {})
	elseif self:Accept("ptr") then
		local inst = self:StartInstruction("ptrn", self.__token)
		inst.value = self.__token.data
		return self:EndInstruction(inst, {})
	elseif self:Accept("typ") then
		local inst = self:StartInstruction("cls", self.__token)
		inst.value = self.__token.data
		return self:EndInstruction(inst, {})
	end
end

function PARSER:Expression_Trailing(expr)

	while self:CheckToken("prd", "lsb", "lpa") do
		
		local excluded

		if self:StatmentContains(self.__token, "ass") then
			excluded = self:LastInStatment(self.__token, "lsb")
		end

		-- Methods
		if self:Accept("prd") then
			self.__prd = self.__token

			local inst = self:StartInstruction("meth", expr.token)

			inst.__operator = self.__token

			self:Require("var", "method name expected after method operator (.)")

			local varToken = self.__token

			--self:Require("lpa", "Left parenthesis (( ) expected to open method parameters.")

			if self:Accept("lpa") then
				inst.__lpa = self.__token

				local expressions = {}
	 
				expressions[1] = expr

				if not self:CheckToken("rpa") then
					expressions[2] = self:Expression_1()

					while(self:Accept("com")) do
						self:Exclude("rpa", "Expression or value expected after comma (,).")

						expressions[#expressions + 1] = self:Expression_1()
					end

				end  

				self:Require("rpa", "Right parenthesis ( )) expected to close method parameters.")

				inst.__rpa = self.__token
				inst.__method = varToken
				inst.method = varToken.data

				expr = self:EndInstruction(inst, {expr})
			else
				inst.type = "feild"

				inst.method = nil
				inst.__method = nil
				inst.__feild = varToken

				expr = self:EndInstruction(inst, {expr})
			end
		elseif self:Accept("lsb") then
			local lsb = self.__token

			-- Check for a set instruction and locate it,
			-- If we are at our set indexer then we break.

			if self:StatmentContains(self.__token, "ass") then
				local excluded = self:LastInStatment(self.__token, "lsb")

				if excluded and excluded.index == self.__token.index then
					self:StepBackward(1)
					break
				end
			end

			local inst = self:StartInstruction("get", expr.token)

			local expressions = {}
 
			expressions[1] = expr

			expressions[2] = self:Expression_1()

			if self:Accept("com") then

				self:Require("typ", "Class expected for index operator, after coma (,).")

				inst.class = self.__token
			end

			self:Require("rsb", "Right square bracket (]) expected to close index operator.")

			inst.__lsb = lsb
			inst.__rsb = self.__token

			expr = self:EndInstruction(inst, expressions)
		elseif self:Accept("lpa") then
			local inst = self:StartInstruction("call", expr.token)

			self:QueueRemove(inst, self.__token)
			local expressions = {}
 
			expressions[1] = expr

			if not self:CheckToken("rpa") then
				expressions[2] = self:Expression_1()

				while self:Accept("com") do
					self:Exclude("rpa", "Expression or value expected after comma (,).")

					expressions[#expressions + 1] = self:Expression_1()
				end

			end  

			self:Require("rpa", "Right parenthesis ( )) expected to close call parameters.")

			expr = self:EndInstruction(inst, expressions)
		end
	end
	
	return expr
end

function PARSER.GetCondition(this)
	self:Require("lpa", "Left parenthesis ( () required, to open condition.")
	
	local inst = self:StartInstruction("cond", self.__token)
	
	local expr = self:Expression_1()

	self:Require("rpa", "Right parenthesis ( )) missing, to close condition.")
	
	return self:EndInstruction(inst, {expr})
end

function PARSER.ExpressionErr(this)
	if not self.__token then
		self:Throw(self.__tokens[#self.__tokens], "Further input required at end of code, incomplete expression")
	end

	self:ExcludeWhiteSpace("Further input required at end of code, incomplete expression")
	self:Exclude("void", "void must not appear inside an equation")
	self:Exclude("add", "Arithmetic operator (+) must be preceded by equation or value")
	self:Exclude("sub", "Arithmetic operator (-) must be preceded by equation or value")
	self:Exclude("mul", "Arithmetic operator (*) must be preceded by equation or value")
	self:Exclude("div", "Arithmetic operator (/) must be preceded by equation or value")
	self:Exclude("mod", "Arithmetic operator (%) must be preceded by equation or value")
	self:Exclude("exp", "Arithmetic operator (^) must be preceded by equation or value")
	self:Exclude("ass", "Assignment operator (=) must be preceded by variable")
	self:Exclude("aadd", "Assignment operator (+=) must be preceded by variable")
	self:Exclude("asub", "Assignment operator (-=) must be preceded by variable")
	self:Exclude("amul", "Assignment operator (*=) must be preceded by variable")
	self:Exclude("adiv", "Assignment operator (/=) must be preceded by variable")
	self:Exclude("and", "Logical operator (&&) must be preceded by equation or value")
	self:Exclude("or", "Logical operator (||) must be preceded by equation or value")
	self:Exclude("eq", "Comparison operator (==) must be preceded by equation or value")
	self:Exclude("neq", "Comparison operator (!=) must be preceded by equation or value")
	self:Exclude("gth", "Comparison operator (>=) must be preceded by equation or value")
	self:Exclude("lth", "Comparison operator (<=) must be preceded by equation or value")
	self:Exclude("geq", "Comparison operator (>) must be preceded by equation or value")
	self:Exclude("leq", "Comparison operator (<) must be preceded by equation or value")
	-- self:Exclude("inc", "Increment operator (++) must be preceded by variable")
	-- self:Exclude("dec", "Decrement operator (--) must be preceded by variable")
	self:Exclude("rpa", "Right parenthesis ( )) without matching left parenthesis")
	self:Exclude("lcb", "Left curly bracket ({) must be part of an table/if/while/for-statement block")
	self:Exclude("rcb", "Right curly bracket (}) without matching left curly bracket")
	self:Exclude("lsb", "Left square bracket ([) must be preceded by variable")
	self:Exclude("rsb", "Right square bracket (]) without matching left square bracket")
	self:Exclude("com", "Comma (,) not expected here, missing an argument?")
	self:Exclude("prd", "Method operator (.) must not be preceded by white space")
	self:Exclude("col", "Ternary operator (:) must be part of conditional expression (A ? B : C).")
	self:Exclude("if", "If keyword (if) must not appear inside an equation")
	self:Exclude("eif", "Else-if keyword (elseif) must be part of an if-statement")
	self:Exclude("els", "Else keyword (else) must be part of an if-statement")
	--self:Exclude("try", "Try keyword (try) must be part of a try-statement")
	--self:Exclude("cth", "Catch keyword (catch) must be part of an try-statement")
	--self:Exclude("fnl", "Final keyword (final) must be part of an try-statement")
	self:Exclude("dir", "directive operator (@) must not appear inside an equation")

	self:Throw(self.__token, "Unexpected symbol found (%s)", self.__token.type)
end

--[[
]]

function PARSER.ClassStatment_0(this)
	if self:Accept("cls") then
		local inst = self:StartInstruction("class", self.__token)

		self:Require("var", "Class anme expected after class")
		inst.__classname = self.__token
	
		self:Require("lcb", "Left curly bracket (}) expected, to open class")
		inst.__lcb = self.__token
		
		local stmts = {}

		if not self:CheckToken("rcb") then
			self.__scope = self.__scope + 1

			stmts = self:Statments(true, self.ClassStatment_1)

			self.__scope = self.__scope - 1
		end

		self:Require("rcb", "Right curly bracket (}) missing, to close class")
		inst.__rcb = self.__token

		return self:EndInstruction(inst, stmts)
	end
end

function PARSER.ClassStatment_1(this)
	if self:Accept("typ") then
		local inst = self:StartInstruction("def_feild", self.__token)
		
		local type = self.__token.data

		if type == "f" and self:CheckToken("typ") then
			self:StepBackward(1)
			return self:Statment_8()
		end

		self:QueueRemove(inst, self.__token)

		inst.class = type
		
		local variables = {}

		self:Require("var", "Variable('s) expected after class for variable.")
		variables[1] = self.__token

		while self:Accept("com") do
			self:Require("var", "Variable expected after comma (,).")
			variables[#variables + 1] = self.__token
		end
		
		local expressions = {}

		if self:Accept("ass") then
			self:ExcludeWhiteSpace( "Assignment operator (=), must not be preceded by whitespace." )
			
			expressions[1] = self:Expression_1()

			while self:Accept("com") do
				self:ExcludeWhiteSpace( "comma (,) must not be preceded by whitespace." )
				expressions[#expressions + 1] = self:Expression_1()
			end
		end

		inst.variables = variables

		return self:EndInstruction(inst, expressions)
	end
end

--[[
]]

EXPR_PARSER = PARSER