--[[
	   ____      _  _      ___    ___       ____      ___      ___     __     ____      _  _          _        ___     _  _       ____   
	  F ___J    FJ  LJ    F _ ", F _ ",    F ___J    F __".   F __".   FJ    F __ ]    F L L]        /.\      F __".  FJ  L]     F___ J  
	 J |___:    J \/ F   J `-' |J `-'(|   J |___:   J (___|  J (___|  J  L  J |--| L  J   \| L      //_\\    J |--\ LJ |  | L    `-__| L 
	 | _____|   /    \   |  __/F|  _  L   | _____|  J\___ \  J\___ \  |  |  | |  | |  | |\   |     / ___ \   | |  J |J J  F L     |__  ( 
	 F L____:  /  /\  \  F |__/ F |_\  L  F L____: .--___) \.--___) \ F  J  F L__J J  F L\\  J    / L___J \  F L__J |J\ \/ /F  .-____] J 
	J________LJ__//\\__LJ__|   J__| \\__LJ________LJ\______JJ\______JJ____LJ\______/FJ__L \\__L  J__L   J__LJ______/F \\__//   J\______/F
	|________||__/  \__||__L   |__|  J__||________| J______F J______F|____| J______F |__L  J__|  |__L   J__||______F   \__/     J______F 

	::Compiler::
]]

local function name(id)
	return EXPR_LIB.GetClass(id).name
end

local function names(ids)
	if isstring(ids) then
		ids = string.Explode(",", ids)
	end

	local names = {}

	for i, id in pairs(ids) do
		names[i] = EXPR_LIB.GetClass(id).name
	end

	return table.concat(names,", ")
end

--[[
]]

local COMPILER = {}
COMPILER.__index = COMPILER

function COMPILER.New()
	return setmetatable({}, COMPILER)
end

function COMPILER:Initalize(instance)
	self.__tokens = instance.tokens
	self.__tasks = instance.tasks
	self.__root = instance.instruction
	self.__script = instance.script
	self.__directives = instance.directives

	self.__scope = {}
	self.__scopeID = 0
	self.__scopeData = {}
	self.__scopeData[0] = self.__scope

	self.__scope.memory = {}
	self.__scope.classes = {}
	self.__scope.server = true
	self.__scope.client = true

	self.__defined = {}

	self.__constructors = {}
	self.__operators = {}
	self.__functions = {}
	self.__methods = {}
	self.__enviroment = {}
end

function COMPILER:Run()
	--TODO: PcallX for stack traces on internal errors?
	local status, result = pcall(self._Run, self)

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

function COMPILER:_Run()
	self:SetOption("state", EXPR_SHARED)

	self:Compile(self.__root)

	local script, traceTbl = self:BuildScript()

	local result = {}
	result.script = self.__script
	result.compiled = script
	result.constructors = self.__constructors
	result.operators = self.__operators
	result.functions = self.__functions
	result.methods = self.__methods
	result.enviroment = self.__enviroment
	result.directives = self.__directives
	result.traceTbl = traceTbl
	
	return result
end

function COMPILER:BuildScript()
	-- This will probably become a separate stage (post compiler?).

	local buffer = {}
	local alltasks = self.__tasks

	local char = 0
	local line = 3
	local traceTable = {}

	for k, v in pairs(self.__tokens) do
		local data = tostring(v.data)

		if v.newLine then
			char = 1
			line = line + 1
			buffer[#buffer + 1] = "\n"
		end

		local tasks = alltasks[v.pos]

		if tasks then
			
			local prefixs = tasks.prefix

			if prefixs then
				--for _ = #prefixs, 1, -1 do
				--	local prefix = prefixs[_]
				for _, prefix in pairs(prefixs) do
					if prefix.newLine then
						char = 1
						line = line + 1
						buffer[#buffer + 1] = "\n"
					end

					buffer[#buffer + 1] = prefix.str
					char = char + #prefix.str + 1
				end
			end

			if not tasks.remove then
				if tasks.replace then
					buffer[#buffer + 1] = tasks.replace.str
					char = char + #tasks.replace
				else
					buffer[#buffer + 1] = data
					char = char + #data + 1
				end

				traceTable[#traceTable + 1] = {e3_line = v.line, e3_char = v.char, native_line = line, native_char = char}
			end

			local postfixs = tasks.postfix

			if postfixs then
				--for _ = #postfixs, 1, -1 do
				--	local postfix = postfixs[_]
				for _, postfix in pairs(postfixs) do
					if postfix.newLine then
						char = 1
						line = line + 1
						buffer[#buffer + 1] = "\n"
					end
					char = char + #postfix.str + 1
					buffer[#buffer + 1] = postfix.str
				end
			end

			if tasks.instruction then
				
			end
		else
			traceTable[#traceTable + 1] = {e3_line = v.line, e3_char = v.char, native_line = line, native_char = char}
			buffer[#buffer + 1] = data
			char = char + #data + 1
		end
	end

	return table.concat(buffer, " "), traceTable
end

function COMPILER:Throw(token, msg, fst, ...)
	local err = {}

	if fst then
		msg = string.format(msg, fst, ...)
	end

	err.state = "compiler"
	err.char = token.char
	err.line = token.line
	err.msg = msg

	error(err,0)
end

--[[
]]

function COMPILER:OffsetToken(token, offset)
	local pos = token.index + offset

	local token = self.__tokens[pos]

	return token
end

--[[
]]

function COMPILER:Import(path)
	local g = _G
	local e = self.__enviroment
	local a = string.Explode(".", path)
	
	if #a > 1 then
		for i = 1, #a - 1 do
			local k = a[i]
			local v = g[k]
			
			if istable(v) then
				if not istable(e[k]) then
					e[k] = {}
				end
				
				g = v
				e = e[k]
			end
		end
	end
	
	local k = a[#a]
	local v = g[k]
	
	if(isfunction(v)) then
		e[k] = v
	end
end

--[[
]]

function COMPILER:PushScope()
	self.__scope = {}
	self.__scope.memory = {}
	self.__scope.classes = {}
	self.__scopeID = self.__scopeID + 1
	self.__scopeData[self.__scopeID] = self.__scope
end

function COMPILER:PopScope()
	self.__scopeData[self.__scopeID] = nil
	self.__scopeID = self.__scopeID - 1
	self.__scope = self.__scopeData[self.__scopeID]
end

function COMPILER:SetOption(option, value, deep)
	if not deep then
		self.__scope[option] = value
	else
		for i = self.__scopeID, 0, -1 do
			local v = self.__scopeData[i][option]

			if v then
				self.__scopeData[i][option] = value
				break
			end
		end
	end
end

function COMPILER:GetOption(option, nonDeep)
	if self.__scope[option] then
		return self.__scope[option]
	end

	if not nonDeep then
		for i = self.__scopeID, 0, -1 do
			local v = self.__scopeData[i][option]

			if v then
				return v
			end
		end
	end
end

function COMPILER:SetVariable(name, class, scope)
	if not scope then
		scope = self.__scopeID
	end

	local var = {}
	var.name = name
	var.class = class
	var.scope = scope
	self.__scopeData[scope].memory[name] = var

	return class, scope, var
end

function COMPILER:GetVariable(name, scope, nonDeep)
	if not scope then
		scope = self.__scopeID
	end

	local v = self.__scopeData[scope].memory[name]

	if v then
		return v.class, v.scope, v
	end

	if not nonDeep then
		for i = scope, 0, -1 do
			local v = self.__scopeData[i].memory[name]

			if v then
				return v.class, v.scope, v
			end
		end
	end
end

local bannedVars = {
	["GLOBAL"] = true,
	["SERVER"] = true,
	["CLIENT"] = true,
	["CONTEXT"] = true,
	["_OPS"] = true,
	["_CONST"] = true,
	["_METH"] = true,
	["_FUN"] = true,
	["invoke"] = true,
}

function COMPILER:AssignVariable(token, declaired, varName, class, scope)
	if bannedVars[varName] then
		self:Throw(token, "Unable to declare variable %s, name is reserved internally.", varName)
	end

	if not scope then
		scope = self.__scopeID
	end

	local c, s, var = self:GetVariable(varName, scope, declaired)

	if declaired then
		if c and c == class then
			self:Throw(token, "Unable to declare variable %s, Variable already exists.", varName)
		elseif c then
			self:Throw(token, "Unable to initalize variable %s, %s expected got %s.", varName, name(c), name(class))
		else
			return self:SetVariable(varName, class, scope)
		end
	else
		if not c then
			self:Throw(token, "Unable to assign variable %s, Variable doesn't exist.", varName)
		elseif c ~= class then
			self:Throw(token, "Unable to assign variable %s, %s expected got %s.", varName, name(c), name(class))
		end
	end

	return c, s, var
end

--[[
]]

function COMPILER:GetOperator(operation, fst, ...)
	if not fst then
		return EXPR_OPERATORS[operation .. "()"]
	end

	local signature = string.format("%s(%s)", operation, table.concat({fst, ...},","))
	
	local Op = EXPR_OPERATORS[signature]

	if Op then
		return Op
	end

	-- TODO: Inheritance.
end

--[[
]]

function COMPILER:QueueReplace(inst, token, str)
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

function COMPILER:QueueRemove(inst, token)
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

local injectNewLine = false

function COMPILER:QueueInjectionBefore(inst, token, str, ...)
	
	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	if not tasks.prefix then
		tasks.prefix = {}
	end

	local r = {}
	local t = {str, ...}

	for i = 1, #t do
		local op = {}
	
		op.token = token
		op.str = t[i]
		op.inst = inst

		if i == 1 then
			op.newLine = injectNewLine
		end

		tasks.prefix[#tasks.prefix + 1] = op
	end

	return r
end

function COMPILER:QueueInjectionAfter(inst, token, str, ...)
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
		
		if i == 1 then
			op.newLine = injectNewLine
		end

		r[#r + 1] = op
		tasks.postfix[#tasks.postfix + 1] = op
	end

	return r
end

--[[
]]

function COMPILER:QueueInstruction(inst, inst, token, inst, type) -- What the hell are these arguments
	local op = {}
	op.token = token
	op.inst = inst
	op.type = type

	local tasks = self.__tasks[token.pos]

	if not tasks then
		tasks = {}
		self.__tasks[token.pos] = tasks
	end

	if not tasks.instruction then
		tasks.instruction = op
	end

	return op
end

function COMPILER:Compile(inst)
	if not inst then
		debug.Trace()
		error("Compiler was asked to compile a nil instruction.")
	end

	if not istable(inst.token) then
		debug.Trace()
		print("token is ", type(inst.token), inst.token)
	end

	if not inst.compiled then
		local instruction = string.upper(inst.type)
		local fun = self["Compile_" .. instruction]

		-- print("Compiler->" .. instruction .. "->#" .. #inst.instructions)

		if not fun then
			self:Throw(inst.token, "Failed to compile unknown instruction %s", instruction)
		end

		--self:QueueInstruction(inst, inst.token, inst.type)

		local type, count = fun(self, inst, inst.token, inst.instructions)

		if type then
			inst.result = type
			inst.rCount = count or 1
		end

		inst.compiled = true
	end

	return inst.result, inst.rCount
end

--[[
]]


--[[
]]

function COMPILER:Compile_SEQ(inst, token, stmts)
	for i = 1, #stmts do
		self:Compile(stmts[i])
	end

	return "", 0
end

function COMPILER:Compile_IF(inst, token)
	local r, c = self:Compile(inst.condition)
	
	if class ~= "b" then
		local isBool = self:Expression_IS(inst.condition)

		if not isBool then
			local t = self:CastExpression("b", inst.condition)

			if not t then
				self:Throw(token, "Type of %s can not be used as a condition.", name(r))
			end
		end
	end

	self:PushScope()

	self:Compile(inst.block)
	
	self:PopScope()

	if inst._else then
		self:Compile(inst._else)
	end

	return "", 0
end

function COMPILER:Compile_ELSEIF(inst, token)
	local class, count = self:Compile(inst.condition)
	
	if class ~= "b" then
		local isBool = self:Expression_IS(inst.condition)

		if not isBool then
			local t = self:CastExpression("b", inst.condition)

			if not t then
				self:Throw(token, "Type of %s can not be used as a condition.", name(r))
			end
		end
	end

	self:PushScope()

	self:Compile(inst.block)
	
	self:PopScope()

	if inst._else then
		self:Compile(inst._else)
	end

	return "", 0
end

function COMPILER:Compile_ELSE(inst, token)
	self:PushScope()

	self:Compile(inst.block)
	
	self:PopScope()

	return "", 0
end

--[[
]]

function COMPILER:CheckState(state, token, msg, frst, ...)
	local s = self:GetOption("state")
	
	if state == EXPR_SHARED or s == state then
		return true
	end

	if token and msg then
		if frst then
			msg = string.format(msg, frst, ...)
		end

		if state == EXPR_SERVER then
			self:Throw(token, "%s is server-side only.", msg)
		elseif state == EXPR_SERVER then
			self:Throw(token, "%s is client-side only.", msg)
		end 
	end

	return false
end

function COMPILER:Compile_SERVER(inst, token)
	if not self:GetOption("server") then
		self:Throw(token, "Server block must not appear inside a Client block.")
	end

	self:PushScope()
	self:SetOption("state", EXPR_SERVER)
	self:Compile(inst.block)
	
	self:PopScope()

	return "", 0
end

function COMPILER:Compile_CLIENT(inst, token)
	if not self:GetOption("client") then
		self:Throw(token, "Client block must not appear inside a Server block.")
	end

	self:PushScope()
	self:SetOption("state", EXPR_CLIENT)
	self:Compile(inst.block)
	
	self:PopScope()

	return "", 0
end

--[[
]]

function COMPILER:Compile_GLOBAL(inst, token, expressions)
	local tArgs = #expressions

	local results = {}

	for i = 1, tArgs do
		local arg = expressions[i]
		local r, c = self:Compile(arg)

		if not inst.variables[i] then
			self:Throw(arg.token, "Unable to assign here, value #%i has no matching variable.", i)
		elseif i < tArgs then
			results[#results + 1] = {r, arg, true}
		else
			for j = 1, c do
				results[#results + 1] = {r, arg, j == 1}
			end
		end
	end

	for i = 1, #inst.variables do
		local result = results[i]
		local token = inst.variables[i]
		local var = token.data

		if not result then
			self:Throw(token, "Unable to assign variable %s, no matching value.", var)
		end

		local class, scope, info = self:AssignVariable(token, true, var, inst.class, 0)

		if info then
			info.prefix = "GLOBAL"
			self:QueueReplace(inst, token, info.prefix .. "." .. var)
		end

		self.__defined[var] = true

		if result[1] ~= inst.class then
			local casted = false
			local arg = result[2]

			if result[3] then
				-- TODO: CAST
			end

			if not casted then
				self:AssignVariable(arg.token, true, var, result[1], 0)
			end
		end
	end

	self.__defined = {}

	return "", 0
end

function COMPILER:Compile_LOCAL(inst, token, expressions)
	local tArgs = #expressions

	local results = {}

	for i = 1, tArgs do
		local arg = expressions[i]
		local r, c = self:Compile(arg)

		if not inst.variables[i] then
			self:Throw(arg.token, "Unable to assign here, value #%i has no matching variable.", i)
		elseif i < tArgs then
			results[#results + 1] = {r, arg, true}
		else
			for j = 1, c do
				results[#results + 1] = {r, arg, j == 1}
			end
		end
	end

	for i = 1, #inst.variables do
		local result = results[i]
		local token = inst.variables[i]
		local var = token.data

		if not result then
			self:Throw(token, "Unable to assign variable %s, no matching value.", var)
		end

		local class, scope, info = self:AssignVariable(token, true, var, inst.class)

		self.__defined[var] = true

		if result[1] ~= inst.class then
			local casted = false
			local arg = result[2]

			if result[3] then
				-- TODO: CAST
			end

			if not casted then
				self:AssignVariable(arg.token, true, var, result[1])
			end
		end
	end

	self.__defined = {}

	return "", 0
end

function COMPILER:Compile_ASS(inst, token, expressions)
	local tArgs = #expressions

	local results = {}

	for i = 1, tArgs do
		local arg = expressions[i]
		local r, c = self:Compile(arg)

		if not inst.variables[i] then
			self:Throw(arg.token, "Unable to assign here, value #%i has no matching variable.", i)
		elseif i < tArgs then
			results[#results + 1] = {r, arg, true}
		else
			for j = 1, c do
				results[#results + 1] = {r, arg, j == 1}
			end
		end
	end

	for i = 1, #inst.variables do
		local result = results[i]

		local token = inst.variables[i]
		local var = token.data

		if not result then
			self:Throw(token, "Unable to assign variable %s, no matching value.", var)
		end

		self.__defined[var] = true

		local type = result[1]
		local class, scope, info = self:GetVariable(var)

		if type ~= class then
			local arg = result[2]

			if result[3] then
				-- TODO: CAST
				-- Once done rember: type = class
			end
		end

		local class, scope, info = self:AssignVariable(token, false, var, class)

		if info and info.prefix then
			var = info.prefix .. "." .. var

			self:QueueReplace(inst, token, var)
		end

		if inst.class == "f" then
			injectNewLine = true
			
			if info.signature then
				local msg = string.format("Failed to assign function to delegate %s(%s), permater missmatch.", var, info.signature)
				self:QueueInjectionAfter(inst, inst.final, string.format("if %s and %s.signature ~= %q then CONTEXT:Throw(%q) %s = nil end", var, var, info.signature, msg, var))
			end
			
			if info.resultClass then
				local msg = string.format("Failed to assign function to delegate %s(%s), result type missmatch.", var, name(info.resultClass))
				self:QueueInjectionAfter(inst, inst.final, string.format("if %s and %s.result ~= %q then CONTEXT:Throw(%q) %s = nil end", var, var, name(info.resultClass), msg, var))
			end
			
			if info.resultCount then
				local msg = string.format("Failed to assign function to delegate %s(%s), result count missmatch.", var, info.resultCount)
				self:QueueInjectionAfter(inst, inst.final, string.format("if %s and %s.count ~= %i then CONTEXT:Throw(%q) %s = nil end", var, var, info.resultCount, msg, var))
			end

			injectNewLine = false
		end
	end

	self.__defined = {}

	return "", 0
end

--[[
]]

function COMPILER:Compile_AADD(inst, token, expressions)
	self:QueueReplace(inst, inst.__operator, "=")

	for k = 1, #inst.variables do
		local token = inst.variables[k]
		local expr = expressions[k]
		local r, c = self:Compile(expr)

		local class, scope, info = self:GetVariable(token.data, nil, false)

		if info and info.prefix then
			self:QueueReplace(inst, token, info.prefix .. "." .. token.data)
		end

		local char = "+"

		local op = self:GetOperator("add", class, r)

		if not op and r ~= class then
			if self:CastExpression(class, expr) then
				op = self:GetOperator("add", class, class)
			end
		end

		if not op then
			self:Throw(expr.token, "Assignment operator (+=) does not support '%s += %s'", name(class), name(r))
		end

		self:CheckState(op.state, token, "Assignment operator (+=)")

		if not op.operation then
			if r == "s" or class == "s" then
				char = ".."
			end

			if info and info.prefix then
				self:QueueInjectionBefore(inst, expr.token, info.prefix .. "." .. token.data, char)
			else
				self:QueueInjectionBefore(inst, expr.token, token.data, char)
			end
		else
			-- Implement Operator
			self.__operators[op.signature] = op.operator

			self:QueueInjectionBefore(inst, expr.token, "_OPS", "[", "\"" .. op.signature .. "\"", "]", "(")

			if op.context then
			    self:QueueInjectionBefore(inst, expr.token "CONTEXT", ",")
			end

			self:QueueInjectionAfter(inst, expr.final, ")" )
		end	

		self:AssignVariable(token, false, token.data, op.result)
	end
end

function COMPILER:Compile_ASUB(inst, token, expressions)
	self:QueueReplace(inst, inst.__operator, "=")

	for k = 1, #inst.variables do
		local token = inst.variables[k]
		local expr = expressions[k]
		local r, c = self:Compile(expr)

		local class, scope, info = self:GetVariable(token.data, nil, false)

		if info and info.prefix then
			self:QueueInjectionBefore(inst, token, info.prefix .. ".")
		end

		local op = self:GetOperator("sub", class, r)

		if not op and r ~= class then
			if self:CastExpression(class, expr) then
				op = self:GetOperator("sub", class, class)
			end
		end

		if not op then
			self:Throw(expr.token, "Assignment operator (-=) does not support '%s -= %s'", name(class), name(r))
		end

		self:CheckState(op.state, token, "Assignment operator (-=)")

		if not op.operation then

			if info and info.prefix then
				self:QueueInjectionBefore(inst, expr.token, info.prefix .. "." .. token.data, "-")
			else
				self:QueueInjectionBefore(inst, expr.token, token.data, char)
			end
		else
			-- Implement Operator
			self.__operators[op.signature] = op.operator

			self:QueueInjectionBefore(inst, expr.token, "_OPS", "[", "\"" .. op.signature .. "\"", "]", "(")

			if op.context then
			    self:QueueInjectionBefore(inst, expr.token "CONTEXT", ",")
			end

			self:QueueInjectionAfter(inst, expr.final, ")" )
		end	

		self:AssignVariable(token, false, token.data, op.result)
	end
end



function COMPILER:Compile_ADIV(inst, token, expressions)
	self:QueueReplace(inst, inst.__operator, "=")

	for k = 1, #inst.variables do
		local token = inst.variables[k]
		local expr = expressions[k]
		local r, c = self:Compile(expr)

		local class, scope, info = self:GetVariable(token.data, nil, false)

		if info and info.prefix then
			self:QueueInjectionBefore(inst, token, info.prefix .. ".")
		end

		local op = self:GetOperator("div", class, r)

		if not op and r ~= class then
			if self:CastExpression(class, expr) then
				op = self:GetOperator("div", class, class)
			end
		end

		if not op then
			self:Throw(expr.token, "Assignment operator (/=) does not support '%s /= %s'", name(class), name(r))
		end

		self:CheckState(op.state, token, "Assignment operator (/=)")

		if not op.operation then

			if info and info.prefix then
				self:QueueInjectionBefore(inst, expr.token, info.prefix .. "." .. token.data, "/")
			else
				self:QueueInjectionBefore(inst, expr.token, token.data, char)
			end
		else
			-- Implement Operator
			self.__operators[op.signature] = op.operator

			self:QueueInjectionBefore(inst, expr.token, "_OPS", "[", "\"" .. op.signature .. "\"", "]", "(")

			if op.context then
			    self:QueueInjectionBefore(inst, expr.token "CONTEXT", ",")
			end

			self:QueueInjectionAfter(inst, expr.final, ")" )
		end	

		self:AssignVariable(token, false, token.data, op.result)
	end
end

function COMPILER:Compile_AMUL(inst, token, expressions)
	self:QueueReplace(inst, inst.__operator, "=")

	for k = 1, #inst.variables do
		local token = inst.variables[k]
		local expr = expressions[k]
		local r, c = self:Compile(expr)

		local class, scope, info = self:GetVariable(token.data, nil, false)

		if info and info.prefix then
			self:QueueInjectionBefore(inst, token, info.prefix .. ".")
		end

		local op = self:GetOperator("mul", class, r)

		if not op and r ~= class then
			if self:CastExpression(class, expr) then
				op = self:GetOperator("mul", class, class)
			end
		end

		if not op then
			self:Throw(expr.token, "Assignment operator (*=) does not support '%s *= %s'", name(class), name(r))
		end

		self:CheckState(op.state, token, "Assignment operator (*=)")

		if not op.operation then

			if info and info.prefix then
				self:QueueInjectionBefore(inst, expr.token, info.prefix .. "." .. token.data, "*")
			else
				self:QueueInjectionBefore(inst, expr.token, token.data, char)
			end
		else
			-- Implement Operator
			self.__operators[op.signature] = op.operator

			self:QueueInjectionBefore(inst, expr.token, "_OPS", "[", "\"" .. op.signature .. "\"", "]", "(")

			if op.context then
			    self:QueueInjectionBefore(inst, expr.token "CONTEXT", ",")
			end

			self:QueueInjectionAfter(inst, expr.final, ")" )
		end	

		self:AssignVariable(token, false, token.data, op.result)
	end
end

--[[
]]

function COMPILER:Compile_TEN(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local expr3 = expressions[3]
	local r3, c3 = self:Compile(expr1)

	local op = self:GetOperator("ten", r1, r2, r3)

	if not op then
		self:Throw(expr.token, "Tenary operator (A ? b : C) does not support '%s ? %s : %s'", name(r1), name(r2), name(r3))
	elseif not op.operation then
		self:QueueReplace(inst, inst.__and, "and")
		self:QueueReplace(inst, inst.__or, "or")
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__and, ",")
		self:QueueReplace(inst, inst.__or, ",")
		
		self:QueueInjectionAfter(inst, expr3.final, ")" )

		self.__operators[op.signature] = op.operator
	end	

	self:CheckState(op.state, token, "Tenary operator (A ? b : C)")

	return op.result, op.rCount
end


function COMPILER:Compile_OR(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("or", r1, r2)

	if not op then
		local is1 = self:Expression_IS(expr1)
		local is2 = self:Expression_IS(expr2)

		if is1 and is2 then
			op = self:GetOperator("and", "b", "b")
		end

		if not op then
			self:Throw(token, "Logical or operator (||) does not support '%s || %s'", name(r1), name(r2))
		end
	elseif not op.operation then
		self:QueueReplace(inst, inst.__operator, "or")
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Logical or operator (||) '%s || %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_AND(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("and", r1, r2)

	if not op then
		local is1 = self:Expression_IS(expr1)
		local is2 = self:Expression_IS(expr2)

		if is1 and is2 then
			op = self:GetOperator("and", "b", "b")
		end

		if not op then
			self:Throw(token, "Logical and operator (&&) does not support '%s && %s'", name(r1), name(r2))
		end
	elseif not op.operation then
		self:QueueReplace(inst, inst.__operator, "and")
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Logical and operator (&&) '%s && %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_BXOR(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("bxor", r1, r2)

	if not op then
		self:Throw(token, "Binary xor operator (^^) does not support '%s ^^ %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueInjectionBefore(inst, expr1.token, "bit.bxor(")

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Binary xor operator (^^) '%s ^^ %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_BOR(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("bor", r1, r2)

	if not op then
		self:Throw(token, "Binary or operator (|) does not support '%s | %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueInjectionBefore(inst, expr1.token, "bit.bor(")

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Binary xor operator (|) '%s | %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_BAND(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("band", r1, r2)

	if not op then
		self:Throw(token, "Binary or operator (&) does not support '%s & %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueInjectionBefore(inst, expr1.token, "bit.band(")

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Binary xor operator (&) '%s & %s'", name(r1), name(r2))

	return op.result, op.rCount
end

--[[function COMPILER.Compile_EQ_MUL(inst, token, expressions)
end]]

function COMPILER:Compile_EQ(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("eq", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (==) does not support '%s == %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Leave the code alone.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (==) '%s == %s'", name(r1), name(r2))

	return op.result, op.rCount
end

--[[function COMPILER.Compile_NEQ_MUL(inst, token, expressions)
end]]

function COMPILER:Compile_NEQ(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("neq", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (!=) does not support '%s != %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueReplace(inst, inst.__operator, "~=")
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (!=) '%s != %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_LTH(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("lth", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (<) does not support '%s < %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Leave the code alone.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (<) '%s < %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_LEQ(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("leg", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (<=) does not support '%s <= %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Leave the code alone.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (<=) '%s <= %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_GTH(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("gth", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (>) does not support '%s > %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Leave the code alone.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (>) '%s > %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_GEQ(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("geq", r1, r2)

	if not op then
		self:Throw(token, "Comparison operator (>=) does not support '%s >= %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Leave the code alone.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Comparison operator (>=) '%s >= %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_BSHL(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("bshl", r1, r2)

	if not op then
		self:Throw(token, "Binary shift operator (<<) does not support '%s << %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueInjectionBefore(inst, expr1.token, "bit.lshift(")

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end
		
		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Binary shift operator (<<) '%s << %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_BSHR(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("bshr", r1, r2)

	if not op then
		self:Throw(token, "Binary shift operator (>>) does not support '%s >> %s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueInjectionBefore(inst, expr1.token, "bit.rshift(")

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Binary shift operator (>>) '%s >> %s'", name(r1), name(r2))

	return op.result, op.rCount
end

--[[
]]

function COMPILER:Compile_ADD(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("add", r1, r2)

	if not op then
		self:Throw(token, "Addition operator (+) does not support '%s + %s'", name(r1), name(r2))
	elseif not op.operation then
		if r1 == "s" or r2 == "s" then
			self:QueueReplace(inst, inst.__operator, "..") -- Replace + with .. for string addition
		end
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Addition operator (+) '%s + %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_SUB(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("sub", r1, r2)

	if not op then
		self:Throw(token, "Subtraction operator (-) does not support '%s - %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Subtraction operator (-) '%s - %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_DIV(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("div", r1, r2)

	if not op then
		self:Throw(expr.token, "Division operator (/) does not support '%s / %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Division operator (/) '%s / %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_MUL(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("mul", r1, r2)

	if not op then
		self:Throw(token, "Multiplication operator (*) does not support '%s * %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Multiplication operator (*) '%s * %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_EXP(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("exp", r1, r2)

	if not op then
		self:Throw(token, "Exponent operator (^) does not support '%s ^ %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Exponent operator (^) '%s ^ %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_MOD(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local expr2 = expressions[2]
	local r2, c2 = self:Compile(expr2)

	local op = self:GetOperator("mod", r1, r2)

	if not op then
		self:Throw(token, "Modulus operator (%) does not support '%s % %s'", name(r1), name(r2))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end

		self:QueueReplace(inst, inst.__operator, ",")
		
		self:QueueInjectionAfter(inst, expr2.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Modulus operator (%) '%s % %s'", name(r1), name(r2))

	return op.result, op.rCount
end

function COMPILER:Compile_NEG(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local op = self:GetOperator("neg", r1)

	if not op then
		self:Throw(token, "Negation operator (-A) does not support '-%s'", name(r1))
	elseif not op.operation then
		-- Do not change the code.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end
		
		self:QueueInjectionAfter(inst, expr1.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Negation operator (-A) '-%s'", name(r1))

	return op.result, op.rCount
end

function COMPILER:Compile_NOT(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local op = self:GetOperator("not", r1)

	if not op then
		self:Throw(token, "Not operator (!A) does not support '!%s'", name(r1), name(r2))
	elseif not op.operation then
		self:QueueReplace(inst, inst.__operator, "not")
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end
		
		self:QueueInjectionAfter(inst, expr1.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Not operator (!A) '!%s'", name(r1))

	return op.result, op.rCount
end

function COMPILER:Compile_LEN(inst, token, expressions)
	local expr1 = expressions[1]
	local r1, c1 = self:Compile(expr1)

	local op = self:GetOperator("len", r1)

	if not op then
		self:Throw(token, "Length operator (#A) does not support '#%s'", name(r1), name(r2))
	elseif not op.operation then
		-- Once again we change nothing.
	else
		self:QueueInjectionBefore(inst, expr1.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr1.token, "CONTEXT", ",")
		end
		
		self:QueueInjectionAfter(inst, expr1.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	self:CheckState(op.state, token, "Length operator (#A) '#%s'", name(r1))

	return op.result, op.rCount
end

function COMPILER:Expression_IS(expr)
	local op = self:GetOperator("is", expr.result)

	if op then
		if not self:CheckState(op.state) then
			return false, expr
		elseif not op.operation then
			expr.result = op.type
			expr.rCount = op.count

			return true, expr
		else
			self:QueueInjectionBefore(inst, expr.token, "_OPS[\"" .. op.signature .. "\"](")

			if op.context then
			    self:QueueInjectionBefore(inst, expr.token, "CONTEXT", ",")
			end
			
			self:QueueInjectionAfter(inst, expr.final, ")" )

			self.__operators[op.signature] = op.operator

			expr.result = op.type
			expr.rCount = op.count

			return true, expr
		end
	elseif expr.result == "b" then
		return true, expr
	end

	return false, expr
end

function COMPILER:CastExpression(type, expr)

	local signature = string.format("(%s)%s", name(type), name(expr.result))
	
	local op = EXPR_CAST_OPERATORS[signature]

	if not op then
		return false, expr
	end

	if not self:CheckState(op.state) then
		return false, expr
	end

	self:QueueInjectionBefore(inst, expr.token, "_OPS[\"" .. op.signature .. "\"](")

	if op.context then
	    self:QueueInjectionBefore(inst, expr.token, "CONTEXT", ",")
	end
		
	self:QueueInjectionAfter(inst, expr.final, ")" )

	self.__operators[op.signature] = op.operator

	expr.result = op.type
	expr.rCount = op.count

	return true, expr
end

function COMPILER:Compile_CAST(inst, token, expressions)
	local expr = expressions[1]

	self:Compile(expr)

	local t = self:CastExpression(inst.class, expr)

	if not t then
		self:Throw(token, "Type of %s can not be cast to type of %s.", name(expr.result), name(inst.class))
	end

	return expr.result, expr.rCount
end

function COMPILER:Compile_VAR(inst, token, expressions)
	if self.__defined[inst.variable] then
		self:Throw(token, "Variable %s is defined here and can not be used as part of an expression.", inst.variable)
	end

	local c, s, var = self:GetVariable(inst.variable)

	if var and var.prefix then
		self:QueueReplace(inst, token, var.prefix .. "." .. token.data)
	end

	if not c then
		self:Throw(token, "Variable %s does not exist.", inst.variable)
	end

	return c, 1
end

function COMPILER:Compile_BOOL(inst, token, expressions)
	return "b", 1
end

function COMPILER:Compile_VOID(inst, token, expressions)
	return "", 1
end

function COMPILER:Compile_NUM(inst, token, expressions)
	return "n", 1
end

function COMPILER:Compile_STR(inst, token, expressions)
	return "s", 1
end

function COMPILER:Compile_PTRN(inst, token, expressions)
	return "_ptr", 1
end

function COMPILER:Compile_CLS(inst, token, expressions)
	self:QueueReplace(inst, token, "\"" .. token.data .. "\"")
	return "_cls", 1
end

function COMPILER:Compile_COND(inst, token, expressions)
	local expr = expressions[1]
	local r, c = self:Compile(expr)

	if r == "b" then
		return r, c
	end

	local op = self:GetOperator("is", r)

	if not op and self:CastExpression("b", expr) then
		return r, "b"
	end

	if not op then
		self:Throw(token, "No such condition (%s).", name(r))
	elseif not op.operation then
		-- Once again we change nothing.
	else
		self:QueueInjectionBefore(inst, expr.token, "_OPS[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr.token, "CONTEXT", ",")
		end
		
		self:QueueInjectionAfter(inst, expr.final, ")" )

		self.__operators[op.signature] = op.operator
	end

	return op.result, op.rCount
end

function COMPILER:Compile_NEW(inst, token, expressions)
	local op
	local ids = {}
	local total = #expressions

	local cls = EXPR_LIB.GetClass(inst.class)
	local constructors = cls.constructors

	if total == 0 then
		op = constructors[inst.class .. "()"]
	else
		for k, expr in pairs(expressions) do
			local r, c = self:Compile(expr)
			ids[#ids + 1] = r

			if k == total then
				if c > 1 then
					for i = 2, c do
						ids[#ids + 1] = r
					end
				end
			end
		end

		for i = #ids, 1, -1 do
			local args = table.concat(ids,",", 1, i)

			if i >= total then
				local signature = string.format("%s(%s)", inst.class, args)

				op = constructors[signature]
			end

			if not op then
				local signature = string.format("%s(%s,...)", inst.class, args)

				op = constructors[signature]
			end

			if op then
				break
			end
		end

		if not op then
			op = constructors[inst.class .. "(...)"]
			
			if op then
				vargs = 1
			end
		end
	end

	local signature = string.format("%s(%s)", name(inst.class), names(ids))

	if not op then
		self:Throw(token, "No such constructor, new %s", signature)
	end

	self:CheckState(op.state, token, "Constructor 'new %s", signature)

	if type(op.operator) == "function" then

		self:QueueRemove(inst, inst.__new)
		self:QueueRemove(inst, inst.__const)
		self:QueueRemove(inst, inst.__lpa)

		self:QueueInjectionBefore(inst, inst.__const, "_CONST[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, inst.__const, "CONTEXT")

		    if total > 0 then
				self:QueueInjectionBefore(inst, inst.__const, ",")
			end
		end

		self.__constructors[op.signature] = op.operator
	elseif type(op.operator) == "string" then
		self:QueueRemove(inst, inst.__new)
		self:QueueRemove(inst, inst.__const)
		self:QueueReplace(inst, inst.__const, op.operator)
	else
		local signature = string.format("%s.", inst.library, op.signature)
		error("Attempt to inject " .. op.signature .. " but operator was incorrect " .. type(op.operator) .. ".")
	end

	if vargs then
		if #expressions >= 1 then
			for i = vargs, #expressions do
				local arg = expressions[i]

				if arg.result ~= "_vr" then
					self:QueueInjectionBefore(inst, self:OffsetToken(arg.token, -1), "{", "\"" .. arg.result .. "\"", ",")

					self:QueueInjectionAfter(inst, arg.final, "}")
				end
			end
		end
	end

	return op.result, op.rCount
end

function COMPILER:Compile_METH(inst, token, expressions)
	local expr = expressions[1]
	local mClass, mCount = self:Compile(expr)

	local op
	local vargs
	local ids = {}
	local total = #expressions

	if total == 1 then
		op = EXPR_METHODS[string.format("%s.%s()", mClass, inst.method)]
		--print("method->", string.format("%s.%s()", mClass, inst.method), op)
	else
		for k, expr in pairs(expressions) do
			if k > 1 then
				local r, c = self:Compile(expr)

				ids[#ids + 1] = r

				if k == total then
					if c > 1 then
						for i = 2, c do
							ids[#ids + 1] = r
						end
					end
				end
			end
		end

		for i = #ids, 1, -1 do
			local args = table.concat(ids,",", 1, i)

			if i <= total then
				local signature = string.format("%s.%s(%s)", mClass, inst.method, args)

				op = EXPR_METHODS[signature]
				--print("method->", signature, op)
			end

			if not op then
				local signature = string.format("%s.%s(%s,...)", mClass, inst.method, args)

				op = EXPR_METHODS[signature]
				--print("method->", signature, op)

				if op then
					vargs = i
				end
			end

			if op then
				break
			end
		end

		if not op then
			op = EXPR_METHODS[string.format("%s.%s(...)", mClass, inst.method)]
				--print("method->", string.format("%s.%s(...)", mClass, inst.method), op)

			if op then
				vargs = 1
			end
		end
	end

	if not op then
		self:Throw(token, "No such method %s.%s(%s).", name(mClass), inst.method, names(ids))
	end

	self:CheckState(op.state, token, "Method %s.%s(%s)", name(mClass), inst.method, names(ids))


	if type(op.operator) == "function" then
		self:QueueRemove(inst, inst.__operator)
		self:QueueRemove(inst, inst.__method)

		if total == 1 then
			self:QueueRemove(inst, inst.__lpa)
		else
			self:QueueReplace(inst, inst.__lpa, ",")
		end

		self:QueueInjectionBefore(inst, expr.token, "_METH[\"" .. op.signature .. "\"](")

		if op.context then
		    self:QueueInjectionBefore(inst, expr.token , "CONTEXT,")
		end

		self.__methods[op.signature] = op.operator
	elseif type(op.operator) == "string" then
		self:QueueReplace(inst, inst.__operator, ":")
		self:QueueReplace(inst, inst.__method, op.operator)
	else
		local signature = string.format("%s.%s", name(inst.class), op.signature)
		error("Attempt to inject " .. op.signature .. " but operator was incorrect, got " .. type(op.operator))
	end

	if vargs then
		if #expressions > 1 then
			for i = vargs, #expressions do
				local arg = expressions[i]

				if arg.result ~= "_vr" then
					self:QueueInjectionBefore(inst, self:OffsetToken(arg.token, -1), "{", "\"" .. arg.result .. "\"", ",")

					self:QueueInjectionAfter(inst, arg.final, "}")
				end
			end
		end
	end

	return op.result, op.rCount
end

function COMPILER:Compile_FUNC(inst, token, expressions)
	local lib = EXPR_LIBRARIES[inst.library.data]

	if not lib then
		-- Please note this should be impossible.
		self:Throw(token, "Library %s does not exist.", inst.library.data)
	end

	local op
	local vargs
	local ids = {}
	local total = #expressions

	if total == 0 then
		op = lib._functions[inst.name .. "()"]
	else
		for k, expr in pairs(expressions) do
			local r, c = self:Compile(expr)

			ids[#ids + 1] = r

			if k == total then
				if c > 1 then
					for i = 2, c do
						ids[#ids + 1] = r
					end
				end
			end
		end

		for i = #ids, 1, -1 do
			local args = table.concat(ids,",", 1, i)

			if i >= total then
				local signature = string.format("%s(%s)", inst.name, args)

				op = lib._functions[signature]
			end

			if not op then
				local signature = string.format("%s(%s,...)", inst.name, args)

				op = lib._functions[signature]

				if op then
					vargs = i
				end
			end

			if op then
				break
			end
		end

		if not op then
			op = lib._functions[inst.name .. "(...)"]
			
			if op then
				vargs = 1
			end
		end
	end

	if not op then
		self:Throw(token, "No such function %s.%s(%s).", inst.library.data, inst.name, names(ids, ","))
	end

	self:CheckState(op.state, token, "Function %s.%s(%s).", inst.library.data, inst.name, names(ids, ","))

	if type(op.operator) == "function" then
		local signature = string.format("%s.%s", inst.library.data, op.signature)

		self:QueueRemove(inst, token)
		self:QueueRemove(inst, inst.library)
		self:QueueRemove(inst, inst.__operator)
		self:QueueRemove(inst, inst.__func)

		self:QueueInjectionAfter(inst, inst.__func, "_FUN[\"" .. signature .. "\"]")

		if op.context then
			self:QueueInjectionAfter(inst, inst.__lpa, "CONTEXT")

		    if total > 0 then
				self:QueueInjectionAfter(inst, inst.__lpa, ",")
			end
		end

		self.__functions[signature] = op.operator
	elseif type(op.operator) == "string" then
		self:QueueRemove(inst, token)
		self:QueueRemove(inst, inst.library)
		self:QueueRemove(inst, inst.__operator)
		self:QueueReplace(inst, inst.__func, op.operator) -- This is error.
		self:Import(op.operator)
	else
		local signature = string.format("%s.", inst.library, op.signature)
		error("Attempt to inject " .. signature .. " but operator was incorrect " .. type(op.operator) .. ".")
	end


	if vargs then
		if #expressions >= 1 then
			for i = vargs, #expressions do
				local arg = expressions[i]

				if arg.result ~= "_vr" then
					self:QueueInjectionAfter(inst, self:OffsetToken(arg.token, -1), "{", "\"" .. arg.result .. "\"", ",")

					self:QueueInjectionAfter(inst, arg.final, "}")
				end
			end
		end
	end

	if inst.library == "system" then
		local res, count = hook.Run("Expression3.PostCompile.System." .. inst.name, this, inst, token, expressions)
		
		if res and count then
			return res, count
		end
	end

	return op.result, op.rCount
end

--[[
]]

function COMPILER:Compile_LAMBDA(inst, token, expressions)
	self:PushScope()

	for _, peram in pairs(inst.perams) do
		local var = peram[2]
		local class = peram[1]

		self:AssignVariable(token, true, var, class)

		if class ~= "_vr" then
			injectNewLine = true
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("if %s == nil or %s[1] == nil then CONTEXT:Throw(\"%s expected for %s, got void\") end", var, var, name(class), var))
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("if %s[1] ~= %q then CONTEXT:Throw(\"%s expected for %s, got \" .. %s[1]) end", var, class, name(class), var, var))
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("%s = %s[2]", var, var))
			injectNewLine = false
		end
	end

	self:SetOption("udf", (self:GetOption("udf") or 0) + 1)

	self:SetOption("retunClass", "?") -- Indicate we do not know this yet.
	self:SetOption("retunCount", -1) -- Indicate we do not know this yet.

	self:Compile(inst.stmts)

	local result = self:GetOption("retunClass")
	local count = self:GetOption("retunCount")

	self:PopScope()

	if result == "?" or count == -1 then
		result = ""
		count = 0
	end

	self:QueueInjectionAfter(inst, inst.__end, ", result = \"" .. result .. "\"")
	self:QueueInjectionAfter(inst, inst.__end, ", count = " .. count)
	self:QueueInjectionAfter(inst, inst.__end, "}")

	return "f", 1
end

--[[
]]

function COMPILER:Compile_RETURN(inst, token, expressions)
	local result = self:GetOption("retunClass")
	local count = self:GetOption("retunCount")

	local results = {}

	for _, expr in pairs(expressions) do
		local r, c = self:Compile(expr)
		results[#results + 1] = {r, c}
	end

	local outClass

	if result == "?" then
		for i = 1, #results do
			local t = results[i][1]

			if not outClass then
				outClass = t
			elseif outClass ~= t then
				outClass = "_vr"
				break
			end
		end

		self:SetOption("retunClass", outClass or "", true)
	else
		outClass = result
	end

	local outCount = 0

	for i = 1, #results do
		local expr = expressions[i]
		local res = results[i][1]
		local cnt = results[i][2]

		if res ~= outClass then
			local ok = self:CastExpression(outClass, expr)

			if not ok then
				self:Throw(expr.token, "Can not return %s here, %s expected.", name(res), name(outClass))
			end
		end

		if i < #results then
			outCount = outCount + 1
		else
			outCount = outCount + cnt
		end
	end

	if count == -1 then
		count = outCount
		self:SetOption("retunCount", outCount, true)
	end

	if count ~= outCount then
		self:Throw(expr.token, "Can not return %i %s('s) here, %i %s('s) expected.", name(outCount), name(outClass), count, name(outClass))
	end
end

--[[
]]

function COMPILER:Compile_DELEGATE(inst, token, expressions)
	local class, scope, info = self:AssignVariable(token, true, inst.variable, "f")

	if info then
		info.signature = table.concat(inst.peramaters, ",")
		info.peramaters = inst.peramaters
		info.resultClass = inst.resultClass
		info.resultCount = inst.resultCount
	end
end

function COMPILER:Compile_FUNCT(inst, token, expressions)
	self:PushScope()

	for _, peram in pairs(inst.perams) do
		local var = peram[2]
		local class = peram[1]

		self:AssignVariable(token, true, var, class)

		if class ~= "_vr" then
			injectNewLine = true
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("if %s == nil or %s[1] == nil then CONTEXT:Throw(\"%s expected for %s, got void\") end", var, var, class, var))
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("if %s[1] ~= %q then CONTEXT:Throw(\"%s expected for %s, got \" .. %s[1]) end", var, class, class, var, var))
			self:QueueInjectionBefore(inst, inst.stmts.token, string.format("%s = %s[2]", var, var))
			injectNewLine = false
		end
	end

	self:SetOption("udf", (self:GetOption("udf") or 0) + 1)

	self:SetOption("retunClass", inst.resultClass)
	self:SetOption("retunCount", -1) -- Indicate we do not know this yet.

	self:Compile(inst.stmts)

	local count = self:GetOption("retunCount")

	self:PopScope()

	local variable = inst.variable

	local class, scope, info = self:AssignVariable(token, true, variable, "f")

	if info then
		info.signature = inst.signature
		info.peramaters = inst.perams
		info.resultClass = inst.resultClass
		info.resultCount = count

		if info.prefix then
			variable = info.prefix .. "." .. variable
		else
			self:QueueInjectionBefore(inst, token, "local")
		end
	end

	self:QueueInjectionBefore(inst, token, variable, " = { op = ")
	self:QueueInjectionAfter(inst, inst.__end, ", signature = \"" .. inst.signature .. "\"")
	self:QueueInjectionAfter(inst, inst.__end, ", result = \"" .. info.resultClass .. "\"")
	self:QueueInjectionAfter(inst, inst.__end, ", count = " .. count)
	self:QueueInjectionAfter(inst, inst.__end, "}")
end

--[[
]]

function COMPILER:Compile_CALL(inst, token, expressions)
	local expr = expressions[1]
	local res, count = self:Compile(expr)

	local prms = {}

	if #expressions > 1 then
		for i = 2, #expressions do
			local arg = expressions[i]
			local r, c = self:Compile(arg)

			prms[#prms + 1] = r

			if i == #expressions and c > 1 then
				for j = 2, c do
					prms[#prms + 1] = r
				end
			end
		end
	end

	local signature = table.concat(prms, ",")

	if res == "f" and expr.type == "var" then
		local c, s, info = self:GetVariable(expr.variable)
		-- The var instruction will have already validated this variable.
		
		if info.signature then
			if info.signature ~= signature then
				self:Throw(token, "Invalid arguments to user function got %s(%s), %s(%s) expected.", expr.variable, names(signature), expr.variable, names(info.signature))
			end

			if #expressions > 1 then
				for i = 2, #expressions do
					local arg = expressions[i]

					if arg.result ~= "_vr" then
						self:QueueInjectionBefore(inst, arg.token, "{", "\"" .. arg.result .. "\"", ",")

						self:QueueInjectionAfter(inst, arg.final, "}")
					end
				end
			end

			self:QueueReplace(inst, expr.token, "invoke")

			self:QueueInjectionAfter(inst, token, "(", "CONTEXT", ",\"" .. info.resultClass .. "\",", tostring(info.resultCount), ",")

			if info.prefix then
				self:QueueInjectionAfter(inst, token, info.prefix .. "." .. expr.variable)
			else
				self:QueueInjectionAfter(inst, token, expr.variable)
			end

			if #prms >= 1 then
				self:QueueInjectionAfter(inst, token, ",")
			end

			return info.resultClass, info.resultCount
		end
	end

	local op

	if #prms == 0 then
		op = self:GetOperator("call", res, "")

		if not op then
			op = self:GetOperator("call", res, "...")
		end
	else
		for i = #prms, 1, -1 do
			local args = table.concat(prms,",", 1, i)

			if i >= #prms then
				op = self:GetOperator("call", res, args)
			end

			if not op then
				op = self:GetOperator("call", args, res, "...")
			end

			if op then
				break
			end
		end
	end

	if not op then
		self:Throw(token, "No such call operation %s(%s)", name(res), names(prms))
	end

	self:CheckState(op.state, token, "call operation %s(%s).", name(res), names(prms))

	self:QueueRemove(inst, token) -- Removes (

	self:QueueInjectionBefore(inst, expr.token, "_OPS[\"" .. op.signature .. "\"]", "(")

	if op.context then
		self:QueueInjectionBefore(inst, expr.token, "CONTEXT", ",")
	end

	if #prms >= 1 then
		self:QueueInjectionBefore(inst, token, ",")
	end

	self.__operators[op.signature] = op.operator

	return op.result, op.rCount
end

--[[
]]

function COMPILER:Compile_GET(inst, token, expressions)
	local value = expressions[1]
	local vType = self:Compile(value)
	local index = expressions[2]
	local iType = self:Compile(index)

	local op
	local keepid = false
	local cls = inst.class

	if not cls then
		op = self:GetOperator("get", vType, iType)

		if not op then
			self:Throw(token, "No such get operation %s[%s]", name(vType), name(iType))
		end
	else
		op = self:GetOperator("get", vType, iType, cls.data)
		
		if not op then
			keepid = true

			self:QueueReplace(inst, cls, "\'" .. cls.data .. "\'")

			op = self:GetOperator("get", vType, iType, "_cls")

			if op then
				if op.result == "" then
					op.result = cls.data
					op.rCount = 1
				end
			end
		end

		if not op then
			self:Throw(token, "No such get operation %s[%s,%s]", name(vType), name(iType), name(cls.data))
		end
	end

	self:CheckState(op.state)

	if not op.operator then
		return op.result, op.rCount
	end

	self:QueueInjectionBefore(inst, value.token, "_OPS[\"" .. op.signature .. "\"](")

	if op.context then
	   self:QueueInjectionBefore(inst, value.token, "CONTEXT", ",")
	end

	if not keepid then
		self:QueueRemove(inst, cls)
	else
		self:QueueReplace(inst, cls, "'" .. cls.data .. "'")
	end

	self:QueueReplace(inst, inst.__rsb, ")" )

	self:QueueReplace(inst, inst.__lsb, "," )

	self.__operators[op.signature] = op.operator

	return op.result, op.rCount
end

function COMPILER:Compile_SET(inst, token, expressions)
	local value = expressions[1]
	local vType = self:Compile(value)
	local index = expressions[2]
	local iType = self:Compile(index)
	local expr = expressions[3]
	local vExpr = self:Compile(expr)

	local op
	local keepclass = false
	local cls = inst.class

	if cls and vExpr ~= cls.data then
		-- TODO: Cast
	end

	if not cls then
		op = self:GetOperator("set", vType, iType, vExpr)
	else
		op = self:GetOperator("set", vType, iType, cls.data)

		if not op then
			keepclass = true
			op = self:GetOperator("set", vType, iType, "_cls", vExpr)
		end
	end

	if not op then
		if not cls then
			self:Throw(token, "No such set operation %s[%s] = ", name(vType), name(iType), name(vExpr))
		else
			self:Throw(token, "No such set operation %s[%s, %s] = ", name(vType), name(iType), name(cls.data), name(vExpr))
		end
	end

	self:CheckState(op.state)

	if not op.operator then
		return op.result, op.rCount
	end

	self:QueueReplace(inst, token, "," )

	self:QueueInjectionBefore(inst, value.token, "_OPS[\"" .. op.signature .. "\"](")

	if op.context then
	   self:QueueInjectionBefore(inst, value.token, "CONTEXT", ",")
	end
	
	if not keepclass then
		self:QueueRemove(isnt, cls)
	else
		self:QueueReplace(isnt, cls, ", '" .. cls.data .. "'")
	end

	self:QueueRemove(inst, inst.__ass, ",")

	self:QueueReplace(inst, inst.__rsb, "," )
	  
	self:QueueInjectionAfter(inst, expr.final, ")")

	self.__operators[op.signature] = op.operator

	return op.result, op.rCount
end

--[[
]]

function COMPILER:Compile_FOR(inst, token, expressions)

	local start = expressions[1]
	local tStart = self:Compile(start)
	local _end = expressions[2]
	local tEnd = self:Compile(_end)
	local step = expressions[3]
	
	if not step and (inst.class ~= "n" or tStart  ~= "n" or tEnd ~= "n") then
		self:Throw(token, "No such loop 'for(%s i = %s %s)'.", name(inst.class), name(tStart), name(tEnd))
	elseif step then
		local tStep = self:Compile(step)
		if inst.class ~= "n" or tStart  ~= "n" or tEnd ~= "n" or tEnd ~= "n" or tStep ~= "n" then
			self:Throw(token, "No such loop 'for(%s i = %s %s %s)'.", name(inst.class), name(tStart), name(tEnd), name(tStep))
		end
	end

	self:PushScope()

	self:AssignVariable(token, true, inst.variable.data, inst.class, nil)

	self:Compile(inst.stmts)

	self:PopScope()
end

--[[

]]

function COMPILER:Compile_TRY(inst, token, expressions)
	self:QueueReplace(inst, token, "local")

	self:QueueInjectionAfter(inst, token, "ok", ",", inst.__var.data, "=", "pcall(")

	self:PushScope()

	self:Compile(inst.protected)

	self:PopScope()

	self:QueueInjectionAfter(inst, inst.protected.final, ")", "if", "(", "not", "ok", "and", inst.__var.data, ".", "state", "==", "'runtime'", ")")

	self:QueueRemove(inst, inst.__catch)
	self:QueueRemove(inst, inst.__lpa)
	self:QueueRemove(inst, inst.__var)
	self:QueueRemove(inst, inst.__rpa)

	self:PushScope()

	self:AssignVariable(token, true, inst.__var.data, "_er", nil)

	self:Compile(inst.catch)

	self:PopScope()

	self:QueueInjectionAfter(inst, inst.catch.final, "elseif not ok then error(", inst.__var.data, ", 0) end")
end

--[[
]]

function COMPILER:Compile_INPORT(inst, token)
	if self:GetOption("state") ~= EXPR_SERVER then
		self:Throw(token, "Wired input('s) must be defined server side.")
	end

	for _, token in pairs(inst.variables) do
		local var = token.data

		if var[1] ~= string.upper(var[1]) then
			self:Throw(token, "Invalid name for wired input %s, name must be cammel cased")
		end

		local class, scope, info = self:AssignVariable(token, true, var, inst.class, 0)

		if info then
			info.prefix = "INPUT"
		end

		self.__directives.inport[var] = {class = inst.class, wire = inst.wire_type, func = inst.wire_func}
	end
end

function COMPILER:Compile_OUTPORT(inst, token)
	if self:GetOption("state") ~= EXPR_SERVER then
		self:Throw(token, "Wired output('s) must be defined server side.")
	end

	for _, token in pairs(inst.variables) do
		local var = token.data

		if var[1] ~= string.upper(var[1]) then
			self:Throw(token, "Invalid name for wired output %s, name must be cammel cased")
		end

		local class, scope, info = self:AssignVariable(token, true, var, inst.class, 0)

		if info then
			info.prefix = "OUTPUT"
		end

		self.__directives.outport[var] = {class = inst.class, wire = inst.wire_type, func = inst.wire_func, func_in = inst.wire_func2}
	end
end


--[[
]]

function COMPILER:StartClass(name, scope)
	if not scope then
		scope = self.__scopeID
	end

	local classes = self.__scopeData[scope].classes

	local newclass = {name = name memory = {}}

	classes[name] = newclass

	return newclass
end

function COMPILER:GetUserClass(name, scope, nonDeep)
	if not scope then
		scope = self.__scopeID
	end

	local v = self.__scopeData[scope].classes[name]

	if v then
		return v.class, v.scope, v
	end

	if not nonDeep then
		for i = scope, 0, -1 do
			local v = self.__scopeData[i].classes[name]

			if v then
				return v.class, v.scope, v
			end
		end
	end
end

function COMPILER.AssToClass(token, declaired, varName, class, scope)
	local class, scope, info = self:AssignVariable(token, declaired, varName, class, scope)
	if declaired then
		local userclass = self:GetOption("userclass")
		userclass.memory[varName] = info
		inf.prefix = "self.vars"
	end
end



function COMPILER:Compile_CLASS(inst, token, stmts)
	self:PushScope()
		local class = self:StartClass(inst.__classname.data)
		
		self:SetOption("userclass", class)

		for i = 1, #stmts do
			self:Compile(stmts[i])
		end

	self:PopScope()

	-- inst.__classname
	self:QueueReplace(inst, token, "local")
	self:QueueRemove(inst, inst.__lcb)
	self:QueueInjectionAfter(inst, inst.__lcb, " =",  "{", "vars", "=", "{", "}", "}")
	self:QueueRemove(inst, inst.__rcb)

	return "", 0
end

function COMPILER:Compile_FEILD(inst, token, expressions)
	local expr = expressions[1]
	local type = self:Compile(expr)
	local userclass = self:GetUserClass(type)

	if not userclass then
		self:Throw(token, "Unable to refrence feild %s.%s here", name(type), inst.__feild.data)
	end

	local info = userclass.vars[inst.__feild.data]

	if not info then
		self:Throw(token, "No sutch feild %s.%s", type, inst.__feild.data)
	end

	return info.result, 1
end

function COMPILER:Compile_DEF_FEILD(inst, token, expressions)
	local tArgs = #expressions
	local userclass = self:GetOption("userclass")

	local tArgs = #expressions

	local results = {}

	for i = 1, tArgs do
		local arg = expressions[i]
		local r, c = self:Compile(arg)

		if not inst.variables[i] then
			self:Throw(arg.token, "Unable to assign here, value #%i has no matching variable.", i)
		elseif i < tArgs then
			results[#results + 1] = {r, arg, true}
		else
			for j = 1, c do
				results[#results + 1] = {r, arg, j == 1}
			end
		end
	end

	for i = 1, #inst.variables do
		local result = results[i]
		local token = inst.variables[i]
		local var = token.data

		if not result then
			self:Throw(token, "Unable to assign variable %s, no matching value.", var)
		end

		local class, scope, info = self:AssignVariable(token, true, var, inst.class, 0)

		if info then
			self:QueueReplace(inst, token, userclass.name .. ".vars." .. var)
		end

		self.__defined[var] = true

		if result[1] ~= inst.class then
			local casted = false
			local arg = result[2]

			if result[3] then
				-- TODO: CAST
			end

			if not casted then
				self:AssignVariable(arg.token, true, var, result[1], 0)
			end
		end
	end

	self.__defined = {}

	return "", 0
end

EXPR_COMPILER = COMPILER