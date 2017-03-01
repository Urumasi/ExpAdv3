--[[
	   ____      _  _      ___    ___       ____      ___      ___     __     ____      _  _          _        ___     _  _       ____   
	  F ___J    FJ  LJ    F _ ", F _ ",    F ___J    F __".   F __".   FJ    F __ ]    F L L]        /.\      F __".  FJ  L]     F___ J  
	 J |___:    J \/ F   J `-' |J `-'(|   J |___:   J (___|  J (___|  J  L  J |--| L  J   \| L      //_\\    J |--\ LJ |  | L    `-__| L 
	 | _____|   /    \   |  __/F|  _  L   | _____|  J\___ \  J\___ \  |  |  | |  | |  | |\   |     / ___ \   | |  J |J J  F L     |__  ( 
	 F L____:  /  /\  \  F |__/ F |_\  L  F L____: .--___) \.--___) \ F  J  F L__J J  F L\\  J    / L___J \  F L__J |J\ \/ /F  .-____] J 
	J________LJ__//\\__LJ__|   J__| \\__LJ________LJ\______JJ\______JJ____LJ\______/FJ__L \\__L  J__L   J__LJ______/F \\__//   J\______/F
	|________||__/  \__||__L   |__|  J__||________| J______F J______F|____| J______F |__L  J__|  |__L   J__||______F   \__/     J______F 

	::Tokenizer::
	`````````````
	A revamp of my expression advanced 2 tokenizer,
	Using code by Divran, Oskar94, and maybe a few others.
]]

local KEYWORDS = {
	EXPADV = {
		["if"] = {"if", "if"},
		["elseif"] = {"eif", "elseif"},
		["else"] = {"els", "else"},
		["while"] = {"whl", "while"},
		["for"] = {"for", "for"},
		["foreach"] = {"each", "foreach"},
		["delegate"] = {"del", "delegate"},
		["true"] = {"tre", "true"},
		["false"] = {"fls", "false"},
		["void"] = {"void", "void"},
		["break"] = {"brk", "break"},
		["continue"] = {"cnt", "continue"},
		["return"] = {"ret", "return"},
		["global"] = {"glo", "global"},
		["server"] = {"sv", "server"},
		["client"] = {"cl", "client"},
		["new"] = {"new", "constructor"},
		["try"] = {"try", "try"},
		["catch"] = {"cth", "catch"},
		["class"] = {"cls", "class"}
	}
}

local TOKENS = {
	EXPADV = {
		{ "+", "add", "addition" },
		{ "-", "sub", "subtract" },
		{ "*", "mul", "multiplier" },
		{ "/", "div", "division" },
		{ "%", "mod", "modulus" },
		{ "^", "exp", "power" },
		{ "=", "ass", "assign" },
		{ "+=", "aadd", "increase" },
		{ "-=", "asub", "decrease" },
		{ "*=", "amul", "multiplier" },
		{ "/=", "adiv", "division" },
		{ "++", "inc", "increment" },
		{ "--", "dec", "decrement" },
		{ "==", "eq", "equal" },
		{ "!=", "neq", "unequal" },
		{ "<", "lth", "less" },
		{ "<=", "leq", "less or equal" },
		{ ">", "gth", "greater" },
		{ ">=", "geq", "greater or equal" },
		{ "&", "band", "and" },
		{ "|", "bor", "or" },
		{ "^^", "bxor", "or" },
		{ ">>", "bshr", ">>" },
		{ "<<", "bshl", "<<" },
		{ "!", "not", "not" },
		{ "&&", "and", "and" },
		{ "||", "or", "or" },
		{ "?", "qsm", "?" },
		{ ":", "col", "colon" },
		{ "", "sep", "semicolon" },
		{ ",", "com", "comma" },
		{ "$", "dlt", "delta" },
		{ "#", "len", "length" },
		--{ "~", "cng", "changed" },
		--{ "->", "wc", "connect" },
		{ ".", "prd", "period" },
		{ "(", "lpa", "left parenthesis" },
		{ ")", "rpa", "right parenthesis" },
		{ "{", "lcb", "left curly bracket" },
		{ "}", "rcb", "right curly bracket" },
		{ "[", "lsb", "left square bracket" },
		{ "]", "rsb", "right square bracket" },
		{ '@', "dir", "directive operator" },
		{ "...", "varg", "varargs" },
	}
}


table.sort( TOKENS.EXPADV, function( token, token2 )
	return #token[1] > #token2[1]
end )

--[[
	Notes: 	I plan on possibly making this compiler multi language capable.
]]

local TOKENIZER = {}
TOKENIZER.__index = TOKENIZER

function TOKENIZER.New(lang)
	return setmetatable({}, TOKENIZER)
end

function TOKENIZER:Initalize(lang, script)
	if KEYWORDS[lang] and TOKENS[lang] then
		self.__pos = 0
		self.__offset = 0
		self.__depth = 0

		self.__char = ""
		self.__data = ""
		self.__dataStart = 1
		self.__dataEnd = 1

		self.__tokenPos = 0
		self.__tokenLine = 0
		self.__tokenChar = 0

		self.__readChar = 1
		self.__readLine = 1

		self.__tokens = {}
		self.__script = script
		self.__buffer = script
		self.__lengh = string.len(script)

		self.language = lang
		self.tokens = TOKENS[lang]
		self.keywords = KEYWORDS[lang]

		self:NextChar()
	else
		return nil, "No such language."
	end
end

function TOKENIZER.Run(this)
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

function TOKENIZER._Run(this)
	while self.__char ~= nil do
		self:Loop()
	end

	local result = {}
	result.tokens = self.__tokens
	result.script = self.__buffer

	return result
end

function TOKENIZER:Throw(offset, msg, fst, ...)
	local err = {}

	if fst then
		msg = string.format(msg, fst, ...)
	end

	err.state = "tokenizer"
	err.char = self.__readChar + offset
	err.line = self.__readLine
	err.msg = msg

	error(err,0)
end

--[[
]]

function TOKENIZER.NextChar(this)
	self.__dataEnd = self.__dataEnd + 1
	self.__data = self.__data .. self.__char
	self:SkipChar()
end

function TOKENIZER.PrevChar(this)
	self.__dataEnd = self.__dataEnd - 2
	self.__pos = self.__pos - 2
	self.__data = string.sub(self.__data, 0, #self.__data - 2)
	self:SkipChar()
end

function TOKENIZER.SkipChar(this)
	if self.__lengh < self.__pos then
		self.__char = nil
	elseif self.__char == "\n" then
		self:PushLine()
	else
		self:PushChar()
	end
end

function TOKENIZER.PushLine(this)
	self.__readLine = self.__readLine + 1
	self.__readChar = 1

	self.__pos = self.__pos + 1
	self.__char = string.sub(self.__script, self.__pos, self.__pos)
end

function TOKENIZER.PushChar(this)
	self.__readChar = self.__readChar + 1

	self.__pos = self.__pos + 1
	self.__char = string.sub(self.__script, self.__pos, self.__pos)
end

function TOKENIZER.Clear(this)
	self.__data = ""
	self.__match = ""
	self.__dataStart = self.__pos
	self.__dataEnd = self.__pos
end

--[[
]]

function TOKENIZER:NextPattern(pattern, exact)
	if self.__char == nil then
		return false
	end

	local s, e, r = string.find(self.__script, pattern, self.__pos, exact)

	if s ~= self.__pos then
		return false
	end

	if not r then
		r = string.sub(self.__script, s, e)
	end

	self.__pos = e + 1
	self.__dataStart = s
	self.__dataEnd = e
	self.__data = self.__data .. r

	self.__match = r

	if self.__pos > self.__lengh then
		self.__char = nil
	else
		self.__char = string.sub(self.__script, self.__pos, self.__pos)
	end

	local ls = string.Explode("\n", r)

	if #ls > 1 then
		self.__readLine = self.__readLine + #ls - 1
		self.__readChar = string.len(ls[#ls]) + 1
	else
		self.__readChar = self.__readChar + string.len(ls[#ls])
	end

	return true
end

function TOKENIZER:MatchPattern(pattern, exact)
	local s, e, r = string.find(self.__script, pattern, self.__pos, exact)

	if s ~= self.__pos then
		return false
	end

	return true, string.sub(self.__script. self.__pos, self.__pos)
end

function TOKENIZER:NextPatterns(exact, pattern, pattern2, ...)
	if self:NextPattern(pattern, exact) then
		return true
	end

	if pattern2 then
		return self:NextPatterns(exact, pattern2, ...)
	end

	return false
end

--[[
]]

function TOKENIZER:CreateToken(type, name, data, origonal)

	if not data then
		data = self.__data
	end

	local tkn = {}
	tkn.type = type
	tkn.name = name
	tkn.data = data

	tkn.start = self.__dataStart + self.__offset
	tkn.stop = self.__dataEnd + self.__offset
	tkn.pos = self.__pos
	tkn.char = self.__readChar
	tkn.line = self.__readLine
	tkn.depth = self.__depth
	tkn.orig = origonal
	
	local prev = self.__tokens[#self.__tokens]

	if prev and prev.line < tkn.line then
		tkn.newLine = true
	end

	tkn.index = #self.__tokens + 1
	self.__tokens[tkn.index] = tkn
end

--[[
]]

function TOKENIZER.SkipSpaces(this)
	self:NextPattern("^[%s\n]*")

	local r = self.__match

	self:Clear()

	return r
end

function TOKENIZER.SkipComments(this)
	if self:NextPattern("^/%*.-%*/") or self:NextPattern("^//.-\n") then
		self.__data = ""
		self.__skip = true
		return true
	elseif self:NextPattern("/*", true) then
		self:Error(0, "Un-terminated multi line comment (/*)", 0)
	else
		return false
	end
end

function TOKENIZER:Replace(str)
	local len = string.len(self.__data) - string.len(str)
	
	self.__data = str

	self.__offset = self.__offset + len
end

--[[
]]

function TOKENIZER.Loop(this)
	if self.__char == nil then
		return false
	end

	self:SkipSpaces()

	-- Comments need to be (--[[]] && --) not (/**/ & //)
	-- Comments also need to be ignored.

	local skip = false

	if self:NextPattern("^/%*.-%*/") then
		skip = true
		local cmnt = "--[[" .. string.sub(self.__data, 3, string.len(self.__data) - 2) .. "]]"
		self:Replace(cmnt)
	elseif self:NextPattern("/*", true) then
		self:Throw(0, "Un-terminated multi line comment (/*)", 0)
	elseif self:NextPattern("^//.-\n") then
		skip = true
		local cmnt = "--" .. string.sub(self.__data, 3)
		self:Replace(cmnt)
	end

	if skip then
		self:Clear()
		return true
	end

	-- Numbers

	if self:NextPattern("^0x[%x]+") then
		local n = tonumber(self.__data)

		if not n then
			self:Throw(0, "Invalid number format (%s)", 0, self.__data)
		end

		self:CreateToken("num", "hex", n)

		return true
	end

	if self:NextPattern("^0b[01]+") then
		local n = tonumber(string.sub(self.__data, 3), 2)

		if not n then
			self:Throw(0, "Invalid number format (%s)", 0, self.__data)
		end

		self:CreateToken("num", "bin", n)

		return true
	end

	if self:NextPattern("^%d+%.?%d*") then
		local n = tonumber(self.__data)

		if not n then
			self:Throw(0, "Invalid number format (%s)", 0, self.__data)
		end

		self:CreateToken("num", "real", n)

		return true
	end

	-- Strings
	
	local pattern = false

	if self.__char == "@" then
		self:SkipChar()

		if not (self.__char == '"' or self.__char == "'") then
			self:PrevChar()
		else
			pattern = true
		end
	end

	if self.__char == '"' or self.__char == "'" then
		local strChar = self.__char

		local escp = false

		self:SkipChar()

		while self.__char do
			local c = self.__char

			if c == "\n" then
				if strChar == "'" then
					self:NextChar()
				else
					break
				end
			elseif not escp then
				if c == strChar then
					break
				elseif c == "\\" then
					escp = true
					self:SkipChar()
					-- Escape sequence.
				else
					self:NextChar()
				end
			elseif c == "\\" then
				escp = false
				self:NextChar()
			elseif c == strChar then
				escp = false
				self.__char = "\n"
				self:NextChar()
			elseif c == "t" then
				escp = false
				self.__char = "\t"
				self:NextChar()
			elseif c == "r" then
				escp = false
				self.__char = "\r"
				self:NextChar()
			elseif self:NextPattern("^([0-9]+)") then
				local n = tonumber(self.__match)

				if not n or n < 0 or n > 255 then
					self:Throw(0, "Invalid char (%s)", n)
				end

				escp = false
				self.__pos = self.__pos - 1
				self.__data = self.__data .. string.char(n)
				self:SkipChar()
			else
				self:Throw(0, "Unfinished escape sequence (\\%s)", self.__char)
			end
		end

		if self.__char and self.__char == strChar then
			self:SkipChar()

			-- Multi line strings need to be converted to lua syntax.
			if strChar == "'" then
				local str = "[[" .. string.sub(self.__data, 1, string.len(self.__data)) .. "]]"
				self:Replace(str)
			else
				local str = "\"" .. string.sub(self.__data, 1, string.len(self.__data)) .. "\""
				self:Replace(str)
			end

			if not pattern then
				self:CreateToken("str", "string")
			else
				self:CreateToken("ptr", "string pattern")
			end

			return true
		end

		local str = self.__data

		if string.len(str) > 10 then
			str = string.sub(str, 0, 10) .. "..."
		end

		self:Throw(0, "Unterminated string (\"%s)", str)
	end

	-- Classes
	
	for k, v in pairs(EXPR_CLASSES) do
		if self:NextPattern("%( *" .. k .. " *%)") then
			self:CreateToken("cst", "cast", v.id, k)
			return true
		end

		if self:NextPattern(k, true) then
			self:CreateToken("typ", "type", v.id, k)
			return true
		end
	end

	-- Keywords.

	if self:NextPattern("^[a-zA-Z][a-zA-Z0-9_]*") then
		local w = self.__data
		local tkn = self.keywords[w]

		if tkn then
			self:CreateToken(tkn[1], tkn[2])
		else
			self:CreateToken("var", "variable")
		end
		
		return true
	end

	-- Ops

	for k = 1, #self.tokens, 1 do
		local v = self.tokens[k]
		local op = v[1]

		if self:NextPattern(op, true) then
			if op == "}" then
				self.__depth = self.__depth - 1
			end

			self:CreateToken(v[2], v[3])

			if op == "{" then
				self.__depth = self.__depth + 1
			end

			return true
		end
	end

	if not self.__char or self.__char == "" then
		self.__char = nil
	else
		self:Throw(0, "Unknown syntax found (%s)", tostring(self.__char))
	end
end


--[[
]]

EXPR_TOKENS = TOKENS
EXPR_KEYWORDS = KEYWORDS
EXPR_TOKENIZER = TOKENIZER
