--[[
	   ____      _  _      ___    ___       ____      ___      ___     __     ____      _  _          _        ___     _  _       ____   
	  F ___J    FJ  LJ    F _ ", F _ ",    F ___J    F __".   F __".   FJ    F __ ]    F L L]        /.\      F __".  FJ  L]     F___ J  
	 J |___:    J \/ F   J `-' |J `-'(|   J |___:   J (___|  J (___|  J  L  J |--| L  J   \| L      //_\\    J |--\ LJ |  | L    `-__| L 
	 | _____|   /    \   |  __/F|  _  L   | _____|  J\___ \  J\___ \  |  |  | |  | |  | |\   |     / ___ \   | |  J |J J  F L     |__  ( 
	 F L____:  /  /\  \  F |__/ F |_\  L  F L____: .--___) \.--___) \ F  J  F L__J J  F L\\  J    / L___J \  F L__J |J\ \/ /F  .-____] J 
	J________LJ__//\\__LJ__|   J__| \\__LJ________LJ\______JJ\______JJ____LJ\______/FJ__L \\__L  J__L   J__LJ______/F \\__//   J\______/F
	|________||__/  \__||__L   |__|  J__||________| J______F J______F|____| J______F |__L  J__|  |__L   J__||______F   \__/     J______F 

	::Expression 3 Base::
]]

print("expr3->cl_init")
include("shared.lua")

--[[
]]

net.Receive("Expression3.RequestUpload", function(len)
	local ent = net.ReadEntity()

	timer.Create("Expression3.SubmitToServer", 1, 1, function()
		if IsValid(ent) and ent.SubmitToServer then
			ent:SubmitToServer(Golem.GetCode())
		end
	end)
end)

function ENT:SubmitToServer(code)
	if code and code ~= "" then
		local ok, res = self:Validate(code)

		if ok then
			net.Start("Expression3.SubmitToServer")
				net.WriteEntity(self)
				net.WriteString(code)
			net.SendToServer()
		else
			self:HandelThrown(res)
			chat.AddText("Failed to validate script (see console).")
		end
	end
end

--[[
]]

net.Receive("Expression3.SendToClient", function(len)
	local ent = net.ReadEntity()
	local ply = net.ReadEntity()
	local script = net.ReadString()

	if script and script ~= "" then
		if ent and IsValid(ent) and ent.ReceiveFromServer then
			if ply and IsValid(ply) then
				ent:ReceiveFromServer(ply, script)
			end
		end
	end
end)

function ENT:ReceiveFromServer(ply, script)
	timer.Simple(1, function()
		if IsValid(self) then
			self:SetCode(script, true)
		end
	end)
end

function ENT:PostInitScript()

end

-- function ENT:GetOverlayText()
function ENT:GetOverlayData()
	return {txt = table.concat({
		"::Expression 3::",
		self:GetPlayerName(),
		self:GetScriptName() or "generic",
		"----------------------",
		"SV average: " .. self:GetServerAverageCPU(),
		"SV total:" .. self:GetServerTotalCPU(),
		"SV warning:" .. tostring(self:GetServerWarning()),
		"----------------------",
		"CL average: " .. self:GetClientAverageCPU(),
		"CL total:" .. self:GetClientTotalCPU(),
		"CL warning:" .. tostring(self:GetClientWarning()),
	}, "\n")}
end

--[[
]]

function ENT:SendToOwner(bConsole, ...)
	local owner = self:GetPlayer()

	if owner == LocalPlayer() then
		Golem.Print(...)
	else
		local const = bConsole and EXPR_CONSOLE or EXPR_CHAT
		EXPR_LIB.SendToClient(owner, self, const, ...)
	end
end