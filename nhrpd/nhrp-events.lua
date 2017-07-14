#!/usr/bin/lua5.2

-- Example NHRP events processing script which validates
-- NHRP registration GRE address against certificate subjectAltName IP
-- and auto-creates BGP pairings and filters based on sbgp extensions.

-- Depends on lua5.2 lua5.2-posix lua5.2-cqueues lua5.2-ossl lua-asn1

local posix = require 'posix'
local struct = require 'struct'
local cq = require 'cqueues'
local cqs = require 'cqueues.socket'
local x509 = require 'openssl.x509'
local x509an = require 'openssl.x509.altname'
local rfc3779 = require 'asn1.rfc3779'

local SOCK = "/var/run/nhrp-events.sock"
posix.unlink(SOCK)

local loop = cq.new()
local nulfd = posix.open("/dev/null", posix.O_RDWR)
local listener = cqs.listen{path=SOCK}

posix.chown(SOCK, "quagga", "quagga")
posix.setpid("u", "quagga")
posix.setpid("g", "quagga")
posix.openlog("nhrp-events", "np")

function string.hex2bin(str)
	return str:gsub('..', function(cc) return string.char(tonumber(cc, 16)) end)
end

local function decode_ext(cert, name, tpe)
	local ext = cert:getExtension(name)
	if not ext then return end
	return tpe.decode(ext:getData())
end

local function do_parse_cert(cert, out)
	for type, value in pairs(cert:getSubjectAlt()) do
		if type == 'IP' then
			table.insert(out.GRE, value)
		end
	end
	if #out.GRE == 0 then return end

	local asn = decode_ext(cert, 'sbgp-autonomousSysNum', rfc3779.ASIdentifiers)
	if asn and asn.asnum and asn.asnum.asIdsOrRanges then
		for _, as in ipairs(asn.asnum.asIdsOrRanges) do
			if as.id then
				out.AS = tonumber(as.id)
				break
			end
		end
	end

	local addrBlocks = decode_ext(cert, 'sbgp-ipAddrBlock', rfc3779.IPAddrBlocks)
	for _, ab in ipairs(addrBlocks or {}) do
		if ab.ipAddressChoice and ab.ipAddressChoice.addressesOrRanges then
			for _, a in ipairs(ab.ipAddressChoice.addressesOrRanges) do
				if a.addressPrefix then
					table.insert(out.NET, a.addressPrefix)
				end
			end
		end
	end

	return true
end

local function parse_cert(certhex)
	local out = {
		cn = "(no CN)",
		AS = 0,
		GRE = {},
		NET = {},
	}
	local cert = x509.new(certhex:hex2bin(), 'der')
	out.cn = tostring(cert:getSubject())
	-- Recognize hubs by certificate's CN to have OU=Hubs
	out.hub = out.cn:match("/OU=Hubs/") and true or nil
	do_parse_cert(cert, out)
	return out
end

local function execute(desc, cmd, ...)
	local piper, pipew = posix.pipe()
	if piper == nil then
		return error("Pipe failed")
	end

	local pid = posix.fork()
	if pid == -1 then
		return error("Fork failed")
	end
	if pid == 0 then
		posix.close(piper)
		posix.dup2(nulfd, 0)
		posix.dup2(pipew, 1)
		posix.dup2(nulfd, 2)
		posix.execp(cmd, ...)
		os.exit(1)
	end
	posix.close(pipew)

	-- This blocks -- perhaps should handle command executions in separate queue.
	local output = {}
	while true do
		local d = posix.read(piper, 8192)
		if d == nil or d == "" then break end
		table.insert(output, d)
	end
	posix.close(piper)

	local _, reason, status = posix.wait(pid)
	if status == 0 then
		posix.syslog(6, ("Executed '%s' successfully"):format(desc))
	else
		posix.syslog(3, ("Failed to execute '%s': %s %d"):format(desc, reason, status))
	end
	return status, table.concat(output)
end

local function configure_bgp(desc, ...)
	local args = {
		"-d", "bgpd",
		"-c", "configure terminal",
	}
	for _, val in ipairs({...}) do
		table.insert(args, "-c")
		table.insert(args, val)
	end
	return execute(desc, "vtysh", table.unpack(args))
end

local last_bgp_reset = 0

local function bgp_reset(msg, local_cert)
	local now = os.time()
	if last_bgp_reset + 60 > now then return end
	last_bgp_reset = now

	configure_bgp("spoke reset",
		"route-map RTT-SET permit 10", "set metric rtt", "exit",
		"route-map RTT-ADD permit 10", "set metric +rtt", "exit",
		("router bgp %d"):format(local_cert.AS),
		"no neighbor hubs",
		"neighbor hubs peer-group",
		"neighbor hubs remote-as 65000",
		"neighbor hubs ebgp-multihop 1",
		"neighbor hubs disable-connected-check",
		"neighbor hubs timers 10 30",
		"neighbor hubs timers connect 10",
		"neighbor hubs next-hop-self all",
		"neighbor hubs soft-reconfiguration inbound",
		"neighbor hubs route-map RTT-ADD in")
end

local function bgp_nhs_up(msg, remote_cert, local_cert)
	configure_bgp(("nhs-up %s"):format(msg.remote_addr),
		("router bgp %s"):format(local_cert.AS),
		("neighbor %s peer-group hubs"):format(msg.remote_addr))
end

local function bgp_nhs_down(msg, remote_cert, local_cert)
	configure_bgp(("nhs-down %s"):format(msg.remote_addr),
		("router bgp %s"):format(local_cert.AS),
		("no neighbor %s"):format(msg.remote_addr))
end

local function bgp_create_spoke_rules(msg, remote_cert, local_cert)
	if not local_cert.hub then return end

	local bgpcfg = {}
	for seq, net in ipairs(remote_cert.NET) do
		table.insert(bgpcfg,
			("ip prefix-list net-%s-in seq %d permit %s le %d"):format(
				msg.remote_addr, seq * 5, net,
				remote_cert.hub and 32 or 26))
	end
	table.insert(bgpcfg, ("router bgp %s"):format(local_cert.AS))
	if remote_cert.hub then
		table.insert(bgpcfg, ("neighbor %s peer-group hubs"):format(msg.remote_addr))
	elseif local_cert.AS == remote_cert.AS then
		table.insert(bgpcfg, ("neighbor %s peer-group spoke-ibgp"):format(msg.remote_addr))
	else
		table.insert(bgpcfg, ("neighbor %s remote-as %s"):format(msg.remote_addr, remote_cert.AS))
		table.insert(bgpcfg, ("neighbor %s peer-group spoke-ebgp"):format(msg.remote_addr))
	end
	table.insert(bgpcfg, ("neighbor %s prefix-list net-%s-in in"):format(msg.remote_addr, msg.remote_addr))

	local status, output = configure_bgp(("nhc-register %s"):format(msg.remote_addr), table.unpack(bgpcfg))
	if output:find("Cannot") then
		posix.syslog(6, "BGP: "..output)
		configure_bgp(
			("nhc-recreate %s"):format(msg.remote_addr),
			("router bgp %s"):format(local_cert.AS),
			("no neighbor %s"):format(msg.remote_addr),
			table.unpack(bgpcfg))
	end
end

local function handle_message(msg)
	if msg.event ~= "authorize-binding" then return end

	-- Verify protocol address against certificate
	local auth = false
	local local_cert = parse_cert(msg.local_cert)
	local remote_cert = parse_cert(msg.remote_cert)
	for _, gre in pairs(remote_cert.GRE) do
		if gre == msg.remote_addr then auth = true end
	end
	if not auth then
		posix.syslog(3, ("GRE %s to NBMA %s DENIED (cert '%s', allows: %s)"):format(
			msg.remote_addr, msg.remote_nbma,
			remote_cert.cn, table.concat(remote_cert.GRE, " ")))
		return "deny"
	end
	posix.syslog(6, ("GRE %s to NBMA %s authenticated for %s"):format(
		msg.remote_addr, msg.remote_nbma, remote_cert.cn))

	-- Automatic BGP binding for hub-spoke connections
	if msg.type == "nhs" and msg.old_type ~= "nhs" then
		if not local_cert.hub then
			if tonumber(msg.num_nhs) == 0 and msg.vc_initiated == "yes" then
				bgp_reset(msg, local_cert)
			end
			bgp_nhs_up(msg, remote_cert, local_cert)
		else
			bgp_create_spoke_rules(msg, remote_cert, local_cert)
		end
	elseif msg.type ~= "nhs" and msg.old_type == "nhs" then
		bgp_nhs_down(msg, remote_cert, local_cert)
	elseif msg.type == "dynamic" and msg.old_type ~= "dynamic" then
		bgp_create_spoke_rules(msg, remote_cert, local_cert)
	end

	return "accept"
end

local function handle_connection(conn)
	local msg = {}
	for l in conn:lines() do
		if l == "" then
			res = handle_message(msg)
			if msg.eventid then
				conn:write(("eventid=%s\nresult=%s\n\n"):format(msg.eventid, res or "default"))
			end
			msg = {}
		else
			local key, value = l:match('([^=]*)=(.*)')
			if key and value then
				msg[key] = value
			end
		end
	end
	conn:close()
end

loop:wrap(function()
	while true do
		local conn = listener:accept()
		conn:setmode("b", "bl")
		loop:wrap(function()
			local ok, msg = pcall(handle_connection, conn)
			if not ok then posix.syslog(3, msg) end
			conn:close()
		end)
	end
end)

print(loop:loop())
