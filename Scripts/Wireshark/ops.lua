--
-- A simple Wireshark dissector for OPS4 (Mulicast and UDP transports)
-- Author: Jakob Lindgren
--
-- Notes: Update port range at bottom of script
--
ops_proto = Proto("ops","OPS Protocol")


local function loadTypeFactory()
	package.prepend_path("opsTypes")
	require("types")
end
pcall(loadTypeFactory)


if OPS_Factory==nil then
	OPS_Factory={}
end



local function loadFactory()
	do
		local _types={}
		_types[1] = {type="c",name="messageType"}
		_types[2] = {type="c",name="publisherPriority"}
		_types[3] = {type="q",name="publicationID"}
		_types[4] = {type="s",name="publisherName"}
		_types[5] = {type="s",name="topicName"}
		_types[6] = {type="s",name="topLevelKey"}
		_types[7] = {type="s",name="address"}
		_types[8] = {type="o",name="data"}
		OPS_Factory["ops.protocol.OPSMessage"] = _types
	end
end

loadFactory()




local function OPS_addObject(buffer,tree,typename)

	local index = OPS_addToTree(buffer,tree,"s","key")

	for subname in string.gmatch(typename, "%S+") do
		for s,t in pairs(OPS_Factory) do
			if s == subname then
				return index + OPS_addToTree(buffer(index,-1),tree,"t",s)
			end
		end
	end
	return index
end


function OPS_addToTree(buffer,tree,type,name)
	index = 0
	if type == "s" then
		local strLen = buffer(0,4):le_int()
		local value = "<empty>"
		if strLen>0 then
			value = buffer(4,strLen):string()
		end
		tree:add(buffer(0,strLen+4),name .. ": " .. value)
		return strLen+4
	elseif type == "c" then
		value = buffer(0,1):le_int()
		tree:add(buffer(0,1),name .. ": " .. value)
		return 1
	elseif type == "h" then
		value = buffer(0,2):le_int()
		tree:add(buffer(0,2),name .. ": " .. value)
		return 2
	elseif type == "i" then
		value = buffer(0,4):le_int()
		tree:add(buffer(0,4),name .. ": " .. value)
		return 4
	elseif type == "q" then
		value = buffer(0,8):le_int64()
		tree:add(buffer(0,8),name .. ": " .. value)
		return 8
	elseif type == "t" then
		local objecttree = tree:add(ops_proto,buffer(),name)
		for k,t in pairs(OPS_Factory[name]) do
			index = index + OPS_addToTree(buffer(index,-1),objecttree,t.type,t.name)
		end
		return index
	elseif type == "o" then
		local strLen = buffer(0,4):le_int()
		typename = buffer(4,strLen):string()

		local objecttree = tree:add(ops_proto,buffer(),name)
		objecttree:add(buffer(0,strLen+4),"typename: " .. typename)

		return 4+strLen +OPS_addObject(buffer(strLen+4,-1),objecttree,typename)
	else
	end
end

function ops_proto.dissector(buffer,pinfo,tree)
	local protocol = buffer(0,4):string()
	local version = buffer(4,2):le_uint()

	if protocol=="opsp" and version==5 then
    	pinfo.cols.protocol = "OPS"
    	local opstree = tree:add(ops_proto,buffer(),"OPS")
    	local segmenttree = opstree:add(ops_proto,buffer(),"segment")
    	local NumberOfSegments = buffer(6,4):le_uint()
    	segmenttree:add(buffer(0,4),"protocol: " .. protocol)
    	segmenttree:add(buffer(4,2),"version: " .. version)
    	segmenttree:add(buffer(6,4),"Number of segments: " .. NumberOfSegments)
    	segmenttree:add(buffer(10,4),"Current Segment: " .. buffer(10,4):le_uint())
    	if NumberOfSegments==1 then
			--local messagetree = opstree:add(ops_proto,buffer(),"payload")
			--local index = OPS_decode(buffer(14,-1),messagetree)
			--local sparetree = opstree:add(ops_proto,buffer(index+14,-1),"Spare")

			local index = OPS_addToTree(buffer(14,-1),opstree,"o","Payload")
			local sparetree = opstree:add(ops_proto,buffer(index+14,-1),"Spare")
    	end
	end
end

udp_table = DissectorTable.get("udp.port")
udp_table:add("6000-10000",ops_proto)
