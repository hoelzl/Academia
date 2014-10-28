-- IASON Argos Setup
here = string.match(arg[0], "^.*/") or "./"
package.path = here.."?.lua;"
    ..here.."../hades/?.lua;"
    ..here.."../hades/lib/?.lua;"
    ..here.."../hades/hexameter/?.lua;"
    ..package.path
local hexameter = require "hexameter"
local serialize = require "serialize"
local ostools   = require "ostools"
local tartaros  = require "tartaros"
local xml       = require "lua2xml"
local show      = serialize.presentation

--using globals here, it, there, world, metaworld, iason

local parameters = ostools.parametrize(arg, {}, function(a, argument, message) print(a, argument, message) end)

if parameters.H or parameters.h or parameters.help then
    io.write("TODO: Create help file. Please understand.")
    io.write("\n")
    os.exit()
end

it= parameters.world and ostools.expand(parameters.world)
    or parameters[1] and ostools.expand(parameters[1])
    or ostools.usrerr("Please pass a world file as a parameter to Iason")


--handy for debugging
--print(show(parameters))
--print(show(environment.addresses))

-- load world file -----------------------------------------------------------------------------------------------------

io.write("::  Loading "..it.."...\n")
tartaros.setup(parameters.tartaros or parameters.tar)
world = dofile(it)
metaworld = getmetatable(world or {})
iason = metaworld.iason or {}
there = ostools.dir(it)


-- set up environment --------------------------------------------------------------------------------------------------

environment = {
    world = it,
    stage = parameters.stage
        or parameters[2]
        or iason.stage
        or "./",
    controller = parameters.controller
        or parameters[3]
        or iason.controller
        or nil,
    template = parameters.template
        or iason.template
        or nil,
    addresses =
        parameters.addresses and ostools.select(parameters.addresses)
        or parameters.ports and ostools.select(parameters.ports, function(name) return "localhost:"..name end)
        or iason.addresses and ostools.select(iason.addresses)
        or iason.ports and ostools.select(iason.ports, function(name) return "localhost:"..name end)
        or ostools.select("localhost:55655,...,localhost:55695"),
    iason =
        parameters.name
        or parameters.iason
        or iason.iason
        or iason.me
        or iason.port
        or nil,
    tartaros =
        parameters.tartaros
        or parameters.tar
        or nil,
    hexameter =
        parameters.hexameter
        or parameters.hex
        or iason.hexameter
        or nil,
    dryrun = parameters.T or false,
}


-- set up resource management ------------------------------------------------------------------------------------------

local addresspool = environment.addresses
local usedaddresses = {}
local function address(preferred)
    if preferred and not usedaddresses[preferred] then
        if type(preferred) == "string" then
            local todelete = nil
            for a,address in ipairs(addresspool) do
                if address == preferred then
                    todelete = a
                end
            end
            table.remove(addresspool, todelete)
            usedaddresses[preferred] = true
        end
        return preferred
    end
    local best = addresspool[1]
    table.remove(addresspool, 1)
    usedaddresses[best] = true
    return best
end

local function file(name)
    return there..(environment.stage)..name
end

local robots = {}
local function robot(type, preferrednumber)
    robots[type] = robots[type] or {next = 1}
    if preferrednumber and (not robots[type][preferrednumber]) then
        robots[type][preferrednumber] = true
        while robots[type][robots[type].next] do
            robots[type].next = robots[type].next + 1
        end
        return type..preferrednumber
    else
        local number = robots[type].next
        robots[type][number] = true
        while robots[type][robots[type].next] do
            robots[type].next = robots[type].next + 1
        end
        return type..number
    end
end

local me = address(environment.iason)


-- copy world file -----------------------------------------------------------------------------------------------------

local definition = nil
local given = assert(io.open(it, "r"))
definition = given:read("*all")
given:close()
local worldfile = assert(io.open(file("world.lua"), "w"))
io.write("**  Writing ", file("world.lua"), " from ", it, "\n")
worldfile:write(definition)
worldfile:close()


-- generate mapping between different robot names and save to file -----------------------------------------------------

local mapping = assert(io.open(file("crew.lua"), "w"))
io.write("**  Generating ", file("crew.lua"), "\n")
mapping:write("return {\n")

local routes = {}
local robots = {}
local rnames = {}

for name,body in pairs(world) do
    if body.deras and body.deras.robot then
        local r = robot(body.deras.type or "fb", body.deras.robotid)
        local a = address(body.deras.address)
        mapping:write("  [\"", r, "\"]")
        mapping:write(" = {address=\"", a, "\",")
        mapping:write(" name=\"", name, "\"},\n")
        routes[name] = a
        robots[r] = name
        rnames[name] = r
    end
end

mapping:write("}\n")
mapping:close()


-- set up IASON server behavior via hexameter --------------------------------------------------------------------------

local subscriptions = {}

local function publish(event, item)
    local category = subscriptions[event] or {}
    for address,space in pairs(category) do
        if space then
            hexameter.tell("put", address, space, {item})
        end
    end
end

local running = {}
local finished = {}
local online = {}
local destroyed = {}
local time = function ()
    local uniqueid = 1
    local function checkforcompletion()
        local allfinished = true
        for _,_ in pairs(running) do
            allfinished = false
        end
        --print("running", serialize.literal(running))
        if allfinished then
            --hexameter.tell("put", "localhost:55555", "effect.tocks", {{}})
            publish("finished", {finished = true})
        end
    end
    return function(msgtype, author, space, parameter)
        --print("received ", msgtype, space)
        local response = {}
        if space == "motors" then
            for i,item in ipairs(parameter) do
                local available = false
                for _,motor in pairs((world[item.body].deras or {}).externalmotors or {}) do
                    if motor.type == item.type then
                        available = true
                    end
                end
                if available then
                    item.id = uniqueid
                    running[uniqueid] = item
                    table.insert(response, {id = item.id, status = "running"})
                    uniqueid = uniqueid + 1
                    hexameter.tell(msgtype, routes[item.body], "motors", {item})
                    io.write("[IASN] delivered motor action ", item.id, " for ", item.body, " to ", routes[item.body], "\n")
                else
                    table.insert(response, {status = "unavailable"})
                    io.write("[IASN] ignored motor action for ", item.body, " (no argos motor ", item.type, " available)\n")
                end
            end
        end
        if space == "sensors" then
            for i,item in ipairs(parameter) do
                local available = false
                for _,sensor in pairs((world[item.body].deras or {}).externalsensors or {}) do
                    if sensor.type == item.type then
                        available = true
                    end
                end
                if available then
                    local result = hexameter.ask(msgtype, routes[item.body], "sensors", {item})[1]
                    table.insert(response, result)
                    io.write("[IASN] delivered sensor action for ", item.body, " to ", routes[item.body], "\n")
                    --io.write("[IASN] delivered sensor action for ", item.body, " to ", routes[item.body], " with result ", serialize.literal(result), "\n")
                else
                    table.insert(response, {status = "unavailable"})
                    io.write("[IASN] ignored sensor action for ", item.body, " (no argos sensor ", item.type, " available)\n")
                end
            end
        end
        if space == "finished" then
            if msgtype == "get" or msgtype == "qry" then
                for i,item in ipairs(parameter) do
                    if item.id then
                        --io.write("[IASN] check on motor action ", item.id, ": ", running[item.id] and "running" or finished[item.id] and "finished" or "not finished", "\n")
                        if running[item.id] then
                            table.insert(response, {})
                        end
                        if finished[item.id] and (not running[item.id]) then
                            table.insert(response, {id = item.id, value = item.id})
                            --io.write("[IASN] answered to action ", id, " with result ", serialize.literal(finished[item.id]), "\n")
                            if msgtype == "get" then
                                finished[item.id] = nil
                            end
                        end
                    end
                end
            elseif msgtype == "put" then
                for i,item in ipairs(parameter) do
                    if item.id then
                        running[item.id] = nil
                        finished[item.id] = item.value or true
                        checkforcompletion()
                        io.write("[IASN] finished motor action ", item.id, "\n")
                        --print("running", serialize.literal(running))
                    end
                end
            end
        end
        if space == "hades.effect.ticks" then
            for i,item in pairs(parameter) do
                checkforcompletion()
            end
        end
        if space == "robots" then
            for i,item in ipairs(parameter) do
                if item.id then
                    if mgstype == "put" then
                        online[item.id] = item.value or true
                        table.insert(response, {id = item.id, value = online[item.id]})
                    elseif msgtype == "get" then
                        table.insert(response, {id = item.id, value = online[item.id]})
                        online[item.id] = nil
                    elseif msgtype == "qry" then
                        table.insert(response, {id = item.id, value = online[item.id]})
                    end
                end
            end
        end
        if space == "destroyed" or space == "events.robot.destruction" then
            for i,item in ipairs(parameter) do
                if msgtype == "put" then
                    if item.id then
                        io.write("[IASN] Robot ", item.id, " signaled destruction.\n")
                        destroyed[item.id] = true
                        table.insert(response, {id = item.id, value = true})
                        publish("robot.destruction", item)
                    end
                end
            end
        end
        if space == "events.robot.initialization" then
            for i,item in pairs(parameter) do
                if msgtype == "put" then
                    if item.id then
                        table.insert(response, {id = item.id, value = true})
                        publish("robot.initialization", item)
                    end
                end
            end
        end
        if space == "events.robot.reset" then
            for i,item in pairs(parameter) do
                if msgtype == "put" then
                    if item.id then
                        table.insert(response, {id = item.id, value = true})
                        publish("robot.reset", item)
                    end
                end
            end
        end
        if space == "subscriptions" then
            if msgtype == "put" then
                for i,item in ipairs(parameter) do
                    if type(item) == "table" and item.to then
                        subscriptions[item.to] = subscriptions[item.to] or {}
                        subscriptions[item.to][item.name or author] = item.space or "iason.subscription"
                    end
                end
                return parameter
            end
        end
        return response
    end
end

--set up iason's network communication
hexameter.init(me, time, nil, nil, environment.hexameter)
io.write("**  Iason is listening on "..me..(environment.servermode and " in server mode " or "").."\n")


-- create argos controller ---------------------------------------------------------------------------------------------

local script = nil
if environment.controller then
    local given = assert(io.open(there..environment.controller, "r"))
    script = given:read("*all")
    given:close()
else
    local default = assert(io.open(here.."argonaut.lua", "r"))
    script = default:read("*all")
    default:close()
end
if script then
    local controller = assert(io.open(file("controller.lua"), "w"))
    io.write("**  Writing ", file("controller.lua"), "\n")
    controller:write("-- NOTE: This file is auto-generated by IASON on each run. Chances are, any changes\n")
    controller:write("--       made to this file will be lost immediately. To alter the controller program,\n")
    controller:write("--       please edit the basic files according to the IASON documentation.\n\n")
    controller:write("environment = ", serialize.literal(environment), "\n")
    controller:write("here = \"", there..(environment.stage), "\"\n")
    controller:write("iason = \"", hexameter.me(), "\"\n")
    controller:write("package.path = \"", package.path, "\"\n")
    controller:write("\n")
    controller:write("-- END OF HEADER (GENERATED BY IASON)\n\n")
    controller:write(script)
    controller:close()
end


-- create argos world config file --------------------------------------------------------------------------------------

if environment.config then
    local given = assert(io.open(there..environment.config, "r"))
    local config = given:read("*all")
    given:close()
    local configuration = assert(io.open(file("world.argos"), "w"))
    io.write("**  Generating ", file("world.argos"), "\n")
    configuration:write(config)
    configuration:close()
else
    iason.argos = iason.argos or {}
    iason.defaultrobot = iason.defaultrobot or {}
    local scalefactor = iason.worldscale or 1
    local function derivetype(body)
        if type(body.deras.robot) == "string" then
            return body.deras.robot
        end
        if iason.defaultrobot and (type(iason.defaultrobot.robot) == "string") then
            return iason.defaultrobot.robot
        end
        if iason.defaultrobot and (type(iason.defaultrobot.type) == "string") then
            return iason.defaultrobot.type
        end
        return "foot-bot"
    end
    local function deriveposition(body)
        if body.deras.position then
            if (type(body.deras.position) == "table") and body.deras.position.x and body.deras.position.y then
                return body.deras.position.x .. "," .. body.deras.position.y .. ",0"
            end
            if (type(body.deras.position) == "function") then
                local x, y = body.deras.position(body)
                if (type(x) == "table") then
                    x, y = x.x, x.y
                end
                return x .. "," .. y .. ",0"
            end
        end
        if body.state and body.state.position then
            if (type(body.state.position) == "table") and body.state.position.x and body.state.position.y then
                return (body.state.position.x * scalefactor) .. "," .. (body.state.position.y * scalefactor) .. ",0"
            end
        end
        if iason.defaultrobot and iason.defaultrobot.position then
            return iason.defaultrobot.position
        end
        return "0,0,0"
    end
    local function deriveorientation(body)
        if body.deras.orientation then
            if (type(body.deras.orientation) == "table") and body.deras.orientation.x and body.deras.orientation.y then
                return "0," .. body.deras.orientation.y .. "," .. body.deras.orientation.x
            end
            if (type(body.deras.orientation) == "function") then
                local z, y, x = body.deras.orientation(body)
                z = z or 0
                y = y or 0
                x = x or 0
                if (type(z) == "table") then
                    z, y, x = z.z or 0, z.y or 0, z.x or 0
                end
                return z .. "," .. y .. "," .. x
            end
        end
        if body.state and body.state.orientation then
            if (type(body.state.orientation) == "table") and body.state.orientation.x and body.state.orientation.y then
                return "0," .. (body.state.orientation.y) .. "," .. (body.state.orientation.x)
            end
        end
        if iason.defaultrobot and iason.defaultrobot.orientation then
            return iason.defaultrobot.orientation
        end
        return "0,0,0"
    end
    local function derivedevices(dev, body)
        assert((dev == "actuators") or (dev == "sensors"))
        if body.deras.argos[dev] then
            return xml.ensure(dev, body.deras.argos[dev])
        end
        if body.deras[dev] or iason.defaultrobot[dev] then
            local devices = {}
            for name,params in pairs(body.deras[dev] or iason.defaultrobot[dev] ) do
                if type(name) == "string" then
                    if params == true then
                        table.insert(devices, {name, implementation="default"})
                    else
                        table.insert(devices, xml.ensure(name, params))
                    end
                else
                    if type(params[1]) == "string" then
                        table.insert(devices, params)
                    end
                end
            end
            return xml.ensure(dev, devices)
        end
        if dev == "actuators" then
            return {
                "actuators",
                {"differential_steering", implementation="default"},
                {"footbot_gripper", implementation="default"},
                {"footbot_turret", implementation="default"},
                {"leds", implementation="default", medium="leds"},
                {"range_and_bearing", implementation="default"}
            }
        end
        if dev == "sensors" then
            return {
                "sensors",
                {"colored_blob_omnidirectional_camera", implementation="rot_z_only", medium="leds", show_rays="true"},
                {"differential_steering", implementation="default"},
                {"footbot_base_ground", implementation="rot_z_only"},
                {"footbot_light", implementation="rot_z_only", show_rays="false"},
                {"footbot_motor_ground", implementation="rot_z_only"},
                {"footbot_proximity", implementation="default", show_rays="true"},
                {"footbot_turret_encoder", implementation="default"},
                {"range_and_bearing", implementation="medium", medium="rab", show_rays="false"}
            }
        end
        --cannot reach here by assertion
    end
    local controllers = iason.argos.controllers or {}
    local arena = iason.argos.arena or {}
    for name,body in pairs(world) do
        if body.deras and body.deras.robot then
            body.deras.argos = body.deras.argos or {}
            table.insert(arena,
                {derivetype(body), id=rnames[name],
                    xml.ensure("body", body.deras.argos.body)
                        or {"body", position=deriveposition(body), orientation=deriveorientation(body)},
                    xml.ensure("controller", body.deras.argos.controller)
                        or {"controller", config="controller_"..rnames[name]}
                }
            )
            if not body.deras.argos.controller then
                table.insert(controllers,
                    {"lua_controller", id="controller_"..rnames[name],
                        derivedevices("actuators", body),
                        derivedevices("sensors", body),
                        {"params", script=file("controller.lua")}
                    }
                )
            end
        end
    end
    local argosconfig = {"argos-configuration",
        xml.ensure("framework", iason.argos.framework)
            or {
                "framework",
                {"system", threads="8"},
                {"experiment", length="0", ticks_per_second="8", random_seed="127"}
            },
        xml.ensure("controllers", controllers),
        xml.ensure("arena", arena),
        xml.ensure("physics_engines", iason.argos.physics_engines)
            or {
                "physics_engines", 
                {"dynamics2d", id="dyn2d", iterations="50"}
            },
        xml.ensure("media", iason.argos.media)
            or {
                "media",
                {"range_and_bearing", id="rab"},
                {"led", id="leds"}
            },
        xml.ensure("visualization", iason.argos.visualization)
            or {
                "visualization",
                {"qt-opengl", lua_editor="false"}
            }
    }
    local configuration = assert(io.open(file("world.argos"), "w"))
    io.write("**  Generating ", file("world.argos"), "\n")
    --configuration:write(string.gsub(template, "%%[^%%]+%%", configenv))
    configuration:write("<?xml version=\"1.0\" ?>\n")
    configuration:write("<!-- NOTE: This file is auto-generated by IASON on each run. Chances are, any changes\n")
    configuration:write("           made to this file will be lost immediately. To alter the ARGoS coniguration,\n")
    configuration:write("           please edit the basic files according to the IASON documentation. -->\n\n")
    configuration:write(xml.translate(argosconfig))
    configuration:close()
end


-- run IASON main loop -------------------------------------------------------------------------------------------------

if environment.dryrun then
    io.write("**  Iason shut down because \"dry run\" was specified.\n")
    os.exit()
end

--ostools.call("cd", there..(environment.stage), ";", "argos3", "-c", "world.argos")

ostools.call("argos3", "-c", there..(environment.stage).."world.argos", "&")

local anyone = true

while anyone do
    hexameter.respond(0)
    anyone = false
    for robotname,_ in pairs(robots) do
        if not destroyed[robotname] then            
            anyone = true
        end
    end
end

hexameter.converse()

io.write("**  Iason finsihed.\n")