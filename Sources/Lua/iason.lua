-- IASON Argos Setup
here = string.match(arg[0], "^.*/") or "./"
package.path = here.."?.lua;"..here.."hades/?.lua;"..here.."hades/lib/?.lua;"..here.."hades/hexameter/?.lua;"..package.path
local hexameter = require "hexameter"
local serialize = require "serialize"
local ostools   = require "ostools"
local tartaros  = require "tartaros"
local show      = serialize.presentation

--using globals here, it, there, world, metaworld, charon

local parameters = ostools.parametrize(arg, {}, function(a,argument,message) print(a, argument, message) end)

if parameters.H or parameters.h or parameters.help then
    io.write("TODO: Create help file. Please understand.")
    io.write("\n")
    os.exit()
end

it= parameters.world and ostools.expand(parameters.world)
    or parameters[1] and ostools.expand(parameters[1])
    or ostools.usrerr("Please pass a world file as a parameter to Charon")


--handy for debugging
--print(show(parameters))
--print(show(environment.addresses))

--load world file
io.write("::  Loading "..it.."...\n")
tartaros.setup(parameters.tartaros or parameters.tar)
world = dofile(it)
metaworld = getmetatable(world or {})
iason = metaworld.iason or {}
there = ostools.dir(it)


--set up environment
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


--copy world file
local definition = nil
local given = assert(io.open(it, "r"))
definition = given:read("*all")
given:close()
local worldfile = assert(io.open(file("world.lua"), "w"))
io.write("**  Writing ", file("world.lua"), " from ", it, "\n")
worldfile:write(definition)
worldfile:close()


--write mapping file
local mapping = assert(io.open(file("argonauts.lua"), "w"))
io.write("**  Generating ", file("argonauts.lua"), "\n")
mapping:write("return {\n")

local routes = {}
local robots = {}

for name,body in pairs(world) do
    if body.deras and body.deras.robot then
        local r = robot(body.deras.type or "fb", body.deras.robotid)
        local a = address(body.deras.address)
        mapping:write("  [\"", r, "\"]")
        mapping:write(" = {address=\"", a, "\",")
        mapping:write(" name=\"", name, "\"},\n")
        routes[name] = a
        robots[r] = name
    end
end

mapping:write("}\n")
mapping:close()



local running = {}
local finished = {}
local online = {}
local destroyed = {}
local time = function ()
    local uniqueid = 1
    return function(msgtype, author, space, parameter)
        --print("received ", msgtype, space)
        local response = {}
        if space == "motors" then
            for i,item in ipairs(parameter) do
                item.id = uniqueid
                running[uniqueid] = item
                table.insert(response, {id = item.id})
                uniqueid = uniqueid + 1
                hexameter.tell(msgtype, routes[item.body], "motors", {item})
                io.write("[IASN] delivered motor action ", item.id, " for ", item.body, " to ", routes[item.body], "\n")
            end
        end
        if space == "finished" then
            if msgtype == "get" or msgtype == "qry" then
                for i,item in ipairs(parameter) do
                    if item.id then
                        io.write("[IASN] check on motor action ", item.id, ": ", running[item.id] and "running" or finished[item.id] and "finished" or "not finished", "\n")
                        if running[item.id] then
                            table.insert(response, {})
                        end
                        if finished[item.id] and (not running[item.id]) then
                            table.insert(response, {id = item.id, value = item.id})
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
                        io.write("[IASN] finished motor action ", item.id, "\n")
                    end
                end
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
        if space == "destroyed" then
            for i,item in ipairs(parameter) do
                if msgtype == "put" then
                    if item.id then
                        io.write("[IASN] Robot ", item.id, " signaled destruction.\n")
                        destroyed[item.id] = true
                        table.insert(response, {id = item.id, value = true})
                    end
                end
            end
        end
        return response
    end
end

--set up iason's network communication
local me = address(environment.iason)
hexameter.init(me, time, nil, nil, environment.hexameter)
io.write("**  Iason is listening on "..me..(environment.servermode and " in server mode " or "").."\n")


--create argos controller

local script = nil
if environment.controller then
    local given = assert(io.open(there..environment.controller, "r"))
    script = given:read("*all")
    given:close()
else
    script = [[
    local hexameter = require "hexameter"
    local tartaros  = require "tartaros"
    
    local mapping = dofile(here.."./argonauts.lua")
    local next = {}
    
    tartaros.setup(]] .. (environment.tartaros and serialize.literal(environment.tartaros) or "") .. [[)
    world = dofile(here.."./world.lua")
    metaworld = getmetatable(world or {})

    function init()
        body = mapping[robot.id] and world[mapping[robot.id].name] or nil
        me = mapping[robot.id] and mapping[robot.id].address or nil
        if me then
            io.write("[CTRL] Robot ", robot.id, " running as ", body and body.name or "???", " @ ", me, "\n")
            local time = function()
                return function (msgtype, author, space, parameter)
                    print("[HEXA] Received", parameter, "@", space)
                    local response = {}
                    if space == "gogogo" then
                        robot.wheels.set_velocity(5,5)
                    end
                    if space == "sensors" then
                        if body and body.deras and body.deras.sensors then
                            for i,item in ipairs(parameter) do
                                for _,sensor in pairs(body.deras.sensors) do
                                    if item.type and (sensor.type == item.type) then
                                        table.insert(response, {
                                            body=item.body,
                                            type=sensor.type,
                                            value=sensor.measure(robot, body, world, item.control or {})})
                                    end
                                end
                            end
                        end
                    end
                    if space == "motors" then
                        if body and body.deras and body.deras.motors then
                            for i,item in ipairs(parameter) do
                                for _,motor in pairs(body.deras.motors) do
                                    if item.type and (motor.type == item.type) then
                                        table.insert(next, function (robot)
                                            motor.run(robot, body, world, item.control or {})
                                            hexameter.tell("put", "]] .. hexameter.me() .. [[", "finished", {{id = item.id}})
                                        end)
                                    end
                                end
                            end
                        end
                    end
                    if body and body.deras and body.deras.respond then
                        return body.deras.respond(robot, body, msgtype, author, space, parameter)
                    end
                    return response
                end
            end
            hexameter.init(me, time)
        end
        if body and body.deras and body.deras.init then
            body.deras.init(robot, body)
        end
    end

    function step()
        for a,action in ipairs(next) do
            action(robot)
        end
        next = {}
        if me then
            hexameter.respond(4)
        end
        if body and body.deras and body.deras.step then
            body.deras.step(robot, body)
        end
    end

    function reset()
        if body and body.deras and body.deras.reset then
            body.deras.reset(robot, body)
        end
    end

    function destroy()
        if body and body.deras and body.deras.destroy then
            body.deras.destroy(robot, body)
        end
        if me then
            hexameter.tell("put", "]] .. hexameter.me() .. [[", "destroyed", {{id = robot.id, value = true}})
            hexameter.converse(2)
            io.write("[CTRL] Robot ", robot.id, " shutting down\n")
            hexameter.term()
        end
    end
    ]]
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


--create argos world file (TODO: implement Lua instantiation interface, not just hard-copying)

local configuration = assert(io.open(file("world.argos"), "w"))
io.write("**  Generating ", file("world.argos"), "\n")

configuration:write([[<?xml version="1.0" ?>
<argos-configuration>

  <!-- ************************* -->
  <!-- * General configuration * -->
  <!-- ************************* -->
  <framework>
    <!-- To make ARGoS faster, you can set 'threads' to some number greater than 0 -->
    <system threads="8" />
    <!-- To change the random seed for each experiment repetition, you can set 'random_seed' to whatever value you like -->
    <experiment length="0" ticks_per_second="8" random_seed="124" />
  </framework>

  <!-- ****************** -->
  <!-- * Loop functions * -->
  <!-- ****************** -->
  <!--<loop_functions label="wall_construction" output="output.txt" />-->

  <!-- *************** -->
  <!-- * Controllers * -->
  <!-- *************** -->
  <controllers>
    <!-- You can tweak some parameters of the sensors and actuators.
         For instance, you can set 'show_rays' to "true" to some of the sensors
         to see the rays cast by the device, thus making debugging easier.
         By default, rays are never shown. Dealing with rays makes ARGoS a
         little slower, so, if you don't need the rays, switch them off.
         For more information, type:
         $ argos -h
    -->
    <lua_controller id="controller">
      <actuators>
        <differential_steering implementation="default" />
        <footbot_gripper implementation="default" />
        <footbot_turret implementation="default" />
        <leds implementation="default" medium="leds" />
        <range_and_bearing implementation="default" />
      </actuators>
      <sensors>
        <colored_blob_omnidirectional_camera implementation="rot_z_only" medium="leds" show_rays="true" />
        <differential_steering implementation="default" />
        <footbot_base_ground implementation="rot_z_only" />
        <footbot_light implementation="rot_z_only" show_rays="false" />
        <footbot_motor_ground implementation="rot_z_only" />
        <footbot_proximity implementation="default" show_rays="true" />
        <footbot_turret_encoder implementation="default" />
        <range_and_bearing implementation="medium" medium="rab" show_rays="false" />
      </sensors>
      <!-- If you want to execute ARGoS with an empty script, use this syntax -->
      <!-- params / -->
      <!-- If you want to execute ARGoS with a script preloaded, use this syntax
           Note: the preloaded script does not appear in the Lua editor! (that's a TODO) -->
      <params script="]] .. file("controller.lua") .. [[" />
    </lua_controller>
  </controllers>

  <!-- *********************** -->
  <!-- * Arena configuration * -->
  <!-- *********************** -->
  <!-- Note: rotations are specified and performed in ZYX order -->
  <arena size="6, 4, 3" center="3, 0, 1.5" positional_grid_size="6,4,1">

    <!-- Floor -->
    <floor id="floor" source="loop_functions" pixels_per_meter="100" />

    <!-- Walls -->
    <box id="wall_north" size="0.2,3.8,1" movable="false">
      <body position="5.9,0,0" orientation="0,0,0" />
    </box>
    <box id="wall_south" size="0.2,3.8,1" movable="false">
      <body position="0.1,0,0" orientation="0,0,0" />
    </box>
    <box id="wall_west" size="6,0.2,1" movable="false">
      <body position="3,1.9,0" orientation="0,0,0" />
    </box>
    <box id="wall_east" size="6,0.2,1" movable="false">
      <body position="3,-1.9,0" orientation="0,0,0" />
    </box>

    <!-- Lights -->
    <light id="light_0" position="5.9,  1.6, 2" orientation="0,0,0" color="yellow" intensity="10.0" medium="leds" />
    <light id="light_1" position="5.9,  0.8, 2" orientation="0,0,0" color="yellow" intensity="10.0" medium="leds" />
    <light id="light_2" position="5.9,  0.0, 2" orientation="0,0,0" color="yellow" intensity="10.0" medium="leds" />
    <light id="light_3" position="5.9, -0.8, 2" orientation="0,0,0" color="yellow" intensity="10.0" medium="leds" />
    <light id="light_4" position="5.9, -1.6, 2" orientation="0,0,0" color="yellow" intensity="10.0" medium="leds" />

    <!-- Foot-bots -->
    <distribute>
      <position method="uniform" min="1.5,-2,0" max="3,2,0" />
      <orientation method="uniform" min="0,0,0" max="360,0,0" />
      <!-- You can play with the number of foot-bots changing the 'quantity' attribute -->
      <entity quantity="4" max_trials="10" base_num="1">
        <!-- You can play with the range of the range and bearing system changing the 'rab_range' attribute
             NOTE: here it is expressed in meters, while in the controller you'll work in cm! -->
        <foot-bot id="fb" rab_range="1.5">
          <controller config="controller" /> 
        </foot-bot>
      </entity>
    </distribute>

    <!-- Objects -->
    <distribute>
      <position method="uniform" min="0,-2,0" max="1.5,2,0" />
      <orientation method="constant" values="0,0,0" />
      <!-- You can play with the number of objects changing the 'quantity' attribute -->
      <entity quantity="25" max_trials="10" base_num="1">
        <cylinder id="cyl" radius="0.1" height="0.15" movable="true" mass="0.1">
          <leds medium="leds">
            <led position="0,0,0.16" color="red" />
          </leds>
        </cylinder>
      </entity>
    </distribute>
  </arena>

  <!-- ******************* -->
  <!-- * Physics engines * -->
  <!-- ******************* -->
  <physics_engines>
    <dynamics2d id="dyn2d" iterations="50" />
  </physics_engines>

  <!-- ********* -->
  <!-- * Media * -->
  <!-- ********* -->
  <media>
    <range_and_bearing id="rab" />
    <led id="leds" />
  </media>

  <!-- ****************** -->
  <!-- * Visualization * -->
  <!-- ****************** -->
  <visualization>
    <!-- To execute ARGoS in batch without visualization, just comment out the 'qt-opengl' section -->
    <qt-opengl lua_editor="false">
      <camera>
        <placement idx="0" position="3.07214,0.0342039,5.49783" look_at="3.07214,0.0342039,4.49783" lens_focal_length="20" />
        <placement idx="1" position="-0.0908117,0,2.42426" look_at="0.616295,0,1.71716" lens_focal_length="20" />
        <placement idx="2" position="6.17136,-0.0281868,2.94765" look_at="5.47854,-0.0292903,2.22655" lens_focal_length="20" />
      </camera>
    </qt-opengl>
  </visualization>

</argos-configuration>]])
configuration:close()

--
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