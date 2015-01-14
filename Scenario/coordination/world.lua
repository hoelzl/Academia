local tartaros = require "tartaros"
local world, metaworld = tartaros.create()
tartaros.load("sisyphos_graph")
tartaros.load("tantalos", true)
local json = require "dkjson"

local getnode = tartaros.sisyphos_graph.getnode
local getedge = tartaros.sisyphos_graph.getedge
local getedges = tartaros.sisyphos_graph.getedges
local ishome = tartaros.sisyphos_graph.ishome
local allhomes = tartaros.sisyphos_graph.allhomes
local label = tartaros.sisyphos_graph.label
local lookup = tartaros.sisyphos_graph.lookup

--static world setup

local graphfile = assert(io.open("./model-02.json", "r"))
local graphdefinition = json.decode(graphfile:read("*all"))
local function fix(tab)
    for key,val in pairs(tab) do
        if type(val) == "table" then
            fix(val)
        end
        if type(val) == "number" then
            tab[key] = math.floor(val)
        end
    end
end
fix(graphdefinition)
tartaros.sisyphos_graph._process(graphdefinition)
graphfile:close()





--pretty print utility

local function explain(body)
    local explanation = ""
    if body.state.position then
        explanation = explanation .. "position:  " .. body.state.position.x .. "," .. body.state.position.y .. "\n"
    else
        explanation = explanation .. "position:  ?\n"
    end
    explanation = explanation.."      mood:      "..(body.state.goal or "<none>").."\n"
    explanation = explanation.."      attention: "..(body.state.attention and "<taught>" or "<clueless>").."\n"
    explanation = explanation.."      damage:    "..(body.state.damage or 0).."\n"
    explanation = explanation.."      reward:    "..(body.state.reward or 0).."\n"
    explanation = explanation.."      cargo:     "..(body.state.carriage and body.state.carriage.id or "<none>").."\n"
    return explanation
end


--sensor library

local result = {
    type = "result",
    class = "sensor",
    measure = function (me, world, control)
        local damages = 0
        local rewards = 0
        for name,body in pairs(world) do
            damages = damages + (body.state.damage or 0)
            rewards = rewards + (body.state.reward or 0)
        end
        for _,home in pairs(allhomes()) do
            for id,object in pairs(home.objects) do
                rewards = rewards + (object.reward or 0)
            end
        end
        return {damages=damages, rewards=rewards, success=rewards-damages, age=me.state.age or "??"}
    end
}

local spot = {
    type = "spot",
    class = "sensor",
    measure = function (me, world, control)
        --local horizon = control.horizon or 10
        local spotted = {}
        for name,body in pairs(world) do
            if not (body == me) then
                if getedge(me.state.position, body.state.position) then
                    table.insert(spotted, name)
                end
            end
        end
        return spotted
    end
}

local listen = {
    type = "listen",
    class = "sensor",
    measure = function (me, world, control)
        return me.state.attention
    end
}

local guts = {
    type = "guts",
    class = "sensor",
    measure = function (me, world, control)
        return {
            goal = me.state.goal or "live",
            features = {
                --position = me.state.position,
                x = me.state.position.x,
                y = me.state.position.y,
                ontarget = ishome(me.state.position.x, me.state.position.y),
                cargo = me.state.carriage and me.state.carriage.id or "nil"
            }
        }
    end
}


--motor library

local forget = {
    type = "forget",
    class = "motor",
    run = function(me, _, control)
        me.state.attention = nil
        return me
    end
}

local nop = {
    type = "nop",
    class = "motor",
    run = function (me, world, control)
        me.state.damage = (me.state.damage or 0) + (me.state.position.cost or 0)
        return me
    end
}

local shout = {
    type = "shout",
    class = "motor",
    run = function(me, world, control)
        if not control or not control.content then
            return me
        end
        if control.name and world[control.name] then
            world[control.name].state.attention = control.content
        else
            for name,body in pairs(world) do
                if tartaros.can(world[name], listen) then
                    world[name].state.attention = control.content
                end
            end
        end
        return me
    end
}

local function pickone(set)
    for key,val in pairs(set) do
        return val
    end
end

local go = {
    type = "go",
    class = "motor",
    run = function(me, world, control)
        if not control.to then
            control.to = pickone(pickone(getedges(me.state.position))).to
        end
        if not (type(control.to) == "table") then
            control.to = lookup(tostring(control.to))
        end
        if (type(control.to) == "table") and control.to.x and control.to.y then
            local target = getnode(control.to)
            local route = getedge(me.state.position, target)
            if route then
                me.state.position = target
                me.state.damage = (me.state.damage or 0) + (route.cost or 0)
            end
        end
        return me
    end
}

local pickup = { --TODO: cost of living applies here, too (and possibly for other "go" as well)
    type = "pickup",
    class = "motor",
    run = function(me, world, control)
        if not me.state.carriage then
            if control.id then
               if getnode(me.state.position).objects[control.id] then
                   me.state.carriage = getnode(me.state.position).objects[control.id]
                   getnode(me.state.position).objects[control.id] = nil
                   return me
                end
            end
            if control.class then
                for id,object in getnode(me.state.position).objects do
                    if object.class == control.class then
                        me.state.carriage = object
                        getnode(me.state.position).objects[object.id] = nil
                        return me
                    end
                end
            end
        end
        return me
    end
}

local drop = {
    type = "drop",
    class = "motor",
    run = function(me, world, control)
        if me.state.carriage then
            me.state.position.objects[me.state.carriage.id] = me.state.carriage
            me.state.carriage = nil
        end
        return me
    end
}


--world definition

world.observer = {
    name = "observer",
    sensors = {},
    motors = {},
    state = {},
    print = function() return "" end
}

world.platon = {
    name = "platon",
    sensors = {result, spot, guts},
    motors = {shout, nop},
    state = {
        position = tartaros.sisyphos_graph.getahome(),
    },
    psyche = function(realm, me)
        local ultimateplan = {
            {type = "nop", control = {}},
            {type = "go", control = {}}
        }
        return function(clock, body)
            hexameter.tell("put", realm, "motors", {{body=body, type="shout", control={content=
                ultimateplan
            }}})
        end
    end,
    obolos = {
        psyche = true,
        results = {"result"}
    },
    time = function (me, world, clock)
        me.state.age = clock
    end,
    print = explain
}

world.math1 = {
    name = "math1",
    sensors = {listen, guts},
    motors = {shout, forget, pickup, drop, nop, go},
    state = {
        position = tartaros.sisyphos_graph.getahome(),
        damage = 0,
        reward = 0
    },
    time = function(body, world, clock)
        body.state.damage = body.state.damage + 1
    end,
    --psyche = "../../Sources/Lua/mathetesneoteros.lua", --"./mathetes.lua"
    psyche = function(realm, me)
        local current = 1
        return function(clock, body)
            local newplan = hexameter.ask("qry", realm, "sensors", {{body=body, type="listen"}})[1].value
            local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
            if type(newplan) == "table" then
                if not newplan[current] then
                    current = 1
                end
                if newplan[current] then
                    hexameter.tell("put", realm, "motors", {{body=body, type=newplan[current].type, control=newplan[current].control}})
                    current = current + 1
                end
            end
        end
    end,
    obolos = {
        psyche = true
    },
    deras = {
        robot = "foot-bot",
        respond = function(robot, body, msgtype, author, space, parameter)
            if space == "stop" then
                robot.wheels.set_velocity(0,0)
            end
        end,
        externalmotors = {
            {
                type = "go",
                class = "motor",
                run = function(robot, me, world, control)
                    robot.colored_blob_omnidirectional_camera.enable()
                    --print("go from", serialize.literal(me.state.position), "to", serialize.literal(control.to))
                    if not (type(control.to) == "table") then
                        control.to = lookup(tostring(control.to))
                    end
                    if (type(control.to) == "table") and control.to.x and control.to.y then
                        local target = getnode(control.to)
                        local route = getedge(me.state.position, target)
                        --print(target and serialize.literal(target) or "NO TARGET", control.to.x, control.to.y)
                        if route then
                            me.state.togo = {
                                x = metaworld.iason.worldscale * (target.x - me.state.position.x),
                                y = metaworld.iason.worldscale * (target.y - me.state.position.y),
                                turned = {x = false, y = false}
                            }
                            me.state.position = target
                            --print("thus", serialize.literal(me.state.togo))
                        else
                            me.state.togo = {noroute = true}
                        end
                    end                    
                    --robot.wheels.set_velocity(-50, -50)
                    robot.leds.set_single_color(13, "yellow")
                    --io.write("[ROBO] go\n")
                    return me
                end,
                update = function(robot, me, world, control)
                    local standard_velocity = 0.5 -- in m/s
                    local argos_ticks_per_second = 8 --must be in sync with actual setting, IASON currently defaults to 8 --TODO: allow motor to read this from somewhere
                    --io.write("[ROBO] update\n")
                    --io.write("[ROBO] update with ", serialize.literal(me.state.togo), "\n")
                    for _,dimension in ipairs({{name="x", turnaround=-0.25}, {name="y", turnaround=0.25}}) do
                        local d = dimension.name
                        local a = dimension.turnaround
                        if me.state.togo and not me.state.togo.noroute then
                            if not me.state.togo.turned[d] then
                                robot.wheels.set_velocity(a*robot.wheels.axis_length*math.pi*argos_ticks_per_second, -a*robot.wheels.axis_length*math.pi*argos_ticks_per_second)
                                me.state.togo.turned[d] = true
                                return true
                            elseif not (math.abs(me.state.togo[d]) < 0.2) then
                                local s = (me.state.togo[d] > 0) and 1 or -1 --cannot be zero because of previous condition
                                local v = s*standard_velocity*100
                                robot.wheels.set_velocity(v, v)
                                me.state.togo[d] = me.state.togo[d] - s*(standard_velocity/argos_ticks_per_second) --distance per second / ticks per second = distance per tick
                                return true
                            end
                        end
                    end
                end,
                expect = function(robot, me, world, control)
                    --io.write("check\n")
                    if me.state.togo.noroute then
                        return true
                    end
                    if me.state.togo.turned.x and me.state.togo.turned.y and (math.abs(me.state.togo.x) < 0.2) and (math.abs(me.state.togo.y) < 0.2) then
                        me.state.togo = nil
                        robot.wheels.set_velocity(0,0)
                        --io.write("true\n")
                        return true
                    end
                    --io.write("false: ", me.state.togo.x, " ", me.state.togo.y, "\n")
                    return false
                end
            }
        },
        actuators = {
            {"differential_steering", implementation="default"},
            {"footbot_gripper", implementation="default"},
            {"footbot_turret", implementation="default"},
            {"leds", implementation="default", medium="leds"},
            {"range_and_bearing", implementation="default"}
        },
        sensors = {
            {"colored_blob_omnidirectional_camera", implementation="rot_z_only", medium="leds", show_rays="true"},
            {"differential_steering", implementation="default"},
            {"footbot_base_ground", implementation="rot_z_only"},
            {"footbot_light", implementation="rot_z_only", show_rays="false"},
            {"footbot_motor_ground", implementation="rot_z_only"},
            {"footbot_proximity", implementation="default", show_rays="true"},
            {"footbot_turret_encoder", implementation="default"},
            {"range_and_bearing", implementation="medium", medium="rab", show_rays="false"}
        }
    },
    print = explain
}
--tartaros.tantalos.motormirror(world.math1, "localhost:55655")
tartaros.clone("math1", "math2")
tartaros.clone("math1", "math3")
tartaros.clone("math1", "math4")
tartaros.clone("math1", "math5")


metaworld.charon = {
    addresses = "localhost:55555,...,localhost:55565,-localhost:55559",
    doomsday = 30,
    avatar = "observer",
    hexameter = {
        socketcache = 10
    }
}

metaworld.iason = {
    hexameter = {
        socketcache = 10
    },
    worldscale = 0.003,
    argos = {
        arena = {
            size = "12,12,12",
            center = "0,0,0",
            positional_grid_size = "10,10,10",
            {"floor", id="floor", source="loop_functions", pixels_per_meter="100"},
            {"light", id="light_0", position="5.0,-5.0, 2", orientation="0,0,0", color="red", intensity="10.0", medium="leds"},
            {"light", id="light_1", position="5.0, 5.0, 2", orientation="0,0,0", color="green", intensity="10.0", medium="leds"},
            {"light", id="light_2", position="-5.0,5.0, 2", orientation="0,0,0", color="blue", intensity="10.0", medium="leds"},
        },
        media = {
            {"range_and_bearing", id="rab"},
            {"led", id="leds"}
        }
    }
}

return world

