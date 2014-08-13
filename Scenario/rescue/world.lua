local tartaros = require "tartaros"
local world, metaworld = tartaros.create()
tartaros.load("sisyphos_graph")
tartaros.load("tantalos", true)

local makenode = tartaros.sisyphos_graph.makenode
local removenode = tartaros.sisyphos_graph.removenode
local makeedge = tartaros.sisyphos_graph.makeedge
local removeedge = tartaros.sisyphos_graph.removeedge
local makeobject = tartaros.sisyphos_graph.makeobject
local makehome = tartaros.sisyphos_graph.makehome
local getnode = tartaros.sisyphos_graph.getnode
local getedge = tartaros.sisyphos_graph.getedge
local getedges = tartaros.sisyphos_graph.getedges
local ishome = tartaros.sisyphos_graph.ishome
local allhomes = tartaros.sisyphos_graph.allhomes
local label = tartaros.sisyphos_graph.label
local lookup = tartaros.sisyphos_graph.lookup

tartaros.publish("edition", tartaros.sisyphos_graph.edition)
tartaros.publish("stuff", tartaros.sisyphos_graph.stuff)
tartaros.publish("makenode", tartaros.sisyphos_graph._makenode)
tartaros.publish("removenode", tartaros.sisyphos_graph._removenode)
tartaros.publish("makeedge", tartaros.sisyphos_graph._makeedge)
tartaros.publish("removeedge", tartaros.sisyphos_graph._removeedge)
tartaros.publish("makeobject", tartaros.sisyphos_graph._makeedge)
tartaros.publish("makehome", tartaros.sisyphos_graph._makehome)

--static world setup

if true then
    tartaros.sisyphos_graph._process(dofile("./model-01.lua"))
else
    makenode(0, 0); label(0, 0, "1")
    makenode(0, 1); label(0, 1, "2")
    makenode(1, 0); label(1, 0, "4")
    makenode(1, 1); label(1, 1, "3")

    makeedge(getnode(0,0), getnode(0,1), 1)
    makeedge(getnode(0,1), getnode(0,0), 1)

    makeedge(getnode(0,0), getnode(1,0), 1)
    makeedge(getnode(1,0), getnode(0,0), 1)

    makeedge(getnode(0,1), getnode(1,1), 1)
    makeedge(getnode(1,1), getnode(0,1), 1)

    makehome(0, 0)
end

makeobject(0, 1, {class="victim", id="v1", reward=1000})
makeobject(1, 1, {class="rubble", id="r1"})



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
            --for _,name in pairs(tartaros.sensor(me, "spot")) do
                if tartaros.can(world[name], listen) then
                    for i,step in ipairs(control.content) do
                        world[name].state.attention = world[name].state.attention or {}
                        table.insert(world[name].state.attention, step)
                    end
                end
            --end
            end
        end
        return me
    end
}

local go = {
    type = "go",
    class = "motor",
    run = function(me, world, control)
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

world.pro = {
    name = "pro",
    sensors = {},
    motors = {shout, forget, go, pickup, drop, nop},
    state = {
        position = getnode(0,0),
        damage = 0,
        reward = 0
    },
    psyche = function(realm, me)
        return function(clock, body)
            hexameter.tell("put", realm, "motors", {{body=body, type="shout", control={content={
                {class="anchor", x=0, y=0, ontarget="yes", cargo="nil", appeal=9001},
                {class="motor", type="go", control={to="2"}},
                {class="release"},
                {class="anchor", x=0, y=0, ontarget="yes", cargo="v1", appeal=10000},
                {class="motor", type="drop", control={}},
                {class="release"},
                {class="anchor", x=0, y=1, ontarget="no", cargo="nil", appeal=10000},
                {class="motor", type="pickup", control={id="v1"}},
                {class="release"},
            }}}})
        end
        --return function(clock, body)
        --    local action = {}
        --    if clock == 1 then
        --        action = {body=body, type="go", control={to=2}}
        --    elseif clock == 2 then
        --        action = {body=body, type="pickup", control={id="v1"}}
        --    elseif clock == 3 then
        --        action = {body=body, type="go", control={to=1}}
        --    elseif clock == 4 then
        --        action = {body=body, type="drop", control={}}
        --    else
        --        action = {body=body, type="nop", control={}}
        --    end
        --    hexameter.tell("put", realm, "motors", {action})
        --    --hexameter.tell("put", realm, "motors", {{body=body, class="motor", type="shout", control={content=action}}})
        --end
    end,
    obolos = {
        psyche = true
    },
    print = explain
}
--world.pro = nil --we really only want to "go pro" for testing purposes

world.platon = {
    name = "platon",
    sensors = {result, spot, guts},
    motors = {shout, nop},
    state = {
        position = getnode(1,1),
    },
    psyche = function(realm, me)
        local ultimateplan = {
            {type = "nop", control = {}}
        }
        return function(clock, body)
            if clock == 1 then
                local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
                local deliberation = hexameter.ask("put", "localhost:55559", "plan", {{body=body, state=feeling, period=clock}})[1] --s/"plan"/"solve"
                if deliberation then
                    local actions = deliberation.solution or deliberation.answer or deliberation.ANSWER
                    if actions == 42 then
                        actions = ultimateplan
                    end
                    local plantext = ""
                    for _,action in pairs(actions) do
                        plantext = plantext..(plantext == "" and "" or ", ")..action.type
                        hexameter.tell("put", realm, "motors", {{body=body, type=action.type, control=action.control}})
                    end
                    print("&&  Plan says: "..plantext)
                end
            end
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
    motors = {shout, forget, pickup, drop, nop, go}, --tartaros.tantalos.combine({go, tartaros.tantalos.proxy(go, "localhost:55659")}, "go")
    state = {
        position = getnode(0,0),
        damage = 0,
        reward = 0
    },
    time = function(body, world, clock)
        body.state.damage = body.state.damage + 1
    end,
    psyche = "../../Sources/Lua/mathetesneoteros.lua", --"./mathetes.lua"
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
        motors = {
            {
                type = "go",
                class = "motor",
                run = function(robot, me, world, control)
                    robot.colored_blob_omnidirectional_camera.enable()
                    --io.write("[ROBO] axis length: ", robot.wheels.axis_length, "\n")
                    for name,value in ipairs(robot.colored_blob_omnidirectional_camera) do
                        io.write("[ROBO] ", me.name, ": (", value.color.red, ",", value.color.blue, ",", value.color.blue, ") @ ", value.angle, "\n")
                    end
                    --print("go from", serialize.literal(me.state.position), "to", serialize.literal(control))
                    if not (type(control.to) == "table") then
                        control.to = lookup(tostring(control.to))
                    end
                    if (type(control.to) == "table") and control.to.x and control.to.y then
                        local target = getnode(control.to)
                        local route = getedge(me.state.position, target)
                        if route then
                            me.state.togo = {x=target.x - me.state.position.x, y=target.y - me.state.position.y, turned = {x = false, y = false}}
                            me.state.position = target
                            --print("thus", serialize.literal(me.state.togo))
                        else
                            me.state.togo = {noroute = true}
                        end
                    end                    
                    --robot.wheels.set_velocity(-50, -50)
                    robot.leds.set_single_color(13, "red")
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
                                robot.wheels.set_velocity(s*standard_velocity*100, s*standard_velocity*100)
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
tartaros.tantalos.mirror(world.math1, "localhost:55659")
tartaros.clone("math1", "math2")
tartaros.clone("math1", "math3")
tartaros.clone("math1", "math4")

metaworld.charon = {
    addresses = "localhost:55555,...,localhost:55565,-localhost:55559",
    doomsday = 30,
    ferry =
    {
        {
            name = "Platon's Mind",
            address = "localhost:55559",
            run = function (worldpath, address)
                return "sbcl --noinform --end-runtime-options --load "..worldpath.."platonsmindrescue.lisp --non-interactive --end-toplevel-options "..address.." > /dev/null 2> /dev/null"
            end,
            setup = function (recycledp, address)
                --hexameter.tell("put", address, "didaskalos.relearn", {tartaros.sisyphos_graph.produce()})
                hexameter.tell("put", address, "didaskalos.model", {{model=tartaros.sisyphos_graph.produce()}})
            end,
            halt = function (worldpath, address)
                hexameter.tell("put", address, "charon.halt", {{charon="halting"}})
                return "echo > /dev/null"
            end,
            recycle = true --NOTE: You can only recycle Hexameter components!
        }
    },
    avatar = "observer",
    hexameter = {
        socketcache = 10
    }
}

metaworld.iason = {
    hexameter = {
        socketcache = 10
    },
    argos = {
        arena = {
            size = "12,12,12",
            center = "0,0,0",
            positional_grid_size = "10,10,10",
            {"floor", id="floor", source="loop_functions", pixels_per_meter="100"},
            {"light", id="light_0", position="5.0,-5.0, 2", orientation="0,0,0", color="red", intensity="10.0", medium="leds"},
            {"light", id="light_1", position="5.0, 5.0, 2", orientation="0,0,0", color="green", intensity="10.0", medium="leds"},
            {"light", id="light_2", position="-5.0,5.0, 2", orientation="0,0,0", color="blue", intensity="10.0", medium="leds"},
        }
    }
}

return world

