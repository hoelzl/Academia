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
    motors = {shout, forget, go, pickup, drop, nop},
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
    print = explain
}

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

return world

