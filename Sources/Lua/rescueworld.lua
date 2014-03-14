local tartaros = require "tartaros"
local world, metaworld = tartaros.create(environment)
tartaros.load("sisyphos", true)
tartaros.load("tantalos", true)


--world graph library

local graph = {nodes={}, edges={}}

local function makenode(x, y, cost, objects)
    local newnode = {x=x, y=y, cost=cost or 0, objects=objects or {}}
    graph.nodes[x] = graph.nodes[x] or {}
    graph.nodes[x][y] = newnode
    return newnode
end

local function getnode(x, y)
    if type(x) == "table" then
        x = x.x
        y = x.y
    end
    if graph.nodes[x] then
        return graph.nodes[x][y]
    end
    return nil
end

local function getedges(x, y)
    if type(x) == "table" then
        x = x.x
        y = x.y
    end
    if graph.edges[x] then
        return graph.edges[x][y] or {}
    end
    return {}
end

local function getedge(from, to)
    if not from or not to or not from.x or not from.y or not to.x or not to.y then
        return false
    end
    if graph.edges[from.x] and graph.edges[from.x][from.y] and graph.edges[from.x][from.y][to.x] then
        return graph.edges[from.x][from.y][to.x][to.y]
    end
    return nil
end

local function makeedge(from, to, cost)
    if not from.x or not from.y or not to.x or not to.y then
        return false
    end
    local newedge = {from=from, to=to, cost=cost or 0}
    graph.edges[from.x] = graph.edges[from.x] or {}
    graph.edges[from.x][from.y] = graph.edges[from.x][from.y] or {}
    graph.edges[from.x][from.y][to.x] = graph.edges[from.x][from.y][to.x] or {}
    graph.edges[from.x][from.y][to.x][to.y] = newedge
    return newedge
end

local counters = {}

local function makeobject(x, y, object)
    if not x or not y or not graph.nodes[x] or not graph.nodes[x][y] then
        return false
    end
    if not graph.nodes[x][y].objects then
        graph.nodes[x][y].objects = {}
    end
    if not object.class then
        object.class = "object"
    end
    if not object.id then
        counters[object.class] = counters[object.class] and (counters[object.class] + 1) or 1
        object.id = object.class.."-"..counters[object.class]
    end
    graph.nodes[x][y].objects[object.id] = object
end


--static world setup

makenode(0, 0)
makenode(1, 1)
makeedge(getnode(0,0), getnode(1,1), 1)


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
        for id,object in pairs(getnode(0,0).objects or {}) do
            rewards = rewards + (object.reward or 0)
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
                position = me.state.position
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

local procrastinate = {
    type = "procrastinate",
    class = "motor",
    run = function (me, world, control)
        me.state.damage = (me.state.damage or 0) + (me.state.position.cost or 0)
        return me
    end
}

local shout = {
    type = "shout",
    class = "motor",
    run = function(me, _, control)
        if control.name and world[control.name] then
            world[control.name].state.attention = control.content
        else
            for _,name in pairs(tartaros.sensor(me, "spot")) do
                if tartaros.can(world[name], listen) then
                    world[name].state.attention = control.content
                end
            end
        end
        return me
    end
}

local go = {
    type = "go",
    class = "motor",
    run = function(me, world, control)
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
}

world.platon = {
    name = "platon",
    sensors = {result, spot, guts},
    motors = {shout, procrastinate},
    state = {
        position = getnode(1,1) --TODO: this needs ot match an actual node in the world
    },
    psyche = function(realm, me)
        local ultimateplan = {
            {type = "procrastinate", control = {}}
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
    motors = {forget, go, pickup, drop, procrastinate},
    state = {
        position = getnode(0,0),
        damage = 0,
        reward = 0
    },
    psyche = "./mathetesneos.lua", --"./mathetes.lua"
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
    doomsday = 12,
    ferry =
    {
        {
            name = "Platon's Mind",
            address = "localhost:55559",
            run = function (worldpath, address)
                return "sbcl --noinform --end-runtime-options --load "..worldpath.."../Lisp/platonsmind.lisp --non-interactive --end-toplevel-options "..address.." > /dev/null 2> /dev/null"
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

