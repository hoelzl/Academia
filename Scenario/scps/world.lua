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
    explanation = explanation.."      plan:      "..(body.state.plan and "<taught>" or "<clueless>").."\n"
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
        return {damages=damages, rewards=rewards, success=rewards-damages, age=me.state.age or "??"}
    end
}

local look = {
    type = "look",
    class = "sensor",
    measure = function (me, world, control)
        local location = tartaros.sisyphos_graph.getnode(me.state.position)
        return location
    end
}

local lookout = {
    type = "lookout",
    class = "sensor",
    measure = function (me, world, control)
        local neighbors = tartaros.sisyphos_graph.getedges(me.state.position)
        return neighbors
    end
}

local listen = {
    type = "listen",
    class = "sensor",
    measure = function (me, world, control)
        return me.state.plan
    end
}

local interview = {
    type = "interview",
    class = "sensor",
    measure = function (me, world, control)
        local memories = {}
        for name,body in pairs(world) do
            memories[name] = body.state.log
        end
        return memories
    end
}

local trust = {
    type = "trust",
    class = "sensor",
    measure = function (me, world, control)
        return me.state.relationships or {}
    end
}



--motor library

local forget = {
    type = "forget",
    class = "motor",
    run = function(me, _, control)
        me.state.plan = nil
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

local teach = {
    type = "teach",
    class = "motor",
    run = function(me, world, control)
        if not control or not control.plan then
            return me
        end
        for name,body in pairs(world) do
            if not control.name or (name == control.name) then -- if name is given, only apply to that body
                if (body.state.position.x == me.state.position.x) and (body.state.position.y == me.state.position.y) then -- only teach to agent on the same node
                    if tartaros.can(world[name], listen) then -- only teach to agents who can listen
                        world[name].state.plan = control.plan
                        world[name].state.planner = me.name
                        world[name].state.damage = (world[name].state.damage or 0) + (world[name].state.currentdamage or 0)
                        world[name].state.reward = (world[name].state.reward or 0) + (world[name].state.currentreward or 0)
                        world[name].state.currentdamage = 0
                        world[name].state.currentreward = 0
                        world[name].state.expecteddamage = control.expecteddamage or 0
                        world[name].state.expectedreward = control.expectedreward or 0
                        world[name].state.log = {} -- are we sure here?
                    end
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
        if not control.to then
            for x,rest in pairs(getedges(me.state.position)) do
                for y,edge in pairs(rest) do
                    control.to = edge.to
                    break
                end
                break
            end
        end
        if not (type(control.to) == "table") then
            control.to = lookup(tostring(control.to))
        end
        if (type(control.to) == "table") and control.to.x and control.to.y then
            local target = getnode(control.to)
            local route = getedge(me.state.position, target)
            if route then
                me.state.position = target
                me.state.currentdamage = (me.state.currentdamage or 0) + (route.cost or 0)
                if control.expectedcost then
                    me.state.log = me.state.log or {}
                    table.insert(me.state.log, {
                        action = "go",
                        planner = me.state.planner,
                        position = me.state.position,
                        target = control.to,
                        expectatedcost = control.expectedcost,
                        cost = route.cost
                    })
                end
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
                   --getnode(me.state.position).objects[control.id] = nil
                   return me
                end
            elseif control.class then
                for id,object in getnode(me.state.position).objects do
                    if object.class == control.class then
                        me.state.carriage = object
                        --getnode(me.state.position).objects[object.id] = nil
                        return me
                    end
                end
            else
                for id,object in getnode(me.state.position).objects do
                    me.state.carriage = object
                    return me
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
            me.state.currentreward = (me.state.currentreward or 0) + (me.state.carriage.reward or 0)
            me.state.position.objects[me.state.carriage.id] = me.state.carriage
            me.state.carriage = nil
            me.state.relationships = me.state.relationships or {}
            table.insert(me.state.relationships, {
                planner = me.state.planner,
                damage = me.state.currentdamage
                reward = me.state.currentreward,
                expectedreward = me.state.expectedreward,
                expecteddamage = me.state.expecteddamage,
            })
            me.state.damage = (me.state.damage or 0) + (me.state.currentdamage or 0)
            me.state.reward = (me.state.reward or 0) + (me.state.currentreward or 0)
            me.state.currentdamage = 0
            me.state.currentreward = 0
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
    sensors = {interview, result},
    motors = {teach, nop},
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
    sensors = {listen, trust, look, lookout},
    motors = {forget, pickup, drop, nop, go},
    state = {
        position = tartaros.sisyphos_graph.getahome(),
        damage = 0,
        reward = 0
    },
    time = function(body, world, clock)
        me.state.age = clock
        body.state.currentdamage = body.state.currentdamage + 1
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
    print = explain
}

for i=2,5 do
    tartaros.clone("math1", "math"..i)
end


metaworld.charon = {
    addresses = "localhost:55555,...,localhost:55585,-localhost:55559",
    doomsday = 30,
    avatar = "observer",
    hexameter = {
        socketcache = 10
    }
}

return world

