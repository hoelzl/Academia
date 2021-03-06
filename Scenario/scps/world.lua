here = string.match(arg[0], "^.*/") or "./"
package.path = here.."../xbt/src/?.lua;"..
    here.."../xbt/src/?/init.lua;"..
    package.path

local tartaros = require "tartaros"
local world, metaworld = tartaros.create()
tartaros.load("sisyphos_graph")
tartaros.load("tantalos", true)

local luatools = require "luatools"
local ser = require "serialize"
local xbt = require "xbt"
local util = require "xbt.util"
local graph = require "xbt.graph"
local path = require "xbt.path"
local nodes = require "example.nodes"

local getnode = tartaros.sisyphos_graph.getnode
local getedge = tartaros.sisyphos_graph.getedge
local getedges = tartaros.sisyphos_graph.getedges
local ishome = tartaros.sisyphos_graph.ishome
local allhomes = tartaros.sisyphos_graph.allhomes
local label = tartaros.sisyphos_graph.label
local lookup = tartaros.sisyphos_graph.lookup

local RNG = math.random

local config = {
    worldnodes = 15,
    worldarea = 500,
    students = 5,
    basetrust = 1,
    refractoryperiod = 1,
    costofliving = 1,
    victimlocations = {3, 5, 12},
    victimreward = function (location) return 1000 end,
    victimcooldown = function (victim, rescuer) return victim.reward end,
    catastrophy = {0.1, 0.05, 10, true},
    periodiccatastrophy_check = function (clock) return false end,
    periodiccatastrophy = {0.1, 0.05, 10, true},
    failurecost = 10,
    damageestimation = function (damage, teacher) return damage * 1.5 end,
    rewardestimation = function (reward, teacher) return reward end,
}

local function emptytable(t)
    for k,v in pairs(t) do
        return false
    end
    return true
end


--static world setup

local function initialize_graph (config)
  local g = graph.generate_graph(config.worldnodes, config.worldarea, graph.make_short_edge_generator(1.1))
  g = graph.copy_badly(g, 0.6, 0.25, 0, true)
  print("Navigation graph has " .. #g.nodes .. " nodes and " .. #g.edges .. " edges.")

  tartaros.sisyphos_graph._process(g)
  tartaros.sisyphos_graph.labelallby("id")
  tartaros.sisyphos_graph._makehome(lookup(1))
  for i,victimlocationid in ipairs(config.victimlocations) do
    local victimlocation = lookup(victimlocationid)
    tartaros.sisyphos_graph.makeobject(victimlocation.x, victimlocation.y, {
      class = "victim",
      id = "v"..i,
      reward = config.victimreward(victimlocation)
    })
  end
  return g
end

-- local g = graph.generate_graph(config.worldnodes, config.worldarea, graph.make_short_edge_generator(1.2))

-- g is the graph before the catastrophe happened, i.e., the graph the system designers build based on the
-- building blueprint.  This graph is passed to the teachers as initial value.
local g = initialize_graph(config)
--print("Diameter:        ", graph.diameter(g.nodes))
--local d,n = graph.maxmin_distance(g.nodes)
--print("Maxmin distance: ", d, "for node", n)
--print("Nodes:           ", #g.nodes, "Edges:", #g.edges)

--for _,node in pairs(g.nodes) do
--    node.edges = nil
--end



--pretty print utility

local function explain(body)
    local explanation = ""
    if body.state.position then
        explanation = explanation .. "position:  " .. (body.state.position.id or "?") .. " @ " .. body.state.position.x .. "," .. body.state.position.y .. "\n"
    else
        explanation = explanation .. "position:  ?\n"
    end
    explanation = explanation.."      plan:      "..(body.state.plan and "<taught by "..(body.state.planner or "?").." during "..(body.state.lasttaught or "?")..">" or "<clueless>").."\n"
    explanation = explanation.."      damage:    "..(body.state.damage or 0).." + "..(body.state.currentdamage or 0).." (expecting "..(body.state.expecteddamage or "nothing")..")\n"
    explanation = explanation.."      reward:    "..(body.state.reward or 0).." + "..(body.state.currentreward or 0).." (expecting "..(body.state.expectedreward or "nothing")..")\n"
    local carriagestring
    if body.state.carriage then
        carriagestring = body.state.carriage.id .. " (lastvisited:" .. (body.state.carriage.lastvisited or "?") .. " reward:" .. (body.state.carriage.reward or "?") .. ")"
    end
    explanation = explanation.."      cargo:     "..(body.state.carriage and carriagestring or "<none>").."\n"
    explanation = explanation.."      last act:  "..(body.state.lastaction and body.state.lastaction.type or "<none>").."\n"
    local truststring
    for name,trust in pairs(body.state.trust or {}) do
        truststring = (truststring or "") .. name .. ":" .. trust .. "  "
    end
    explanation = explanation.."      trust:     "..(truststring and truststring or "<none>").."\n"
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
            damages = damages + (body.state.damage or 0) + (body.state.currentdamage or 0)
            rewards = rewards + (body.state.reward or 0) + (body.state.currentreward or 0)
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
        me.state.lastaction = {type="forget", control=control}
        me.state.plan = nil
        return me
    end
}

local nop = {
    type = "nop",
    class = "motor",
    run = function (me, world, control)
        me.state.lastaction = {type="nop", control=control}
        me.state.damage = (me.state.damage or 0) + (me.state.position.cost or 0)
        return me
    end
}

local teach = {
    type = "teach",
    class = "motor",
    run = function(me, world, control)
        me.state.lastaction = {type="teach", control=control}
        if not control or not control.plan then
            return me
        end
        local taught = false
        for name,body in pairs(world) do
            body.state.trust = body.state.trust or {}
            if not control.name or (name == control.name) then -- if name is given, only apply to that body
                if body.state.position and (body.state.position.x == me.state.position.x) and (body.state.position.y == me.state.position.y) then -- only teach to agent on the same node
                    if (body.state.lasttaught or -100) + config.refractoryperiod <= body.state.age then -- do not teach during refractory period
                        if not body.state.carriage then -- do not teach while robot is carrying a victim
                            if tartaros.can(world[name], listen) then -- only teach to agents who can listen
                                if RNG() < (body.state.trust[me.name] or config.basetrust or 1) then -- check teaching success based on trust
                                    world[name].state.plan = control.plan
                                    world[name].state.planner = me.name
                                    world[name].state.damage = (world[name].state.damage or 0) + (world[name].state.currentdamage or 0)
                                    world[name].state.reward = (world[name].state.reward or 0) + (world[name].state.currentreward or 0)
                                    world[name].state.currentdamage = 0
                                    world[name].state.currentreward = 0
                                    world[name].state.expecteddamage = control.expecteddamage or 0
                                    world[name].state.expectedreward = control.expectedreward or 0
                                    world[name].state.log = {}
                                    world[name].state.lasttaught = world[name].state.age
                                end
                            end
                        end
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
        me.state.lastaction = {type="go", control=control}
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
            control.to = lookup(control.to)
        end
        if (type(control.to) == "table") and control.to.x and control.to.y then
            local target = getnode(control.to)
            local route = getedge(me.state.position, target)
            local source = me.state.position
            assert(source)
            if route then
                me.state.position = target
                me.state.currentdamage = (me.state.currentdamage or 0) + (route.cost or 0) - (route.reward or 0)
                me.state.log = me.state.log or {}
                table.insert(me.state.log, {
                        action = "go",
                        result = "success",
                        planner = me.state.planner,
                        position = me.state.position,
                        source = source,
                        target = control.to,
                        expectatedcost = control.expectedcost,
                        cost = route.cost
                    })
            else
              me.state.currentdamage = (me.state.currentdamage or 0) + (config.failurecost or 0)
              me.state.log = me.state.log or {}
              table.insert(me.state.log, {
                action = "go",
                result = "failure",
                planner = me.state.planner,
                position = me.state.position,
                source = source,
                target = control.to,
                expectatedcost = control.expectedcost,
                cost = config.failurecost
              })
            end
        end
        return me
    end
}

local pickup = {
    type = "pickup",
    class = "motor",
    run = function(me, world, control)
        me.state.lastaction = {type="pickup", control=control}
        if not me.state.carriage then
            local victim
            if control.id then
               if getnode(me.state.position).objects[control.id] then
                   victim = getnode(me.state.position).objects[control.id]
                end
            elseif control.class then
                for id,object in pairs(getnode(me.state.position).objects) do
                    if object.class == control.class then
                        victim = object
                        break
                    end
                end
            else
                for id,object in pairs(getnode(me.state.position).objects) do
                    victim = object
                    break
                end
            end
            if victim then
                me.state.carriage = luatools.deepcopy(victim)
                me.state.carriage.reward = config.victimcooldown(me.state.carriage, me)
                victim.lastvisited = me.state.age
            end
        end
        return me
    end
}

local drop = {
    type = "drop",
    class = "motor",
    run = function(me, world, control)
        me.state.lastaction = {type="drop", control=control}
        if me.state.carriage then
            me.state.currentreward = (me.state.currentreward or 0) + (me.state.carriage.reward or 0)
            me.state.carriage = nil
            me.state.relationships = me.state.relationships or {}
            table.insert(me.state.relationships, {
                planner = me.state.planner,
                damage = me.state.currentdamage,
                reward = me.state.currentreward,
                expectedreward = me.state.expectedreward,
                expecteddamage = me.state.expecteddamage,
            })
            me.state.trust = me.state.trust or {}
            me.state.trust[me.state.planner] = ((me.state.currentreward+1)/(me.state.expectedreward+1)) * ((me.state.expecteddamage+1)/(me.state.currentdamage+1)) * (me.state.trust[me.state.planner] or config.basetrust)
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
    time = function (me, world, clock)
        if false then
            if clock == 1 then
          g = graph.copy_badly(g, unpack(config.catastrophy))
          tartaros.sisyphos_graph.delete()
          tartaros.sisyphos_graph._process(g)
--            for e,edge in ipairs(g.edges) do
--                g.edges[e] = config.edgedestruction(edge)
--            end
        elseif config.periodiccatastrophy and config.periodiccatastrophy_check
               and config.periodiccatastrophy_check(clock) then
          g = graph.copy_badly(g, unpack(config.periodiccatastrophy))
          tartaros.sisyphos_graph.delete()
          tartaros.sisyphos_graph._process(g)
        end
    end
    end,
    print = function() return "" end    
}

world.platon = {
    name = "platon",
    sensors = {interview, result},
    motors = {teach, nop},
    state = {
        position = tartaros.sisyphos_graph.getahome(),
    },
    psyche = function(realm, me, body)
        local myplan = graph.copy(g)
        return function(clock, body)
            --TODO: compute expectations!
            local distances, successors, navigationtable
            if clock == 1 then
                distances, successors = graph.floyd(myplan)
                navigationtable = {dist=distances, succ=successors}
            else
                local logs = hexameter.ask("qry", realm, "sensors", {{body=body, type="interview"}})[1].value
                local samples = {}
                for student,log in pairs(logs) do
                    for e,entry in ipairs(log) do
                        table.insert(samples, {from_id = entry.source.id, to_id = entry.target.id, reward = -entry.cost})
                    end
                end
                local update_ratio = graph.update_edge_rewards(myplan, samples)
                distances, successors = graph.floyd(myplan)
                navigationtable = {dist=distances, succ=successors}
                --print(ser.literal(successors))
            end
            --alternative: 
            -- expectedreward = victim.reward for average victim
            -- expecteddamage = 2 * average distance to victim
            local bestroute, bestreward
            for i,victimlocationid in ipairs(config.victimlocations) do
              local victimlocation = lookup(victimlocationid)
              for _,victim in pairs(victimlocation.objects) do
                local routecost = -distances[1][victimlocation.id] or 10000000000
                local effectivereward = victim.reward - routecost
                if effectivereward > (bestreward or -1000000000) then
                  bestreward = victim.reward
                  bestroute = routecost
                end
              end
            end
            bestreward = config.rewardestimation(bestreward, "platon")
            bestroute = config.damageestimation(bestroute, "platon")
            hexameter.tell("put", realm, "motors", {{body=body, type="teach", control={plan=navigationtable, expectedreward=bestreward, expecteddamage=bestroute}}})
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

world.sophistes = {
    name = "sophistes",
    sensors = {interview, result},
    motors = {teach, nop},
    state = {
        position = tartaros.sisyphos_graph.getahome(),
    },
    psyche = function(realm, me)
        local distances,successors = graph.floyd(g)
        local navigationtable = {dist=distances, succ=successors}
        return function(clock, body)
            hexameter.tell("put", realm, "motors", {{body=body, type="teach", control={plan=navigationtable, expectedreward=100000, expecteddamage=0}}})
        end
    end,
    obolos = {
        psyche = true
    },
    time = function (me, world, clock)
        me.state.age = clock
    end,
    print = explain
}


local function is_carrying_victim (node, path, state)
  --print("Is carrying victim!")
  return state.carrying
end
xbt.define_function_name("is_carrying_victim", is_carrying_victim)

local function is_at_home_node (node, path, state)
  --print("Is at home node!"); io.flush()
  return ishome(state.location.x, state.location.y) == "yes"
end
xbt.define_function_name("is_at_home_node", is_at_home_node)

local function has_located_victim (node, path, state)
  return state.location.objects and not emptytable(state.location.objects)
end
xbt.define_function_name("has_located_victim", has_located_victim)

local function can_pick_up_victim (node, path, state)
  local res = not state.carrying 
  return res
end
xbt.define_function_name("can_pick_up_victim", can_pick_up_victim)

local function pick_up_victim (node, path, state)
  hexameter.tell("put", state.realm, "motors", {{body=state.body, type="pickup"}})
  state.carrying = true
  return xbt.succeeded(10, 0)
end
xbt.define_function_name("pick_up_victim", pick_up_victim)

local function pick_home_location (node, path, state)
  state.target_node = tartaros.sisyphos_graph.getahome()
end
xbt.define_function_name("pick_home_location", pick_home_location)

local function pick_victim_location (node, path, state)
  local besttarget, bestreward
  if not state.plan then return xbt.failed(0, "No plan available.") end
  for i,victimlocationid in ipairs(config.victimlocations) do
    local victimlocation = lookup(victimlocationid)
    for _,victim in pairs(victimlocation.objects) do
      print(ser.literal(victimlocation))
      local routecost = -state.plan.dist[state.location.id][victimlocation.id] or 10000000000
      --print(ser.literal(plan))                            
      local effectivereward = victim.reward - routecost
      if effectivereward > (bestreward or -1000000000) then
        bestreward = effectivereward
        besttarget = victimlocation
      end
    end
  end
  state.target_node = besttarget
  return xbt.succeeded(0, 0)
end
xbt.define_function_name("pick_victim_location", pick_victim_location)

local function drop_off_victim (node, path, state)
  hexameter.tell("put", state.realm, "motors", {{body=state.body, type="drop"}})
  state.carrying = false
  return xbt.succeeded(10, 0)
end
xbt.define_function_name("drop_off_victim", drop_off_victim)

local function update_robot_data (node, path, state)
  --print("Update robot data!"); io.flush()
  state.plan = hexameter.ask("qry", state.realm, "sensors", {{body=state.body, type="listen"}})[1].value
  return 0
end
xbt.define_function_name("update_robot_data", update_robot_data)

local function move_towards_chosen_location (node, path, state)
  local succ = state.plan.succ[state.location.id][state.target_node.id]
  hexameter.tell("put", state.realm, "motors", {{body=state.body, type="go", control={to=succ}}})
  return 20
end
xbt.define_function_name("move_towards_chosen_location", move_towards_chosen_location)

local robot_xbt = xbt.choice({
  xbt.when("is_at_home_node",
    xbt.seq({
      xbt.action("update_robot_data"),
      xbt.when("is_carrying_victim", xbt.fun("drop_off_victim"))})),
  xbt.when("has_located_victim",
    xbt.when("can_pick_up_victim", xbt.fun("pick_up_victim"))),
  xbt.when("is_carrying_victim",
    xbt.seq({xbt.action("pick_home_location"),
             xbt.action("move_towards_chosen_location")})),
  xbt.seq({xbt.fun("pick_victim_location"),
           xbt.action("move_towards_chosen_location")})
})

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
        body.state.age = clock
        body.state.currentdamage = (body.state.currentdamage or 0) + (config.costofliving or 0)
    end,
    --psyche = "../../Sources/Lua/mathetesneoteros.lua", --"./mathetes.lua"
    psyche = function(realm, me, body)
        local xbt_state = xbt.make_state({
          body=body,
          carrying=false,
          reward=0,
          realm=realm,
          value=0
        })
        local initialized = false

        return function(clock, body)
          if not initialized then
            hexameter.tell("put", realm, "motors", {{body=body, type="nop"}})
            initialized = true
          end

          xbt_state.location = hexameter.ask("qry", realm, "sensors", {{body=body, type="look"}})[1].value
          local path = path.new()
          local result = xbt.tick(robot_xbt, path, xbt_state)
          xbt_state.reward = xbt_state.reward + result.reward
          xbt_state.value = xbt_state.value + (result.value or 0)
          -- Reset the node, but don't clear its data
          xbt.reset_node(robot_xbt, path, xbt_state, false)
            --[[--
            if not carrying and location.objects and not emptytable(location.objects) then
                hexameter.tell("put", realm, "motors", {{body=body, type="pickup"}})
                carrying = true
            elseif carrying and (ishome(location.x, location.y) == "yes") then
                hexameter.tell("put", realm, "motors", {{body=body, type="drop"}})
                carrying = false
            elseif plan then
                if carrying then
                    local home = tartaros.sisyphos_graph.getahome()
                    local succ = plan.succ[location.id][home.id]
                    hexameter.tell("put", realm, "motors", {{body=body, type="go", control={to=succ}}})
                else
                    local besttarget, bestreward
                    for i,victimlocationid in ipairs(config.victimlocations) do
                        local victimlocation = lookup(victimlocationid)
                        for _,victim in pairs(victimlocation.objects) do
                            --print(ser.literal(location), ser.literal(victimlocation))
                            local routecost = plan.dist[location.id][victimlocation.id] or 10000000000
                            --print(ser.literal(plan))                            
                            local effectivereward = victim.reward - routecost
                            if effectivereward > (bestreward or -1000000000) then
                                bestreward = effectivereward
                                besttarget = victimlocation
                            end
                        end
                    end
                    if besttarget then
                        local succ = plan.succ[location.id][besttarget.id]
                        hexameter.tell("put", realm, "motors", {{body=body, type="go", control={to=succ}}})
                    end
                end
            else
                hexameter.tell("put", realm, "motors", {{body=body, type="nop"}})
            end
            --]]--
        end
    end,
    obolos = {
        psyche = true
    },
    print = explain
}

for i=2,config.students do
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

