require "serialize"
local show = serialize.presentation

local settings = {
    rates = {
        invent = 0.5,
        combine = 0.5,
        praiseshare = 0.1
    }
}

local explain = function (body)
    local explanation = ""
    explanation = explanation.."position:  "..body.state.x..","..body.state.y.."\n"
    explanation = explanation.."      mood:      "..body.state.goal.."\n"
    explanation = explanation.."      target:    "..(body.state.targetx or "-")..","..(body.state.targety or "-").."\n"
    explanation = explanation.."      attention: "..(body.state.attention and "<taught>" or "<clueless>").."\n"
    explanation = explanation.."      carrying:  "..(body.state.carrying or 0).."\n"
    explanation = explanation.."      collected: "..(body.state.collected or 0).."\n"
    return explanation
end

-- static world description library

statics = {}

local function place(object, x, y)
    statics[x] = statics[x] or {}
    statics[x][y] = statics[x][y] or {}
    table.insert(statics[x][y], object)
end

local function placemultiple(object, xs, ys)
    if not (type(xs) == "table") then xs = {xs} end
    if not (type(ys) == "table") then ys = {ys} end
    for _,x in pairs(xs) do
        for _,y in pairs(ys) do
            place(object, x, y)
        end
    end
end

local function range(start, stop, step)
    step = step or 1
    local result = {}
    local i = start
    while i <= stop do
        table.insert(result, i)
        i = i + step
    end
    return result
end

local function thereis(x, y, class)
    if not statics[x] then return false end
    if not statics[x][y] then return false end
    for _, object in pairs(statics[x][y]) do
        if object.class == class then
            return true
        end
    end
    return false
end

local function accessible(x, y)
    if not statics[x] then return true end
    if not statics[x][y] then return true end
    for _, object in pairs(statics[x][y]) do
        if not object.accessible then
            return false
        end
    end
    return true
end

local function space()
    return {
        class = "space",
        accessible = true
    }
end

local function nest()
    return {
        class = "nest",
        accessible = true
    }
end

local function resource()
    return {
        class = "resource",
        accessible = true
    }
end

local function wall()
    return {
        class = "wall",
        accessible = false
    }
end


-- sensor/motor library

local spot = {
    type = "spot",
    class = "sensor",
    measure = function (me, world, control)
        local horizon = control.horizon or 10
        local spotted = {}
        for name,body in pairs(world) do
            --TODO: spotting shouldn't work in squares, but it's a fine approximation for now
            if not (body == me) then
                if (math.abs(body.state.x - me.state.x) <= horizon) or (math.abs(body.state.y - me.state.y) <= horizon) then
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
            goal=me.state.goal,
            features={
                x=me.state.x,
                y=me.state.y,
                targetx=me.state.targetx or 0,
                targety=me.state.targety or 0
            }
        }
    end
}

local look = {
    type = "look",
    class = "sensor",
    measure = function (me, world, control)
        if statics[me.state.x] and statics[me.state.x][me.state.y] then
            return statics[me.state.x][me.state.y]
        else
            return {}
        end
    end
}

local move = {
    type = "move",
    class = "motor",
    run = function(me, _, control)
        local newx = me.state.x
        local newy = me.state.y
        if control.dir then
            local trans = {n="up", e="right", s="down", w="left"}
            if trans[control.dir] then
                control[trans[control.dir]] = 1
            end
        end
        if control.up then
            newy = me.state.y - 1
        end
        if control.down then
            newy = me.state.y + 1
        end
        if control.right then
            newx = me.state.x + 1
        end
        if control.left then
            newx = me.state.x - 1
        end
        --print("$$$$  ",me.name, me.state.x,me.state.y,"-->",newx,newy)
        if accessible(newx, newy) then
            me.state.x = newx
            me.state.y = newy
        end
        if thereis(me.state.x, me.state.y, "resource") then
            me.state.targetx = 0
            me.state.targety = 0
            if (not me.state.carrying) or (me.state.carrying == 0) then
                me.state.carrying = 1
            end
        end
        if thereis(me.state.x, me.state.y, "nest") then
            me.state.targetx = 2
            me.state.targety = 2
            me.state.collected = (me.state.collected or 0) + (me.state.carrying or 0)
            me.state.carrying = 0
        end
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
            for _,name in pairs(sensor(me, "spot")) do
                if can(world[name], listen) then
                    world[name].state.attention = control.content
                end
            end
        end
        return me
    end
}

local strive = {
    type = "strive",
    class = "motor",
    run = function(me, _, control)
        me.state.goal = control.goal or me.state.goal
    end
}

local forget = {
    type = "forget",
    class = "motor",
    run = function(me, _, control)
        me.state.attention = nil
        return me
    end
}

local brainstorm = {
    type = "brainstorm",
    class = "motor",
    run = function(me, _, control)
        me.state.attention = control.content
        return me
    end
}

local procrastinate = {
    type = "procrastinate",
    class = "motor",
    run = function (me, world, control)
        return me
    end
}


-- static world configuration

place(nest(),      0,  0)
place(resource(),  2,  2)
place(resource(), -5, -7)
placemultiple(wall(), range(-1, 7),           -1)
placemultiple(wall(),           -1, range(-1, 7))
placemultiple(wall(), range(-1, 7),            8)
placemultiple(wall(),            8, range(-1, 7))


-- dynamic world configuration

world = {
    observ = {
        name = "observ",
        sensors = {},
        motors = {move},
        state = {
            x = 0,
            y = 0
        },
        tocked = 10
    },
    platon = {
        name = "platon",
        sensors = {spot, guts, look},
        motors = {move, shout, procrastinate, strive},
        state = {
            x = 1,
            y = 1,
            goal = "live"
        }
    },
    math1 = {
        name = "math1",
        sensors = {spot, listen, guts, look},
        motors = {move, forget, procrastinate, strive, brainstorm},
        state = {
            x = 0,
            y = 1,
            goal = "navigate",
            targetx = 0,
            targety = 0
        },
        print = explain
    },
    math2 = {
        name = "math2",
        sensors = {spot, listen, guts, look},
        motors = {move, forget, procrastinate, strive, brainstorm},
        state = {
            x = 1,
            y = 1,
            goal = "navigate",
            targetx = 0,
            targety = 0
        },
        print = explain
    }
}

return world --not necessary at the time of writing this, but could become part of the specification