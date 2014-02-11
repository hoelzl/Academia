require "serialize"
require "tartaros"
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

--import functions
for name,value in pairs(tartaros.sisyphos) do
    _G[name] = value
end
for name,value in pairs(tartaros.tantalos) do
    _G[name] = value
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

local result = {
    type = "result",
    class = "sensor",
    measure = function (me, world, control)
        local carriage = 0
        for name,body in pairs(world) do
            carriage = carriage + (body.state.collected or 0)
        end
        return {carriage=carriage}
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
        sensors = {result},
        motors = {move},
        state = {
            x = 0,
            y = 0
        },
        obolos = {
            results = {"result"},
            mission = {
                carrysomestuff = { --name is optional, can as well be an array
                    type = "result",
                    goal = {carriage=1}
                }
            }
        },
        tocked = auto
    },
    platon = {
        name = "platon",
        sensors = {spot, guts, look},
        motors = {move, shout, procrastinate, strive},
        state = {
            x = 1,
            y = 1,
            goal = "live"
        },
        psyche = function(realm, me)
            local ultimateplan = {
                {type = "procrastinate", control = {}}
            }
            return function(clock, body)
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
        end,
        obolos = {
            psyche = true --"localhost:55558"
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
        psyche = "./mathetesneos.lua", --"./mathetes.lua"
        obolos = {
            psyche = true
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
        psyche = "./mathetesneos.lua", --"./mathetes.lua"
        obolos = {
            psyche = true
        },
        print = explain
    }
}

metaworld = {
    statics = statics, --remember, this is tartaros.sisyphos.statics
    charon = {
        addresses = "localhost:55555,...,localhost:55565,-localhost:55556,-localhost:55559",
        doomsday = 12,
        ferry = {
            {
                name = "Developer's Concern",
                address = "localhost:77777",
                run = function (basepath, address) return "echo \"   \" hello "..basepath.." "..address.." !" end
            },
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
        }
        --avatar = "observ"
    }
}

setmetatable(world, metaworld)

return world