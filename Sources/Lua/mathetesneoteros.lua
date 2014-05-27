require "serialize"
local show = serialize.presentation
local rand = math.random

local function merge(old, new)
    for _,val in ipairs(new) do
        val.age = nil
        table.insert(old, val)
    end
    return old
end

local function straighten(actions)
    local result = {}
    for i,step in ipairs(actions) do
        if step.class == "motor" then
            table.insert(result, step)
        end
    end
    return result
end

return function(realm, me)
    local plan = {}
    local teaching = 0
    local function interpret(plan, body, feeling)
        local best = {}
        local bestappeal = -10000
        local bestanchor = {}
        local teachfactor = 0
        for p,particle in ipairs(plan) do
            if particle.class == "anchor" then
                local matching = true
                for feature,value in pairs(feeling.features) do
                    if (not particle[feature]) or (not (particle[feature] == value)) then
                        --print(particle[feature] .. " does not match " .. value)
                        matching = false
                    end
                end
                if matching then
                    --print(particle.appeal)
                    if particle.appeal and particle.appeal > bestappeal and rand() > 0.4 then
                        local path = {}
                        local i = p
                        local releasing = false
                        while not releasing do
                            if plan[i].class == "motor" then --TODO: this needs to copy everything, sort out actions later
                                plan[i].body = body
                            end
                            if plan[i].class == "release" then
                                releasing = true
                            end
                            if plan[i].class == "teach" then
                                teachfactor = teachfactor + 1
                            end
                            table.insert(path, plan[i])
                            i = i + 1
                        end
                        best = path
                        bestappeal = particle.appeal
                        bestanchor = particle
                    end
                end
            end
        end
        return best, bestanchor, teachfactor
    end
    return function(clock, body)
        local newplan = hexameter.ask("qry", realm, "sensors", {{body=body, type="listen"}})[1].value
        if newplan then
            plan = merge(plan, newplan)
            hexameter.tell("put", realm, "motors", {{type="forget", body=body}})
        end
        local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
        local actions, anchor, teachfactor = interpret(plan, body, feeling)
        if teaching > 0 then
            teaching = teaching - 1
            print("&&  guts say ", feeling.goal, " with ", show(feeling.features))
            print("&&  so we react by teaching ", show(actions))
            hexameter.tell("put", realm, "motors", {{body=body, type="shout", control={content=actions}}})
        else
            teaching = teaching + teachfactor
            print("&&  guts say ", feeling.goal, " with ", show(feeling.features))
            print("&&  so we react by doing ", show(straighten(actions)))
            print("&&  because of appeal "..(anchor.appeal or ""))
            hexameter.tell("put", realm, "motors", straighten(actions))
            for i,step in ipairs(plan) do
                step.age = step.age and step.age + 1 or 1
                if step.age > 10 and (step.class == "motor" or step.class == "teach") then
                    table.remove(plan, i)
                end
            end
        end
    end
end