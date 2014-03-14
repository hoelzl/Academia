require "serialize"
local show = serialize.presentation
local rand = math.random

local function merge(old, new)
    for _,val in ipairs(new) do
        table.insert(old, val)
    end
    return old
end

local function shoutout(path, body)
    local actions = {}
    local concepts = {}
    local conceptual = false
    for i,step in ipairs(path) do
        if step.meta == "concept" then
            conceptual = true
            local newstep = {} --yay, another deepcopy implementation --TODO: MAKE LIBRARY FOR THIS
            for key,val in pairs(step) do
                if not (key == "meta") then
                    newstep[key] = val
                end
            end
            table.insert(concepts, newstep)
        else
            table.insert(actions, step)
        end
    end
    if conceptual then
        table.insert(actions, {body=body, class="motor", type="shout", control={content=concepts}})
    end
    return actions
end

return function(realm, me)
    local plan = {}
    local lastanchor = nil
    local prediction = {}
    local function interpret(plan, body, feeling)
        local best = {}
        local bestappeal = -10000
        local bestanchor = {}
        for p,particle in ipairs(plan) do
            if particle.class == "anchor" then
                local matching = true
                for feature,value in pairs(feeling.features) do
                    if (not particle[feature]) or (not (particle[feature] == value)) then
                        matching = false
                    end
                end
                if matching then
                    if not (lastanchor == particle) and particle.appeal and particle.appeal > bestappeal and rand() > 0.2 then
                        local path = {}
                        local i = p
                        local releasing = false
                        while not releasing do
                            if plan[i] and plan[i].meta == "concept" then
                                table.insert(path, plan[i])
                            else
                                if plan[i].class == "motor" then --TODO: this needs to copy everything, sort out actions later
                                    plan[i].body = body
                                    table.insert(path, plan[i])
                                end
                                if plan[i].class == "feature" then
                                    prediction = plan[i] --too simplistic
                                end
                                if plan[i].class == "release" then
                                    releasing = true
                                end
                            end
                            i = i + 1
                        end
                        best = path
                        bestappeal = particle.appeal
                        bestanchor = particle
                    end
                end
            end
        end
        lastanchor = bestanchor
        return best
    end
    return function(clock, body)
        local newplan = hexameter.ask("qry", realm, "sensors", {{body=body, type="listen"}})[1].value
        if newplan then
            --plan = newplan
            plan = merge(plan, newplan)
            hexameter.tell("put", realm, "motors", {{type="forget", body=body}})
        end
        local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
        if lastanchor then
            for key,val in pairs(prediction or {}) do
                if not (key == "class") and not (key == "meta") then
                    if feeling.features[key] and not (feeling.features[key] == val) then
                        lastanchor.appeal = lastanchor.appeal / 10
                        print("PREDICTION FAILED", key, feeling.features[key], val)
                        print("  changed appeal", lastanchor.appeal)
                    end
                    prediction = {}
                end
            end
        end
        local actions = shoutout(interpret(plan, body, feeling), body)
        print("&&  guts say ", feeling.goal, " with ", show(feeling.features))
        print("&&  so we react by doing ", show(actions))
        hexameter.tell("put", realm, "motors", actions)
    end
end