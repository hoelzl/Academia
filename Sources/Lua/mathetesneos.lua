require "serialize"
local show = serialize.presentation
local rand = math.random

return function(realm, me)
    local plan = {}
    local function interpret(plan, body, feeling)
        local best = {}
        local bestappeal = -10000
        for p,particle in ipairs(plan) do
            if particle.class == "anchor" then
                local matching = true
                for feature,value in pairs(feeling.features) do
                    if (not particle[feature]) or (not (particle[feature] == value)) then
                        matching = false
                    end
                end
                if matching then
                    if particle.appeal and particle.appeal > bestappeal then
                        local path = {}
                        local i = p
                        local releasing = false
                        while not releasing do
                            if plan[i].class == "motor" then --TODO: this needs to copy everything, sort out actions later
                                plan[i].body = body
                                table.insert(path, plan[i])
                            end
                            if plan[i].class == "release" then
                                releasing = true
                            end
                            i = i + 1
                        end
                        best = path
                        bestappeal = particle.appeal
                    end
                end
            end
        end
        return best
    end
    return function(clock, body)
        local newplan = hexameter.ask("qry", realm, "sensors", {{body=body, type="listen"}})[1].value
        if newplan then
            plan = newplan
        end
        local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
        local actions = interpret(plan, body, feeling)
        print("&&  guts say ", feeling.goal, " with ", show(feeling.features))
        print("&&  so we react by doing ", show(actions))
        hexameter.tell("put", realm, "motors", actions)
    end
end