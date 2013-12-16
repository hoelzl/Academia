require "serialize"
local show = serialize.presentation

local ultimateplan = {
    {type = "procrastinate", control = {}}
}


return function(realm, me)
    return function(clock, body)
        local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
        local deliberation = hexameter.ask("put", "localhost:55559", "solve", {{body=body, state=feeling}})[1]
        if deliberation then
            local actions = deliberation.solution or deliberation.answer or deliberation.ANSWER
            if actions == 42 then
                actions = ultimateplan
            end
            for _,action in pairs(actions) do
                hexameter.tell("put", realm, "motors", {{body=body, type=action.type, control=action.control}})
            end
        end
    end
end