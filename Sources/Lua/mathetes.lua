require "serialize"
local show = serialize.presentation

function utilitycompare(utility1, utility2)
    if not utility1 then
        return true
    end
    if not utility2 then
        return false
    end
    return utility1[1] < utility2[1]
end

function isempty(table)
    for _,_ in pairs(table) do
        return false
    end
    return true
end

function listify(entity)
    if type(entity) == "table" then
        if isempty(entity) or entity[1] then
            return entity
        else
            return {entity}
        end
    else
        return {entity}
    end
end

function append(table, appandage)
    for _,entity in ipairs(appandage) do
        table.insert(table, item)
    end
    return table
end

function ismeta(attribute)
    if attribute == "class" then
        return true
    end
    return false
end

--TODO: implement more syntactic tests and validation
return function(realm, me)
    local plan = {}
    local function interpret(plan, body, feeling, actions)
        actions = actions or {}
        if ismeta(feeling.goal) then
            print("$$  guts sensor returned invalid goal")
            return nil
        end
        if type(plan) == "table" then
            for _,alternative in ipairs(listify(plan)) do
                if alternative.class == "commit goal" then
                    if alternative[feeling.goal] then
                        return interpret(alternative[feeling.goal], body, feeling, actions)
                    end
                elseif alternative.class == "commit feature" then
                    local committed = false
                    for _,branch in pairs(alternative) do
                        if (type(branch) == "table") and branch.on and branch["do"] then
                            local pattern = branch.on
                            local action = branch["do"]
                            if type(pattern) == "table" then
                                local matching = true
                                for feature,value in pairs(pattern) do
                                    if not (feeling.features[feature] == value) then
                                        matching = false
                                    end
                                end
                                if matching then
                                    committed = true
                                    actions = interpret(action, body, feeling, actions)
                                end
                            end
                        end
                    end
                    if committed then
                        return actions
                    elseif alternative.otherwise then
                        return interpret(alternative.otherwise, body, feeling, actions)
                    end
                elseif alternative.class == "commit best" then
                    if alternative.of then
                        actions = interpret(alternative.of, body, feeling, actions)
                        local best = nil
                        for _,action in pairs(actions) do
                            if utilitycompare(best, action) then
                                best = {action}
                            end
                        end
                        return best or {}
                    end
                elseif alternative.class == "motor" then
                    table.insert(actions, {body=body, type=alternative.type, control=alternative.control, utility=alternative.utility})
                end
            end
            return actions
        else
            print("$$  tried to act out syntactically invalid plan")
            return nil
        end
    end
    return function(clock, body)
        local newplan = hexameter.ask("qry", realm, "sensors", {{body=body, type="listen"}})[1].value
        if newplan then
            plan = newplan
        end
        local feeling = hexameter.ask("qry", realm, "sensors", {{body=body, type="guts"}})[1].value
        print("&&  guts say ", feeling.goal, " with ", show(feeling.features))
        hexameter.tell("put", realm, "motors", interpret(plan, body, feeling))
    end
end