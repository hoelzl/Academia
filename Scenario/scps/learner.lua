package.path = (string.match(arg[0], "^.*/") or "./").."../../Sources/Lua/hades/hexameter/?.lua;"..package.path
require "hexameter"

-- ...

local myaddress = "localhost:77777"
local mybehavior = function ()
    return function (msgtype, author, space, parameter)
        local response = {}
        if space == "echo" then
            for i,item in ipairs(parameter) do
                table.insert(response, item)
            end
        end
        if (space == "live, universe and everything") or (space == "lue") then
            table.insert(response, {answer=42})
        end
        -- ...
        return response
    end
end

print("::  Starting learner, please exit with Ctrl-C.")

hexameter.init(myaddress, mybehavior)

-- ...

while true do
    hexameter.respond(0)
end
