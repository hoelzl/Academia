X = {type = "type", contents = "contents", tabsize = 2}

function X.convertall(elements, indent)
    indent = indent or 0
    if not (type(elements) == "table") then
        error("malformed XML elements")
    end
    local result = ""
    for _,element in ipairs(elements) do
        result = result .. X.convert(element, indent)
    end
    return result
end

function X.convert(element, indent)
    indent = indent or 0
    local result = string.rep(" ", X.tabsize * indent)
    if X.isprimitive(element) then
        result = result .. X.convertprimitive(element) .. "\n"
    else
        if not (type(element) == "table" and type(element[X.type]) == "string") then
            error("malformed XML element")
        end
        result = result .. "<" .. element[X.type]
        for name,value in pairs(element) do
            if not ((name == X.type) or (name == X.contents)) then
                if type(name) == "string" then
                    if X.isprimitive(value) then
                        result = result .. " " .. name .. "=\"" .. X.convertprimitive(value) .. "\""
                    end
                end
            end
        end
        if element[X.contents] then
            result = result .. ">\n" .. X.convertall(element[X.contents], indent + 1)
            result = result .. string.rep(" ", X.tabsize * indent) .. "</" .. element[X.type] .. ">\n"
        else
            result = result .. " />\n"
        end
    end
    return result
end

function X.isprimitive(thing)
    return (type(thing) == "string") or (type(thing) == "number") or (type(thing) == "boolean")
end

function X.convertprimitive(primitive)
    return tostring(primitive)
end

function X.generate(element)
    if X.isprimitive(element) then
        return element
    else
        local newelement = {}
        local elemtype = nil
        local contents = {}
        local empty = true
        for name,value in pairs(element) do
            if type(name) == "string" then
                newelement[name] = value
            end
        end
        for index,value in ipairs(element) do
            if index == 1 then
                elemtype = value
            else
                empty = false
                contents[index-1] = X.generate(value)
            end
        end
        newelement[X.type] = elemtype
        newelement[X.contents] = (not empty) and contents
        return newelement
    end
end

function X.generateall(elements)
    if not (type(elements) == "table") then
        error("malformed XML elements")
    end
    local result = {}
    for e,element in ipairs(elements) do
        result[e] = X.generate(element)
    end
    return result
end

function X.translate(element, indent)
    return X.convert(X.generate(element), indent)
end

function X.translateall(elements, indent)
    return X.convertall(X.generateall(elements), indent)
end

function X.ensure(elemtype, element)
    if not (type(element) == "table") then
        return element
    end
    if not (element[1] == elemtype) then
        table.insert(element, 1, elemtype)
    end
    return element
end

function X.test()
    local test1 = X.convertall({
            {type = "html", contents = {
                {type = "head", contents = {
                    {type = "title", contents = {
                        "Test"
                    }}
                }},
                {type = "body", contents = {
                    {type = "a", href = "http://ascens-ist.eu", contents = {
                        "click here"
                    }}
                }}
            }}
        })
    local test2 = X.convert(X.generate(
            {"html",
                {"head",
                    {"title",
                        "Test"
                    }
                },
                {"body",
                    {"a", href = "http://ascens-ist.eu",
                        "click here"
                    }
                }
            }
        ))
    return test1 .. "\n\n" .. test2
end

--print(X.test())


return X