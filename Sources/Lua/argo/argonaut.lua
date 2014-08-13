--NOTE: This script is meant to be used with Iason, don't run as stand-alone!

local hexameter = require "hexameter"
local tartaros  = require "tartaros"

local mapping = dofile(here.."./crew.lua")
local next = {}
local expectations = {}

tartaros.setup(environment.tartaros)
world = dofile(here.."./world.lua")
metaworld = getmetatable(world or {})

function init()
    body = mapping[robot.id] and world[mapping[robot.id].name] or nil
    me = mapping[robot.id] and mapping[robot.id].address or nil
    if me then
        io.write("[CTRL] Robot ", robot.id, " running as ", body and body.name or "???", " @ ", me, "\n")
        local time = function()
            return function (msgtype, author, space, parameter)
                --print("[HEXA] Received", parameter, "@", space)
                local response = {}
                if space == "gogogo" then
                    robot.wheels.set_velocity(5,5)
                end
                if space == "sensors" then
                    if body and body.deras and body.deras.sensors then
                        for i,item in ipairs(parameter) do
                            for _,sensor in pairs(body.deras.sensors) do
                                if item.type and (sensor.type == item.type) then
                                    table.insert(response, {
                                        body=item.body,
                                        type=sensor.type,
                                        value=sensor.measure(robot, body, world, item.control or {})})
                                end
                            end
                        end
                    end
                end
                if space == "motors" then
                    if body and body.deras and body.deras.motors then
                        for i,item in ipairs(parameter) do
                            for _,motor in pairs(body.deras.motors) do
                                if item.type and (motor.type == item.type) then
                                    table.insert(next, function (robot)
                                        motor.run(robot, body, world, item.control or {})
                                        if motor.expect then
                                            table.insert(expectations, {
                                                condition = function (robot)
                                                    return motor.expect(robot, body, world, item.control or {})
                                                end,
                                                update = function (robot)
                                                    if motor.update then
                                                        motor.update(robot, body, world, item.control or {})
                                                    end
                                                end,
                                                consequence = function (robot)
                                                    io.write("[ROBO] ", robot.id, " finished action ", item.id, "\n")
                                                    hexameter.tell("put", iason, "finished", {{id = item.id}})
                                                end
                                            })
                                        else
                                            hexameter.tell("put", iason, "finished", {{id = item.id}})
                                        end
                                    end)
                                end
                            end
                        end
                    end
                end
                if body and body.deras and body.deras.respond then
                    return body.deras.respond(robot, body, msgtype, author, space, parameter)
                end
                return response
            end
        end
        hexameter.init(me, time)
    end
    if body and body.deras and body.deras.init then
        body.deras.init(robot, body)
    end
    if me then
        hexameter.tell("put", iason, "events.robot.initialization", {{id = robot.id}})
    end
end

function step()
    for a,action in ipairs(next) do
        action(robot)
    end
    for e,expectation in ipairs(expectations) do
        if expectation.condition(robot) then
            expectation.consequence(robot)
            expectations[e] = nil
        else
            expectation.update(robot)
        end
    end
    next = {}
    if me then
        hexameter.respond(4)
    end
    if body and body.deras and body.deras.step then
        body.deras.step(robot, body)
    end
end

function reset()
    if body and body.deras and body.deras.reset then
        body.deras.reset(robot, body)
    end
    if me then
        hexameter.tell("put", iason, "events.robot.reset", {{id = robot.id}})
    end
end

function destroy()
    if body and body.deras and body.deras.destroy then
        body.deras.destroy(robot, body)
    end
    if me then
        hexameter.tell("put", iason, "destroyed", {{id = robot.id, value = true}})
        hexameter.converse(2)
        io.write("[CTRL] Robot ", robot.id, " shutting down\n")
        hexameter.term()
    end
end