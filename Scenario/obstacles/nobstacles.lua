local tartaros = require "tartaros"
local serialize = require "serialize"
local world, metaworld = tartaros.create()
tartaros.load("tantalos", true)

math.randomseed(1337)

-- dummy sensors and motors for HADES (example is meant to be run on ARGoS, IASON will handle the actual scenario)

local proximity = {
    type = "proximity",
    class = "sensor",
    measure = function (me, world, control)
        local proxtable = {}
        for i=1,100 do
            proxtable[i] = {value=0.42}
        end
        return proxtable
    end
}

local setvelocity = {
    type = "setvelocity",
    class = "motor",
    run = function(me, world, control)
        return me
    end
}

--world definition

world.observer = {
    name = "observer",
    sensors = {},
    motors = {},
    state = {},
    print = function() return "" end
}

world.math1 = {
    name = "math1",
    sensors = {proximity},
    motors = {setvelocity},
    state = {},
    psyche = function(realm, me)
        local SPEED = 5
        return function(clock, body)
            --NOTE: the code of this function is a direct adaptation of the ARGoS example
            -- Search for the reading with the highest value
        	value = -1 -- highest value found so far
        	idx = -1   -- index of the highest value
            local prox = hexameter.ask("get", realm, "sensors", {{body=body, type="proximity"}})[1].value
        	for i=1,24 do
        		if value < prox[i].value then
        			idx = i
        			value = prox[i].value
        		end
            end
        	-- At this point, 'value' contains the highest proximity reading found
         	-- and 'idx' contains the index of that sensor
        	if value == 0 then
        		-- The robot has no obstacles around, just go straight
        		--robot.wheels.set_velocity(SPEED, SPEED)
                hexameter.tell("put", realm, "motors", {{body=body, type="setvelocity", control={left=SPEED, right=SPEED}}})
        	else
        		-- The robot has obstacles, set wheels according to 'idx'
        		if idx <= 12 then
        			-- The closest obstacle is between 0 and 180 degrees: soft turn towards the right
        			--robot.wheels.set_velocity(SPEED, (idx-1) * SPEED / 11)
                    hexameter.tell("put", realm, "motors", {{body=body, type="setvelocity", control={left=SPEED, right=math.ceil((idx-1) * SPEED / 11)}}})
                    
        		else
        			-- The closest obstacle is between 180 and 360 degrees: soft turn towards the left
        			--robot.wheels.set_velocity((24-idx) * SPEED / 11, SPEED)
                    hexameter.tell("put", realm, "motors", {{body=body, type="setvelocity", control={left=math.ceil((24-idx) * SPEED / 11), right=SPEED}}})
                    
        		end
        	end
        end
    end,
    obolos = {
        --psyche = true,
    },
    deras = {
        robot = "foot-bot",
        externalmotors = {
            {
                type = "setvelocity",
                class = "motor",
                run = function(robot, me, world, control)
                    --print("[ROBO] ", robot.id, "setting velocity ", control.left, control.right)
                    robot.wheels.set_velocity(control.left, control.right)
                    return me
                end
            }
        },
        externalsensors = {
            {
                type = "proximity",
                class = "sensor",
                measure = function(robot, me, world, control)
                    --print("[ROBO] ", robot.id, "measuring proximity ")
                    return robot.proximity
                end
            }
        },
        actuators = {
            {"differential_steering", implementation="default"},
            {"leds", implementation="default", medium="leds"},
        },
        sensors = {
            {"colored_blob_omnidirectional_camera", implementation="rot_z_only", medium="leds", show_rays="true"},
            {"differential_steering", implementation="default"},
            {"footbot_proximity", implementation="default", show_rays="true"},
        },
        position = function() return 4*math.random()-2, 4*math.random()-2 end,
        orientation = function() return math.random(0, 360) end
    }
}
tartaros.tantalos.project(world.math1, "localhost:55655")
tartaros.clone("math1", "math2")
tartaros.clone("math1", "math3")
tartaros.clone("math1", "math4")
tartaros.clone("math1", "math5")
tartaros.clone("math1", "math6")
tartaros.clone("math1", "math7")
tartaros.clone("math1", "math8")
tartaros.clone("math1", "math9")
tartaros.clone("math1", "math10")
tartaros.clone("math1", "math11")
tartaros.clone("math1", "math12")



metaworld.charon = {
    addresses = "localhost:55555,...,localhost:55565,-localhost:55559",
    doomsday = 0,
    ferry = {},
    avatar = "observer",
    hexameter = {
        socketcache = 10
    }
}

metaworld.iason = {
    hexameter = {
        socketcache = 10
    },
    argos = {
        arena = {
            size="4, 4, 1",
            center="0,0,0.5",
            [[
                <box id="bn" size="0.1, 4, 0.2" movable="false">
                    <body position="2,0,0" orientation="0,0,0"/>
                </box>
                <box id="bs" size="0.1, 4, 0.2" movable="false">
                    <body position="-2,0,0" orientation="0,0,0"/>
                </box>
                <box id="be" size="4, 0.1, 0.2" movable="false">
                    <body position="0,-2,0" orientation="0,0,0"/>
                </box>
                <box id="bw" size="4, 0.1, 0.2" movable="false">
                    <body position="0,2,0" orientation="0,0,0"/>
                </box>
                <distribute>
                    <position method="uniform" min="-2,-2,0" max="2,2,0"/>
                    <orientation method="uniform" min="0,0,0" max="360,0,0"/>
                    <entity quantity="20" max_trials="100">
                        <box id="o" size="0.2, 0.2, 0.2" movable="false"/>
                    </entity>
                </distribute>
            ]]
        },
        media = {
            {"led", id="leds"}
        },
    }
}

metaworld.argonaut = {
    hexameter = {
        socketcache = 10
    }
}

return world

