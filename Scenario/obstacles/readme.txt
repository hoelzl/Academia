This so-called "scenario" is a direct adaption of the ARGoS example script
found at http://iridia.ulb.ac.be/~cpinciroli/extra/h-414/ under the 
heading Exercise/obstacle_avoidance.

To run this scenario, execute the following commands in separate shells
(assuming you already cd'd into this directory right here):

1.  Execute:
    lua ../../Sources/Lua/argo/iason.lua ./obstacles.lua ./tmp/

2.  An ARGoS window shall open.
    Click on the "run" button in the upper toolbar.

3.  Execute in a separate shell:
    lua ../../Sources/Lua/hades/charon.lua ./obstacles.lua
    
Both IASON and CHARON are configured to run indefinitely at the moment.
Thus, CHARON needs to be killed manually via Ctrl-C! However, you can start
CHARON with the parameter --doomsday 42, which will cause it to shut down
after 42 time steps. The robots will only be able to go straightforward
from then on.
IASON should enter its shutdown sequence once you close the ARGoS window.
That can be quite a lengthy process, though. I won't judge you for Ctrl-C'ing
it as well! (Also, don't forget that when killing IASON, ARGoS tends to 
hang up, so as you're manually killing stuff anyway, ...)
