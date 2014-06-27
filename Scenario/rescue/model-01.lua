return {
    nodes = {
        {x=0, y=0, id="1"},
        {x=0, y=1, id="2"},
        {x=1, y=0, id="4"},
        {x=1, y=1, id="3"},
    },
    edges = {
        {from={x=0, y=0}, to={x=0, y=1}, cost=1}, {to={x=0, y=0}, from={x=0, y=1}, cost=1},
        {from={x=0, y=0}, to={x=1, y=0}, cost=1}, {to={x=0, y=0}, from={x=1, y=0}, cost=1},
        {from={x=0, y=1}, to={x=1, y=1}, cost=1}, {to={x=0, y=1}, from={x=1, y=1}, cost=1},
    },
    homes = {
        {x=0, y=0}
    },
}