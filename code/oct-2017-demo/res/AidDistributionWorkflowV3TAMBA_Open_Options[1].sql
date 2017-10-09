------------------------------------------------------------------
-- MPC based information exchanges for Aid Distribution
-- Step A: Find reachable ports given deadline (and ship name)
-- earliest_arrival() is a pseudo function taking 5 params (2 locations & speed) and returns a time, 
-- rounded up to the next integer
select port.port_id, 
	earliest_arrival(ship.longitude, ship.latitude, port.longitude, port.latitude, ship.maxspeed) as arrival
	INTO reachable_ports
from port, ship
where earliest_arrival(ship.longitude, ship.latitude, port.longitude, port.latitude, ship.maxspeed) <= deadline
  	and ship.name = shipname;

-- Step B: Find feasible ports from reachable ports that can handle the ship draft and cargo
select port.port_id
from reachable_ports, port, ship
where reachable_ports.port_id = port.port_id
	and port.available = true
	and port.harbordepth >= ship.draft 
	and port.offloadcapacity >= ship.cargo
	and ship.name = shipname;

-----------------------------------------------------------------
-- Non-MPC Options:
-- Open Alternate 1: Fully open data exchanges with the IRC

-- Step A - IRC to determine reachable ports per ship

-- First, info released by each provider nation for each ship to IRC
select ship.name, ship.longitude, ship.latitude, ship.maxspeed
from ship;

-- Then, info released by recipient nation about ports to IRC
select port.port_id, port.longitude, port.latitude
from port;

-- From these, IRC determiness reachable ports for each ship
-- i.e.: compute all port_id's where
--       earliest_arrival(ship.longitude, ship.latitude, port.longitude, port.latitude, ship.maxspeed) <= deadline

-- Step B - IRC to determine feasible ports per ship

-- First, info released by each provider nation per ship to IRC
select ship.name, ship.draft, ship.cargo
from ship;

-- Then, info released by recipient nation about ports to IRC
select port.id, port.available, port.harbordepth, port.offloadcapacity
from port;

-- From these IRC determines feasible ports per ship
-- i.e. compute all port_id's where
-- reachable_ports.port_id = port.port_id
--	and port.available = true
--	and port.harbordepth >= ship.draft 
--	and port.offloadcapacity >= ship.cargo

-----------------------------------------------------------------
-- Open Alternate 2: only port information openly shared with IRC
-- Step A - IRC to determine reachable ports per ship

-- First, info released to IRC by recipient nation about ports
-- This is ALL PUBLIC anyway
select port.port_id, port.longitude, port.latitude
into port_location
from port;

-- From these, IRC asks each ship if the port is reachable, providing the port_location data
-- Only reachability of each port_id is returned to IRC, ship data is not released to IRC
select port.port_id
into reachable_ports
from ship, port_location
where earliest_arrival(ship.longitude, ship.latitude, 
	port_location.longitude, port_location.latitude, ship.maxspeed) <= deadline
  	and ship.name = shipname;

-- Step B - IRC to determine feasible ports per ship

-- First, info released to IRC by recipient nation about port capacities
select port.id, port.available, port.harbordepth, port.offloadcapacity
into port_capacity
from port;

-- From these IRC determines feasible ports per ship, providing the port_capacity and reachable_port data
-- Only feasibility of each port_id is returned to IRC, ship data is not released to IRC
select port.port_id
from reachable_ports, port_capacity, ship
where reachable_ports.port_id = port_capacity.port_id
	and port_capacity.available = true
	and port_capacity.harbordepth >= ship.draft 
	and port_capacity.offloadcapacity >= ship.cargo
	and ship.name = shipname;

-----------------------------------------------------------------
-- Open Alternate 3: only ship information openly shared with IRC
-- Step A - IRC to determine reachable ports per ship

-- First, info released to IRC by each provider nation for each ship to IRC
select ship.name, ship.longitude, ship.latitude, ship.maxspeed
into ship_location
from ship;

-- From these, IRC asks each ship if the ship which ports are reachable, providing the ship_location data
-- Only reachability of each port_id is returned to IRC, port data is not released to IRC
select port.port_id
into reachable_ports
from ship_location, port
where earliest_arrival(ship_location.longitude, ship_location.latitude, 
	port.longitude, port.latitude, ship_location.maxspeed) <= deadline
  	and ship_location.name = shipname;

-- Step B - IRC to determine feasible ports per ship

-- First, info released to IRC by provider nation about ship capacities
select ship.name, ship.draft, ship.cargo
into ship_capacity
from ship;

-- From these IRC determines feasible ports per ship, providing the ship_capacity and reachable_port data
-- Only feasibility of each port_id is returned to IRC, port data is not released to IRC
select port.port_id
from reachable_ports, port_capacity, ship
where reachable_ports.port_id = port.port_id
	and port.available = true
	and port.harbordepth >= ship_capacity.draft 
	and port.offloadcapacity >= ship_capacity.cargo
	and ship_capacity.name = shipname;

-----------------------------------------------------------------
