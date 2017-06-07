-- Aid Distribution workflow - 3 step version

CREATE extension IF NOT EXISTS cube;
CREATE extension IF NOT EXISTS earthdistance;

-- Manual inputs 
\set deadline 12
--\set shipname '''Mercy'''
\set shipname '''Comfort'''

DROP TABLE IF EXISTS reachable_ports, feasible_ports, available_slots, slot_assignments;

--
-- Step A: Find reachable ports given deadline (and ship name)
-- Earliest arrival from ship location to port location (rounded up)
create or replace function earliest_arrival(
  ship_location POINT,
  port_location POINT,
  max_speed BIGINT)
  returns BIGINT as
$$
  select ceil((ship_location <@> port_location) / max_speed)::BIGINT
$$
language SQL IMMUTABLE returns NULL on NULL INPUT;

-- find all ports that can be reached by the deadline by named ship
-- saving eariest arrival time for reachable ports for use in step C
create or replace function compute_reachable_ports(deadline BIGINT, shipname TEXT)
  returns TABLE (port_id BIGINT, arrival BIGINT) as 
$$
  select port.port_id, earliest_arrival(POINT(ship.longitude, ship.latitude),
    POINT(port.longitude, port.latitude), ship.maxspeed) as arrival
  from port, ship
  where earliest_arrival(POINT(ship.longitude, ship.latitude),
  	POINT(port.longitude, port.latitude), ship.maxspeed) <= deadline
  	and ship.name = shipname;
$$
language SQL;

SELECT * INTO reachable_ports
FROM compute_reachable_ports(:deadline, :shipname);

\echo 'StepA: Reachable ports by ship' :shipname 'by deadline=' :deadline
SELECT rport.port_id, port.name, earliest_arrival(POINT(ship.longitude, ship.latitude),
    POINT(port.longitude, port.latitude), ship.maxspeed) AS arrival
FROM reachable_ports AS rport, port, ship
WHERE port.port_id = rport.port_id
  AND ship.name = :shipname
ORDER BY arrival;

--
-- Step B: Find feasible ports from reachable ports that can handle the ship draft and cargo
--
create or replace function compute_feasible_ports(shipname TEXT)
  returns TABLE (port_id BIGINT) as
$$
  select port.port_id
  from reachable_ports, port, ship
  where reachable_ports.port_id = port.port_id
    and port.available = true
    and port.harbordepth >= ship.draft 
    and port.offloadcapacity >= ship.cargo
    and ship.name = shipname;
$$
language SQL;

select * into feasible_ports
from compute_feasible_ports(:shipname);

\echo 'Step B: Feasible & reachable ports capable of handling ship' :shipname
SELECT port.port_id, port.name
FROM feasible_ports, port
WHERE feasible_ports.port_id = port.port_id;

--
-- Step C: Find berths in feasible ports that can accommodate ship arrival and length
--
-- function to create a table of open slots (gaps) between reserved slots in table slot
create or replace function compute_available_slots()
  returns TABLE (port_id BIGINT, berth_id BIGINT, slot_id BIGINT, slotstart BIGINT, slotend BIGINT) as
$$
  WITH slot1 AS
  (SELECT port_id, berth_id, row_number() OVER (PARTITION BY port_id, berth_id) AS row_id, slotstart, slotend
  FROM slot
  ORDER BY port_id, berth_id, slotstart)
-- Select open gaps between reserved slots by JOINing above table with itself offset by one slot
-- FULL join used to get inital and end gaps as well (assume 0 - 30 hour timeframe)
  SELECT COALESCE(slot1.port_id, slot2.port_id) AS port_id,
  COALESCE(slot1.berth_id, slot2.berth_id) AS berth_id, 
  row_number() OVER (PARTITION BY COALESCE(slot1.port_id, slot2.port_id),
    COALESCE(slot1.berth_id, slot2.berth_id)) AS gap_id,
  COALESCE(slot1.slotend, 0) AS gapstart, 
  COALESCE(slot2.slotstart, 30) AS gapend
  FROM slot1
  FULL JOIN slot1 AS slot2 ON
  slot1.port_id = slot2.port_id AND
  slot1.berth_id = slot2.berth_id AND
  slot1.row_id + 1 = slot2.row_id 
  WHERE COALESCE(slot1.slotend, 0) < COALESCE(slot2.slotstart, 30)
  ORDER BY port_id, berth_id, COALESCE(slot1.slotend, slot2.slotstart);
$$
language SQL;

-- make temp table of open slots
SELECT * into available_slots
FROM compute_available_slots();

-- debug output
--SELECT * FROM available_slots;

-- Find berths in feasible ports that can accommodate ship arrival and length 
create or replace function assign_slots(deadline BIGINT, shipname TEXT)
  returns TABLE (port_id BIGINT, berth_id BIGINT, slot_id BIGINT, offloadstart BIGINT) as
$$
  SELECT port.port_id, availslot.berth_id, availslot.slot_id,
    GREATEST(rport.arrival, availslot.slotstart) AS offloadstart
  FROM reachable_ports AS rport, feasible_ports AS fport, port, 
    available_slots AS availslot, berth, ship
  WHERE port.port_id = fport.port_id
    AND port.port_id = rport.port_id
    AND port.port_Id = berth.port_id 
    AND availslot.port_id = berth.port_id
    AND availslot.berth_id = berth.berth_id
    AND ship.name = shipname 
    -- ship fits in berth
    AND berth.berthlength >= ship.length
    -- begin offload by deadline
    AND rport.arrival <= deadline AND availslot.slotstart <= deadline
    -- arrival AND available slot start + offloadtime before end of slot window
    AND rport.arrival + port.offloadtime <= availslot.slotend
    AND availslot.slotstart + port.offloadtime <= availslot.slotend
  ORDER BY GREATEST(rport.arrival, availslot.slotstart), port_id, berth_id;
$$
language SQL; 

SELECT * into slot_assignments
FROM assign_slots(:deadline, :shipname);

-- debug output
--SELECT * FROM slot_assignments;

\echo 'Step C: Ship' :shipname 'can reach & fit the following ports/berths by deadline =' :deadline
SELECT assign.port_id, port.name, assign.berth_id, rport.arrival, assign.offloadstart, port.offloadtime
FROM slot_assignments AS assign, available_slots as availslot, reachable_ports as rport, port
WHERE port.port_id = assign.port_id
  AND assign.port_id = rport.port_id
  AND assign.port_id = availslot.port_id
  AND availslot.berth_id = assign.berth_id
  AND availslot.slot_id = assign.slot_id
ORDER BY GREATEST(rport.arrival, availslot.slotstart), berth_id
-- Uncomment to get first opportunity only
--LIMIT 1
;
