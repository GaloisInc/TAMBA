--
-- SQL for declaration of relations needed for the April 2017 PRIME CRT aid distribution use case.
-- Author: Tim Ellis, SRI
--
-- This SQL file is runnable (repeatedly) via
--
-- $ psql < ./AidDistributionVx.sql
--

BEGIN;

-- delete all TABLEs if they exist, so we start from scratch
DROP TABLE IF EXISTS dbversion CASCADE;
DROP TABLE IF EXISTS ship CASCADE;
DROP TABLE IF EXISTS port CASCADE;
DROP TABLE IF EXISTS berth CASCADE;
DROP TABLE IF EXISTS slot CASCADE;
--DROP TABLE IF EXISTS portberths CASCADE;

-- CREATE all TABLEs
-- Track  versions of this schema and data sets
-- Intent here is to use a (md5) hash for the (future dataset) for data tracking
-- schemaversion must be unique. This will be updated manually, or by build script
CREATE TABLE dbversion (
  schemaversion bigserial PRIMARY KEY,
  datahash text NOT NULL,
  datadate date,
  desription text
);

CREATE TABLE ship (
  ship_id bigserial PRIMARY KEY,
  name text NOT NULL,
  cargo bigint,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  length bigint,
  draft bigint,
  maxspeed bigint
);

CREATE TABLE port (
  port_id bigserial PRIMARY KEY,
  name text NOT NULL,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  offloadcapacity bigint,
  offloadtime bigint,
  harbordepth bigint,
  available boolean
);

CREATE TABLE berth (
  port_id bigint,
  berth_id bigint,
  berthlength bigint,
  PRIMARY KEY (port_id, berth_id)
);

CREATE TABLE slot (
  port_id bigint,
  berth_id bigint,
  slot_id bigint,
  ship_id bigint REFERENCES ship NOT NULL,
  slotstart bigint,
  slotend bigint,
  PRIMARY KEY (port_id, berth_id, slot_id),
  FOREIGN KEY (port_id, berth_id) REFERENCES berth (port_id, berth_id)
);

-- Add data into TABLEs
INSERT INTO dbversion (schemaversion, datahash, datadate, desription) VALUES
 (3, 'version three', '2017-03-09', 'Updated aid distribution schema, individual port/berth/slot');

INSERT INTO ship (ship_id, name, cargo, latitude, longitude, length, draft, maxspeed) VALUES
 (0, 'NoShip', 0, 0.0, 0.0, 0, 0, 1),
 (1, 'Comfort', 1000, 8.963735, 121.944782, 600, 40, 10),
 (2, 'Mercy', 2000, 8.287618, 122.524406, 800, 50, 20);

 INSERT INTO port (port_id, name, latitude, longitude, 
    offloadcapacity, offloadtime, harbordepth, available) VALUES
  (1, 'CebuCity',          10.316, 123.886, 2500, 2, 65, true),
  (2, 'LapuLapu',          10.268, 123.994, 2000, 3, 55, false),
  (3, 'Alcoy',              9.683, 123.500, 1500, 4, 45, true),
  (4, 'Jagna',              9.650, 124.366, 2000, 3, 50, true),
  (5, 'Tagbilaran',         9.673, 123.873, 1800, 5, 45, false),
  (6, 'SiquijorCity',       9.213, 123.516,  800, 7, 30, false),
  (7, 'SanJuan',            9.167, 123.500, 1000, 8, 40, false),
  (8, 'Maria',              9.200, 123.650, 1100, 6, 45, true),
  (9, 'Larena',             9.250, 123.600,  600, 5, 30, false),
  (10, 'EnriqueVillanueve', 9.272, 123.638, 1500, 5, 50, false);

INSERT INTO berth (port_id, berth_id, berthlength) VALUES
  (1,  1, 1000),
  (1,  2, 1200),
  (2,  1,  800),
  (2,  2, 1000),
  (3,  1,  700),
  (3,  2,  500),
  (4,  1, 1000),
  (4,  2,  800),
  (5,  1,  600),
  (6,  1,  500),
  (7,  1,  600),
  (8,  1,  700),
  (8,  2,  900),
  (9,  1,  400),
  (10, 1,  550),
  (11, 1,  800);

INSERT INTO slot (port_id, berth_id, slot_id, ship_id, slotstart, slotend) VALUES
  ( 1,  1,  1, 0,  0,  4),
  ( 1,  1,  2, 0, 10, 12),
  ( 1,  1,  3, 0, 21, 25),
  ( 1,  2,  1, 0,  0,  6),
  ( 1,  2,  2, 0, 12, 22),
  ( 2,  1,  1, 0,  5,  7),
  ( 2,  1,  2, 0, 11, 15),
  ( 2,  1,  3, 0, 27, 30),
  ( 2,  2,  1, 0,  0, 10),
  ( 2,  2,  2, 0, 15, 25),
  ( 3,  1,  1, 0,  8, 10),
  ( 3,  1,  2, 0, 15, 17),
  ( 3,  1,  3, 0, 22, 30),
  ( 3,  2,  1, 0, 12, 20),
  ( 3,  2,  2, 0, 26, 30),
  ( 4,  1,  1, 0,  0,  7),
  ( 4,  1,  2, 0,  9, 12),
  ( 4,  1,  3, 0, 22, 24),
  ( 4,  2,  1, 0,  0, 10),
  ( 4,  2,  2, 0, 18, 24),
  ( 5,  1,  1, 0,  0,  5),
  ( 5,  1,  2, 0, 13, 20),
  ( 6,  1,  1, 0,  5, 10),
  ( 6,  1,  2, 0, 18, 21),
  ( 7,  1,  1, 0,  7, 11),
  ( 7,  1,  2, 0, 17, 22),
  ( 8,  1,  1, 0,  4,  8),
  ( 8,  1,  2, 0, 14, 17),
  ( 8,  1,  3, 0, 24, 30),
  ( 8,  2,  1, 0,  6, 11),
  ( 8,  2,  2, 0, 20, 30),
  ( 9,  1,  1, 0,  9, 14),
  ( 9,  1,  2, 0, 23, 28),
  (10,  1,  1, 0,  0,  3),
  (10,  1,  2, 0,  9, 14),
  (10,  1,  3, 0, 20, 27);


-- now remove the testing data, leaving tables ready for real data
--TRUNCATE TABLE ship CASCADE;
--TRUNCATE TABLE port CASCADE;
--TRUNCATE TABLE berth CASCADE;
--TRUNCATE TABLE berthslots CASCADE;
--TRUNCATE TABLE portberths CASCADE;

commit;