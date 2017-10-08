import sys

# reachable ::=
#   int reachable = 0;
#
#   int lat_steps = ship?_latitude - port_latitude;
#
#   if lat_steps < 0 then
#     lat_steps = -lat_steps;
#   endif;
#
#   int long_steps = ship?_longitude - port_longitude;
#
#   if long_steps < 0 then
#     long_step = -long_steps;
#   endif;
#
#   if (lat_steps + long_steps) / ship?_maxspeed <= deadline then
#     reachable = 1;
#   endif;

# feasible ::=
#   int feasible = 0;
#
#   if (port_available && ship_draft <= port?_harbordepth && ship_cargo <= port_cap) then
#     feasible = 1;
#   endif;

# aid ::=
#   int result = 0;
#
#   if (reachable && feasible) then
#     result = 1;
#   endif;

def main ():
    if len(sys.argv) != 4:
        exit(1)

    num_ships = int(sys.argv[1])
    num_ports = int(sys.argv[2])
    file_name = sys.argv[3]

    print "Creating a prob file %s, with %s ship(s) and %s port(s)..." % (file_name, num_ships, num_ports)

    out = open(file_name, 'w+')

    secret_header = "secret:\n"
    secret_header_stmt = "  skip;\n\n"

    out.write(secret_header)
    out.write(secret_header_stmt)

    # Are all these actually private? Or do some of them need to be public args to queries?
    ship_var_data = [("latitude", -90000, 90000), ("longitude", -180000, 180000), ("maxspeed", 5, 50)]
    port_var_data = [("harbordepth", 10, 100)]

    belief_header = "belief:\n"
    belief_header_stmt = ""

    for i in range(num_ships):
        for (prefix, rng_min, rng_max) in ship_var_data:
            belief_header_stmt += ("  int ship%d_%s = uniform %d %d;\n" % (i, prefix, rng_min, rng_max))

    belief_header_stmt += "\n"

    for i in range(num_ports):
        for (prefix, rnd_min, rnd_max) in port_var_data:
            belief_header_stmt += ("  int port%d_%s = uniform %d %d;\n" % (i, prefix, rng_min, rng_max))

    out.write(belief_header)
    out.write(belief_header_stmt)

    querydefs = "\n"

    for i in range(num_ships):
        for j in range(num_ports):
            querydef_name = "querydef aid_%d_%d ship_draft ship_cargo port_latitude port_longitude port_available port_cap deadline -> result :\n" % (i, j)

            querydef_reachable = ("  int reachable = 0;\n"
                                  "\n"
                                  "  int lat_steps = ship" + str(i) + "_latitude - port_latitude;\n"
                                  "\n"
                                  "  if lat_steps < 0 then\n"
                                  "    lat_steps = -1 * lat_steps;\n"
                                  "  endif;\n"
                                  "\n"
                                  "  int long_steps = ship" + str(i) + "_longitude - port_longitude;\n"
                                  "\n"
                                  "  if long_steps < 0 then\n"
                                  "    long_step = -1 * long_steps;\n"
                                  "  endif;\n"
                                  "\n"
                                  "  if (lat_steps + long_steps) / ship" + str(i) + "_maxspeed <= deadline then\n"
                                  "    reachable = 1;\n"
                                  "  endif;\n\n"
                                  )

            querydef_feasible = ("  int feasible = 0;\n"
                                 "\n"
                                 "  if (port_available == 1) and (ship_draft <= port" + str(j) + "_harbordepth) and (ship_cargo <= port_cap) then\n"
                                 "    feasible = 1;\n"
                                 "  endif;\n\n"
                                 )

            querydef_aid = ("  int result = 0;\n"
                            "\n"
                            "  if (reachable == 1) and (feasible == 1) then\n"
                            "    result = 1;\n"
                            "  endif;\n"
                            )

            querydefs += querydef_name
            querydefs += querydef_reachable
            querydefs += querydef_feasible
            querydefs += querydef_aid
            querydefs += "\n"

    out.write(querydefs)

if __name__ == "__main__": main ()
