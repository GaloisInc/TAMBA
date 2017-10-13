import sys

def main ():
    if len(sys.argv) != 4:
        exit(1)

    ship_var_data = [("latitude", 3000, 18000), ("longitude", 115000, 130000), ("maxspeed", 5, 50)]
    port_var_data = [("harbordepth", 10, 100)]

    num_ships = int(sys.argv[1])
    num_ports = int(sys.argv[2])
    file_name = sys.argv[3]

    print "Creating a prob file %s, with %s ship(s) and %s port(s)..." % (file_name, num_ships, num_ports)

    out = open(file_name, 'w+')

    # Secret

    secret_header = "secret:\n"
    secret_header_stmt = "  skip;\n"

    secret = secret_header + secret_header_stmt

    out.write(secret)

    # Belief

    belief_header = "belief:\n"
    belief_header_stmt = ""

    for i in range(num_ships):
        for (prefix, rng_min, rng_max) in ship_var_data:
            belief_header_stmt += ("  int ship%d_%s = uniform %d %d;\n" % (i, prefix, rng_min, rng_max))

    belief_header_stmt += "\n"

    for i in range(num_ports):
        for (prefix, rnd_min, rnd_max) in port_var_data:
            belief_header_stmt += ("  int port%d_%s = uniform %d %d;\n" % (i, prefix, rng_min, rng_max))

    belief = belief_header + belief_header_stmt

    out.write("\n" + belief)

    ## MPC Aid

    ### Signature

    mpc_aid_sig = "querydef mpc_aid ship_id port_id ship_draft ship_cargo port_available port_longitude port_latitude port_offloadcapacity deadline -> result :\n"

    ### Preamble

    mpc_preamble = ""

    for (prefix, _, _) in ship_var_data:
        mpc_preamble += ("  int ship_%s = 0;\n" % prefix)

    mpc_preamble += "\n"

    for (prefix, _, _) in port_var_data:
        mpc_preamble += ("  int port_%s = 0;\n" % prefix)

    mpc_preamble += "\n"

    for i in range(num_ships):
        mpc_preamble += ("  if ship_id == %d then\n" % (i))

        for (prefix, _, _) in ship_var_data:
            mpc_preamble += ("    ship_%s = ship%d_%s;\n" % (prefix, i, prefix))

        mpc_preamble += "  endif;\n"

    mpc_preamble += "\n"

    for j in range(num_ports):
        mpc_preamble += ("  if port_id == %d then\n" % (j))

        for (prefix, _, _) in port_var_data:
            mpc_preamble += ("    port_%s = port%d_%s;\n" % (prefix, j, prefix))

        mpc_preamble += "  endif;\n"

    ### Body
    
    mpc_aid_body = ""

    mpc_aid_body += ("  int reachable = 0;\n"
                     "\n"
                     "  int lat_steps = ship_latitude - port_latitude;\n"
                     "\n"
                     "  if lat_steps < 0 then\n"
                     "    lat_steps = -1 * lat_steps;\n"
                     "  endif;\n"
                     "\n"
                     "  int long_steps = ship_longitude - port_longitude;\n"
                     "\n"
                     "  if long_steps < 0 then\n"
                     "    long_steps = -1 * long_steps;\n"
                     "  endif;\n"
                     "\n"
                     "  if (lat_steps + long_steps) <= deadline * ship_maxspeed then\n"
                     "    reachable = 1;\n"
                     "  endif;\n"
                     "\n"
                     "  int feasible = 0;\n"
                     "\n"
                     "  if (port_available == 1) and (ship_draft <= port_harbordepth) and (ship_cargo <= port_offloadcapacity) then\n"
                     "    feasible = 1;\n"
                     "  endif;\n"
                     "\n"
                     "  int result = 0;\n"
                     "\n"
                     "  if (reachable == 1) and (feasible == 1) then\n"
                     "    result = 1;\n"
                     "  endif;\n"
                     )

    mpc_aid = mpc_aid_sig + mpc_preamble + "\n" + mpc_aid_body

    out.write("\n" + mpc_aid)
   
if __name__ == "__main__": main ()
