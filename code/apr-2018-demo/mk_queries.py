import sys

################################################################################
##### HOW TO USE THIS SCRIPT
################################################################################
# When you invoke this script, you have to provide 4 arguments
#
# 1) The "From" perspective
# 2) The "to" perspective
# 3) The desired distance metric ('man' = manhattan or 'cheb' = chebyshev)
# 4) The desired output file name
#
# The possible perspectives are:
#
#   "us"
#   "hmas"
#   "plan"
#   "boho"
#   "cebu"
#   "siqu"
#
# So a sample execution would look like:
#
# $ python mk_queries.py hmas boho man ausie.prob

def main ():
    if len(sys.argv) != 5:
        exit(1)

    ############################################################################
    # ARGUMENT HANDLING
    #
    # These arguments determine what perspective we are calculating the leakage for.
    perspective_from = sys.argv[1]
    perspective_to   = sys.argv[2]

    # This determines the distance metric
    dist_type = sys.argv[3]

    # This determines the output filename
    file_name = sys.argv[4]
    out = open(file_name, 'w+')

    print "Creating a prob file %s, from %s\'s perspective to %s\'s perspective..." % (file_name, perspective_from, perspective_to)

    num_ships = 10
    num_ports = 10

    ############################################################################
    # SHIP DATA BY PARTNER
    #
    # Ship data are tuples of the following info:
    # id(0),name(1),cargo(2),latitude(3),longitude(4),length(5),draft(6),maxspeed(7)
    us_ships = [(1, "comfort"  ,1000,  8963, 121944,600,40,10),
                (2, "mercy"    ,2000,    8287, 122524,800,50,20),
                (3, "sanctuary",1800,11145,126899,500,35,30),
                (4, "haven"    ,1500,    9563, 118995,550,30,35)]

    hmas_ships = [(5, "refuge",2000,   5685, 119124,650,48,32),
                  (6, "hope"  ,1600,     7496, 119503,480,40,22),
                  (7, "solace",1400,   9208, 126801,450,32,25)]

    plan_ships = [(8,  "peaceArk",1900, 8703, 122962,685,55,34),
                  (9,  "nanyun"  ,1400,   9232, 120660,550,46,28),
                  (10, "nankang" ,1200,  11800,126280,500,42,26)]
############################################################################
    # PORT DATA BY PARTNER
    #
    # Port data are tuples of the following info:
    # id(0),name(1),latitude(2),longitude(3),offloadcapacity(4),offloadtime(5),harbordepth(6),available(7)
    #
    # The only actual secret is the port harbor-depth

    bohol_ports    = [(4,  "jagna",            9650,  124366, 2000, 3, 50, True),
                      (5,  "tagbilaran",       9673,  123873, 1800, 5, 45, True)]

    cebu_ports     = [(1,  "cebucity",         10316, 123886, 2500, 2, 65, True),
                      (2,  "lapulapu",         10268, 123994, 2000, 3, 55, True),
                      (3,  "alcoy",            9683,  123500, 1500, 4, 45, True)]

    siquijor_ports = [(6,  "siquijorcity",     9213,  123516, 800,  7, 30, True),
                      (7,  "sanjuan",          9167,  123500, 1000, 8, 40, True),
                      (8,  "maria",            9200,  123650, 1100, 6, 45, True),
                      (9,  "larena",           9250,  123600, 600,  5, 30, True),
                      (10, "enriquevillanueve",9272,  123638, 1500, 5, 50, True)]

    ############################################################################ 
    # PARTNER MAP
    #
    # Dictionary mapping a partner to its data. The boolean determines whether
    #     it's a ship (True) or not (False)
    partner_map = { "us"   : (True, us_ships),
                    "hmas" : (True, hmas_ships),
                    "plan" : (True, plan_ships),
                    "boho" : (False, bohol_ports),
                    "cebu" : (False, cebu_ports),
                    "siqu" : (False, siquijor_ports)}

    # look up the partner's data then remove that item from the map
    is_ship, data = partner_map[perspective_from]

    del partner_map[perspective_from]

    ############################################################################
    # WRITE OUT THE SECRET DATA
    #
    # Here we write out the secret data of the 'from' perspective that
    # we've been provided

    secret_header = "secret:\n"
    secret_header_stmt = ""

    if is_ship:
        for ship in data:
            secret_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "lat", ship[3]))
            secret_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "long", ship[4]))
            secret_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "maxspeed", ship[7]))
    else:
        for port in data:
            secret_header_stmt += ("  int port%d_%s = %d;\n" % (port[0], "harbordepth", port[6]))


    secret = secret_header + secret_header_stmt

    out.write(secret)

    ############################################################################
    # WRITE OUT PRIOR BELIEF
    #
    # Here we write out the initial belief using the 'from' perspective that
    # we've been provided

    ship_var_data_ranges = [("lat", 3000, 18000), ("long", 115000, 130000), ("maxspeed", 5, 50)]
    port_var_data_ranges = [("harbordepth", 10, 100)]

    belief_header = "belief:\n"
    belief_header_stmt = ""

    if is_ship:
        for ship in data:
            for (prefix, rng_min, rng_max) in ship_var_data_ranges:
                belief_header_stmt += ("  int ship%d_%s = uniform %d %d;\n" % (ship[0], prefix, rng_min, rng_max))
    else:
        for port in data:
            for (prefix, rng_min, rng_max) in port_var_data_ranges:
                belief_header_stmt += ("  int port%d_%s = uniform %d %d;\n" % (port[0], prefix, rng_min, rng_max))

    belief = belief_header + belief_header_stmt

    out.write(belief)
        

    ############################################################################
    ## Query

    mpc_aid_sig = "querydef mpc_aid ship_id port_id ship_draft ship_cargo port_available port_long port_lat port_offloadcapacity deadline -> result :\n"

    ### Preamble Print out data of non-perspective ships

    mpc_preamble = ""

    for partner in partner_map:
        is_ship, data = partner_map[partner]
        if is_ship:
            for ship in data:
                mpc_preamble += ("  int ship%d_%s = %d;\n" % (ship[0], "lat", ship[3]))
                mpc_preamble += ("  int ship%d_%s = %d;\n" % (ship[0], "long", ship[4]))
                mpc_preamble += ("  int ship%d_%s = %d;\n" % (ship[0], "maxspeed", ship[7]))
        else:
            for port in data:
                mpc_preamble += ("  int port%d_%s = %d;\n" % (port[0], "harbordepth", port[6]))
            
    mpc_preamble += "\n"

    for (prefix, _, _) in ship_var_data_ranges:
        mpc_preamble += ("  int ship_%s = 0;\n" % prefix)

    mpc_preamble += "\n"

    for (prefix, _, _) in port_var_data_ranges:
        mpc_preamble += ("  int port_%s = 0;\n" % prefix)

    mpc_preamble += "\n"

    ############################################################################
    # Write out ship selection logic for 10 ships

    for i in range(num_ships):
        mpc_preamble += ("  if ship_id == %d then\n" % (i))

        for (prefix, _, _) in ship_var_data_ranges:
            mpc_preamble += ("    ship_%s = ship%d_%s;\n" % (prefix, i, prefix))

        mpc_preamble += "  endif;\n"

    mpc_preamble += "\n"

    ############################################################################
    # Write out port selection logic for 10 ports

    for j in range(num_ports):
        mpc_preamble += ("  if port_id == %d then\n" % (j))

        for (prefix, _, _) in port_var_data_ranges:
            mpc_preamble += ("    port_%s = port%d_%s;\n" % (prefix, j, prefix))

        mpc_preamble += "  endif;\n"


    ############################################################################
    # Write out the core logic
    
    chebyshev    = ""
    manhattan    = ""
    mpc_aid_body = ""

    ############################
    # Different distance metrics
    
    chebyshev += ("  int x_diff = ship_lat - port_lat;\n"
                 "\n"
                 "  if x_diff < 0 then\n"
                 "      x_diff = -1 * x_diff;\n"
                 "  endif;\n"
                 "\n"
                 "  int y_diff = ship_long - port_long;\n"
                 "  \n"
                 "  if y_diff < 0 then\n"
                 "      y_diff = -1 * y_diff;\n"
                 "  endif;\n"
                 "\n"
                 "  if x_diff > y_diff then\n"
                 "      if (x_diff <= deadline * ship_speed) then\n"
                 "          reachable = 1;\n"
                 "      endif;\n"
                 "  else\n"
                 "      if (y_diff <= deadline * ship_speed) then\n"
                 "          reachable = 1;\n"
                 "      endif;\n"
                 "  endif; \n")


    manhattan += ("  if (ship_lat - port_lat) + (ship_long - port_long) <= deadline * ship_speed and\n"
                  "     (ship_lat - port_lat) + (port_long - ship_long) <= deadline * ship_speed and\n"
                  "     (port_lat - ship_lat) + (port_long - ship_long) <= deadline * ship_speed and\n"
                  "     (port_lat - ship_lat) + (ship_long - port_long) <= deadline * ship_speed then\n"
                  "    reachable = 1;\n"
                  "  endif;\n"
                  "\n")

    dist_map = { "cheb" : chebyshev, "man" : manhattan }


    mpc_aid_body += ("  int feasible = 0;\n"
                     "\n"
                     "  if (port_available == 1) and (ship_draft <= port_harbordepth) and (ship_cargo <= port_offloadcapacity) then\n"
                     "    feasible = 1;\n"
                     "  endif;\n"
                     "\n"
                     "  int result = 0;\n"
                     "\n"
                     "  if (reachable == 1) and (feasible == 1) then\n"
                     "    result = 1;\n"
                     "  endif;\n")

    mpc_aid = mpc_aid_sig + mpc_preamble + "\n" + dist_map[dist_type] + "\n" + mpc_aid_body

    out.write("\n" + mpc_aid)

if __name__ == "__main__": main ()
