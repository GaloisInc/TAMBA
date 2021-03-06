import sys

################################################################################
##### HOW TO USE THIS SCRIPT
################################################################################
# When you invoke this script, you have to provide 4 arguments
#
# 1) The "From" perspective
# 2) The "to" perspective
# 3) The desired distance metric ('man' = manhattan or 'cheb' = chebyshev)
# 4) The deadline
# 5) The desired output file name
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
# $ python mk_queries.py hmas boho man 50 ausie.prob

def main ():
    if len(sys.argv) != 7:
        exit(1)

    ############################################################################
    # ARGUMENT HANDLING
    #
    # These arguments determine what perspective we are calculating the leakage for.
    perspective_from = sys.argv[1]
    perspective_to   = sys.argv[2]

    # This determines the distance metric
    dist_type = sys.argv[3]

    # The deadline
    deadline = int(sys.argv[4])

    # This determines the output filename
    file_name = sys.argv[5]
    out = open(file_name, 'w+')

    # The ship to restrict to
    ship_restrict = sys.argv[6]

    print "Creating a prob file %s, from %s\'s perspective to %s\'s perspective..." % (file_name, perspective_from, perspective_to)

    num_ships = 10
    num_ports = 10

    ############################################################################
    # SHIP DATA BY PARTNER
    #
    # Ship data are tuples of the following info:
    # id(0),name(1),cargo(2),latitude(3),longitude(4),length(5),draft(6),maxspeed(7)
    us_ships = [(1, "comfort"  ,1000, 156, 2128,600,40,10),
                (2, "mercy"    ,2000, 145, 2138,800,50,20),
                (3, "sanctuary",1800, 195, 2215,500,35,30),
                (4, "haven"    ,1500, 167, 2077,550,30,35)]

    hmas_ships = [(5, "refuge",2000,   99, 2079,650,48,32),
                  (6, "hope"  ,1600,  131, 2086,480,40,22),
                  (7, "solace",1400,  161, 2213,450,32,25)]

    plan_ships = [(8,  "peaceArk",1900, 152, 2146,685,55,34),
                  (9,  "nanyun"  ,1400, 161, 2106,550,46,28),
                  (10, "nankang" ,1200, 206, 2204,500,42,26)]

    all_ships = us_ships + hmas_ships + plan_ships

    ############################################################################
    # PORT DATA BY PARTNER
    #
    # Port data are tuples of the following info:
    # id(0),name(1),latitude(2),longitude(3),offloadcapacity(4),offloadtime(5),harbordepth(6),available(7)
    #
    # The only actual secret is the port harbor-depth

    bohol_ports    = [(4,  "jagna",             168,  2171, 2000, 3, 50, True),
                      (5,  "tagbilaran",        169,  2162, 1800, 5, 45, True)]

    cebu_ports     = [(1,  "cebucity",          180,  2162, 2500, 2, 65, True),
                      (2,  "lapulapu",          179,  2164, 2000, 3, 55, True),
                      (3,  "alcoy",             169,  2155, 1500, 4, 45, True)]

    siquijor_ports = [(6,  "siquijorcity",      161,  2156, 800,  7, 30, True),
                      (7,  "sanjuan",           160,  2155, 1000, 8, 40, True),
                      (8,  "maria",             161,  2158, 1100, 6, 45, True),
                      (9,  "larena",            161,  2157, 600,  5, 30, True),
                      (10, "enriquevillanueve", 162,  2158, 1500, 5, 50, True)]

    all_ports = bohol_ports + cebu_ports + siquijor_ports

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
    # We need to know the set of perspective: `pers`, the data
    # for the 'from' perspective `pers_from` and the data
    # for the 'to' perspective `pers_to`

    pers_from = partner_map[perspective_from]
    pers_to   = partner_map[perspective_to]

    pers = []
    for part in set([perspective_from, perspective_to]):
        pers.append(partner_map[part])
        del partner_map[part]


    ############################################################################
    # WRITE OUT THE SECRET DATA
    #
    # Here we write out the secret data of the 'from' perspective that
    # we've been provided

    secret_header = "secret:\n"
    secret_header_stmt = ""

    for is_ship, data in pers:
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

    ship_var_data_ranges = [("lat", 52, 314), ("long", 2007, 2269), ("maxspeed", 5, 50)]
    port_var_data_ranges = [("harbordepth", 10, 100)]

    belief_header = "belief:\n"
    belief_header_stmt = ""

    # From perspective has a belief
    (is_ship,data) = pers_from
    if is_ship:
        for ship in data:
            for (prefix, rng_min, rng_max) in ship_var_data_ranges:
                belief_header_stmt += ("  int ship%d_%s = uniform %d %d;\n" % (ship[0], prefix, rng_min, rng_max))
    else:
        for port in data:
            for (prefix, rng_min, rng_max) in port_var_data_ranges:
                belief_header_stmt += ("  int port%d_%s = uniform %d %d;\n" % (port[0], prefix, rng_min, rng_max))

    # To perspective is constant so it doesn't contribute to belief
    (is_ship,data) = pers_to
    if is_ship:
        for ship in data:
            belief_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "lat", ship[3]))
            belief_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "long", ship[4]))
            belief_header_stmt += ("  int ship%d_%s = %d;\n" % (ship[0], "maxspeed", ship[7]))
    else:
        for port in data:
            belief_header_stmt += ("  int port%d_%s = %d;\n" % (port[0], "harbordepth", port[6]))

    belief = belief_header + belief_header_stmt

    out.write(belief)
        

    ############################################################################
    ## QUERY DEFINITION

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

    for i in range(1, num_ships + 1):
        mpc_preamble += ("  if ship_id == %d then\n" % (i))

        for (prefix, _, _) in ship_var_data_ranges:
            mpc_preamble += ("    ship_%s = ship%d_%s;\n" % (prefix, i, prefix))

        mpc_preamble += "  endif;\n"

    mpc_preamble += "\n"

    ############################################################################
    # Write out port selection logic for 10 ports

    for j in range(1, num_ports + 1):
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

    reachable = "  int reachable = 0;\n\n"
    

    chebyshev += ("  if ((3959*(ship_lat - port_lat) <= deadline * ship_maxspeed) and\n"
                  "      (3959*(port_lat - ship_lat) <= deadline * ship_maxspeed)) or\n"
                  "     ((3959*(ship_long - port_long) <= deadline * ship_maxspeed) and\n"
                  "      (3959*(port_long - ship_long) <= deadline * ship_maxspeed)) then\n"
                  "    reachable = 1;\n"
                  "  endif;\n"
                  "\n")

    manhattan += ("  if 3959*((ship_lat - port_lat) + (ship_long - port_long)) <= deadline * ship_maxspeed and\n"
                  "     3959*((ship_lat - port_lat) + (port_long - ship_long)) <= deadline * ship_maxspeed and\n"
                  "     3959*((port_lat - ship_lat) + (port_long - ship_long)) <= deadline * ship_maxspeed and\n"
                  "     3959*((port_lat - ship_lat) + (ship_long - port_long)) <= deadline * ship_maxspeed then\n"
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

    mpc_aid = mpc_aid_sig + mpc_preamble + "\n" + reachable + dist_map[dist_type] + "\n" + mpc_aid_body

    out.write("\n" + mpc_aid)

    ############################################################################
    ## QUERY SEQUENCE



    ## Seeing which ships can reach which ports

    query_data = []

    is_ship_from, data_from = pers_from
    is_ship_to, data_to = pers_to
    if is_ship_from:
        for (sid, _, scar, sla, slo, sle, sdr, sp) in data_from:
            if(str(sid) == ship_restrict):
                for (pid, _, pla, plo, off, offt, pde, pav) in data_to:
                    query_data.append((sid,pid,sdr,scar,pav,plo,pla,off))
    else:
        for (sid, _, scar, sla, slo, sle, sdr, sp) in data_to:
            if(str(sid) == ship_restrict):
                for (pid, _, pla, plo, off, offt, pde, pav) in data_from:
                    query_data.append((sid,pid,sdr,scar,pav,plo,pla,off))

    # remove duplicate queries
    query_data = set(query_data)

    query_num = 0
    for (sid, pid, sdr, scar, pav, plo, pla, off) in query_data:
        query_num += 1
        query_call =  ("(* %d *)\n" % (query_num))
        query_call += ("query mpc_aid:\n")
        query_call += ("  int ship_id = %d;\n" % (sid))
        query_call += ("  int port_id = %d;\n" % (pid))
        query_call += ("  int ship_draft = %d;\n" % (sdr))
        query_call += ("  int ship_cargo = %d;\n" % (scar))
        query_call += ("  int port_available = %d;\n" % (pav))
        query_call += ("  int port_long = %d;\n" % (plo))
        query_call += ("  int port_lat = %d;\n" % (pla))
        query_call += ("  int port_offloadcapacity = %d;\n" % (off))
        query_call += ("  int deadline = %d;\n" % deadline)

        out.write("\n" + query_call)

    print("Total number of queries generated: %d\n" % len(query_data))

if __name__ == "__main__": main ()
