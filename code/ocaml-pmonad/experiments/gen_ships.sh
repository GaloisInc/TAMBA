COMMON="--min-speed 1 --max-speed 10"

(OUTPUT="../data/ships_ship.tsv"
rm -r ${OUTPUT}
touch ${OUTPUT}
for D in 0 1 2 3 4 5 6 7 8 9 10
do
    ./exp_ships ${COMMON} --digits ${D} --margin ship >> ${OUTPUT}
done) &

(OUTPUT="../data/ships_loc.tsv"
rm -r ${OUTPUT}
touch ${OUTPUT}
for D in 0 1 2 3 4 5 6 7 8 9 10
do
    ./exp_ships ${COMMON} --digits ${D} --margin loc >> ${OUTPUT} 
done) &

(OUTPUT="../data/ships_speed.tsv"
rm -r ${OUTPUT}
touch ${OUTPUT}
for D in 0 1 2 3 4 5 6 7 8 9 10
do
    ./exp_ships ${COMMON} --digits ${D} --margin speed >> ${OUTPUT}
done) &

wait
