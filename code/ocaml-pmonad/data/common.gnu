set term postscript eps enhanced 10
set size 0.5, 0.3

default_tics_scale = 0.5

set grid back lt 0 lw 1 lc rgb "#aaaaaa"
set log y 2

set for [e = 0 : 10] ytics add ("2^{".(sprintf("%1.1f", (e/-2.0)))."}" 2**(e/-2.0))

set xtics 1
set xtics format "%g"

set xtics scale default_tics_scale
set ytics scale default_tics_scale

dl(x) = (log(x) / log(2))

minmargin = 1.0

set tmargin minmargin
set bmargin 3.0
set lmargin 8.0
set rmargin minmargin

xoff = 0.5
yoff = 0.5
