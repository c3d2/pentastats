set terminal png size 600,400 font "Helvetica, 10"
set xdata time
set timefmt "%s"
set output "ds12.png"
#set xrange ["1354050342":"1354057331"]
set xtics 4*3600 format "%a %H:%M"
set yrange [0:35]
set grid
set xlabel "Time"
set ylabel "Connections"
set title "Stream listeners"
set key left box
plot \
     "ds12.data" using ($1+7200):2 title "Großer Saal" with lines smooth csplines, \
     "ds12.data" using ($1+7200):3 title "Kleiner Saal" with lines smooth csplines
