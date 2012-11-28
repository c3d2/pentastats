set terminal png size 600,400 font "Helvetica, 10"
set xdata time
set timefmt "%s"
set output "pentaradio-20121127.png"
set xrange ["1354050342":"1354057331"]
set xtics 900 format "%H:%M"
set yrange [0:30]
set grid
set xlabel "Time"
set ylabel "Connections"
set title "Stream listeners"
set key left box
plot \
     "pentaradio-20121127-video.data" using ($1+3600):2 title "Video (Ogg)" with lines smooth csplines, \
     "pentaradio-20121127-audio.data" using ($1+3600):3 title "Audio (MP3)" with lines smooth csplines, \
     "pentaradio-20121127-audio.data" using ($1+3600):4 title "Audio (Ogg)" with lines smooth csplines, \
     "pentaradio-20121127-audio.data" using ($1+3600):5 title "Audio (AAC)" with lines smooth csplines, \
     "pentaradio-20121127-audio.data" using ($1+3600):6 title "Audio (MP3 low)" with lines smooth csplines
