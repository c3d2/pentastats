pentaradio-20121127.png: pentaradio-20121127.gnuplot pentaradio-20121127-audio.data pentaradio-20121127-video.data
	gnuplot $<

ds12.png: ds12.gnuplot ds12.data
	gnuplot $<

%.data: %.stats stats2data
	time ./stats2data < $< > $@

stats2data: stats2data.hs
	ghc --make -O2 $@

clean:
	rm -f stats2data{,.hi,.o} *.data *.png
