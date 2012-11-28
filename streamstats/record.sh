#!/bin/sh

STATUS_URL=http://stream.datenspuren.de:8000/status.xsl
OUTFILE=pentaradio-video.stats
INTERVAL=10

while true
do
    date +%s
    curl -s $STATUS_URL | xsltproc --html icecast-stats.xsl -
    sleep $INTERVAL
done > $OUTFILE
