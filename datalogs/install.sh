#!/bin/sh
curl -k -L -Ss https://lehre.bpm.in.tum.de/~pm-prak/datasets.zip > data.zip
unzip -q -u -o data.zip
rm data.zip
echo Downloaded log files from https://lehre.bpm.in.tum.de/~pm-prak/