#! /bin/bash
#
# make-vid.sh
# Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
#
# Distributed under terms of the MIT license.
#


ffmpeg -i img/$1-%d.png -c:v libx264 -preset veryslow -crf 10 $1.mkv
ffmpeg -r 60 -i img/$1-%d.png -c:v libx264 -preset veryslow -crf 10 $1-60fps.mkv

