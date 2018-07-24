#! /bin/bash
#
# make-hept-vid.sh
# Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
#
# Distributed under terms of the MIT license.
#

# see 
# https://en.wikibooks.org/wiki/FFMPEG_An_Intermediate_Guide/image_sequence#Making_a_video_from_an_Image_Sequence 

ffmpeg -i img/hept-%d.png hept.mp4

