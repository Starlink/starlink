#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/cat.sh "$0" "$@"

# test the PreviewPlot class

puts "auto_path = $auto_path"
util::setXdefaults
wm withdraw .

PreviewPlot .pp -file test.preview
tkwait window .pp
exit


