#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/cat.sh "$0" "$@"

#
# test the astroimage tcl command
#

puts "testing the astrocat Tcl command with an image server..."
astrocat cat
cat open dss@eso
puts "getting image..."

if {[catch {
    set filename [cat getimage -pos {3:19:48 +41:30:39} -width 1 -height 1]
} msg]} {
    puts "getimage returned error: '$msg'"
    exit 1
}

puts "getimage returns filename = '$filename', renaming to ./dss.fits..."
exec mv $filename ./dss.fits

exit 0
