#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}

puts "Removing shared memory areas (tcl script)\n[exec ipcs -m]:"
set f [open "|ipcs -m"]
while {[gets $f line] != -1} {
    set num [lindex $line 1]
    if {![catch {expr $num}]} {
	if {[catch {exec ipcrm -m $num} msg]} {
	    puts "not removed: $num ($msg)"
	} else {
	    puts "removed: $num"
	}
    }
}
exit
