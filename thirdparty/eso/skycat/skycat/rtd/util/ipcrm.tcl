
puts "removing shared memory areas (tcl script)\n[exec ipcs -m]:"
set f [open "|ipcs -m"]
while {[gets $f line] != -1} {
    set num [lindex $line 1]
    if {![catch {expr $num}]} {
	if {[catch {exec /bin/ipcrm -m $num} msg]} {
	    puts "not removed: $num ($msg)"
	} else {
	    puts "removed: $num"
	}
    }
}
exit
