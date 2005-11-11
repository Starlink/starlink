# E.S.O. - VLT project 
# "@(#) $Id: rmipc.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# rmipc - remove shared memory and semaphores owned by $USER
#
# who             when       what
# --------------  ---------  ----------------------------------------
# abrighto        11/10/95   created
# pbiereic        26/08/99   only remove memory owned by USER

set i 0
set opts "-m -s"
if { "$tcl_platform(os)" == "Linux" } { set opts "shm sem" }
foreach opt $opts {
    if {$i == 0} {
	puts "removing shared memory areas owned by $env(USER):"
	incr i
    } else {
	puts "removing semaphores owned by $env(USER):"
    }
    set f [open "|ipcs $opt"]
    while {[gets $f line] != -1} {
        if { ![lcontain $line $env(USER)]} { continue }
	set num [lindex $line 1]
	if {![catch {expr $num}]} {
	    if {[catch {exec ipcrm $opt $num} msg]} {
		puts "not removed: $num ($msg)"
	    } else {
		puts "removed: $num"
	    }
        }
    }
}
exit
