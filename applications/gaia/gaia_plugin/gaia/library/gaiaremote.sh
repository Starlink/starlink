#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_wish $0 ${1+"$@"}
#
# gaRemote
#
# test the remote RTD interface byb displaying an image.
#

wm withdraw .

# open a socket to a running Rtd application and return the file
# descriptor for remote commands

proc connect_to_gaia {} {
    global env
    # get the hostname and port info from the file ~/.rtd-remote,
    # which is created by rtdimage when the remote subcommand is used
    if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
	puts "can't open ~/.rtd-remote: make sure rtd is running: $msg"
	exit 1
    }

    lassign [read $fd] pid host port
    close $fd

    if {[catch {exec kill -0 $pid} msg]} {
	puts "could it be that rtd is not running? ($msg)"
	exit 1
    }

    set fd [server_connect -nobuf $host $port]
    return $fd
}

# send the command to rtd and return the results or generate an error

proc send_to_rtd {args} {
    global rtd_fd
    puts $rtd_fd $args
    lassign [gets $rtd_fd] status length
    set result {}
    if {$length > 0} {
	set result [read $rtd_fd $length]
    }
    if {$status != 0} {
	error $result
    }
    return $result
}

if { $argc == 0 } { 
   puts "Usage: gaRemote gaia_command"
   exit
}

#  Open up connection to GAIA.
set rtd_fd [connect_to_gaia]

puts [eval send_to_rtd $argv]
exit
