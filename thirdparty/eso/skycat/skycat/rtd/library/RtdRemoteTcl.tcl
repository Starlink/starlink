#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdRemoteTcl.tcl,v 1.1.1.1 2006/01/12 16:38:11 abrighto Exp $"
#
# RtdRemoteTcl.tcl - procedures for the Rtd remote Tcl interface
# 
# who              when       what
# --------------   ---------  ----------------------------------------
# Peter Biereichel 11/08/99  created

# connect to a running Rtd process and return the file descriptor
# for the connection socket.

proc connect_to_rtd {} {
    global env
    # get the hostname and port info from the file ~/.rtd-remote,
    # which is created by rtdimage when the remote subcommand is used
    if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
	puts "can't open ~/.rtd-remote: make sure rtd is running: $msg"
	return 0
    }

    lassign [read $fd] pid host port
    #puts ".rtd-remote: pid=$pid, host=$host, port=$port"
    close $fd

    if {[catch {exec kill -0 $pid} msg]} {
	return 0
    }

    set fd [server_connect -nobuf $host $port]
    return $fd
}


# send the command to rtd and return the result

proc send_to_rtd {rtd_fd cmd} {
    puts $rtd_fd $cmd
    lassign [gets $rtd_fd] status length
    set result {}
    if {$length > 0} {
	set result [read $rtd_fd $length]
    }
    if {$status != 0} {
	#puts $result
    }
    return $result
}

# execute tcl command

proc etcl {rtd_fd cmd} {
    #puts "Tcl command: $cmd"
    set ret [send_to_rtd $rtd_fd "remotetcl {$cmd}"]
    #puts "Reply: $ret"
    return $ret
}
