#!/usr/local/bin/tcl
#
# tRemote.tcl 
#
# test the remote RTD interface
#

# open a socket to a running Rtd application and return the file
# descriptor for remote commands

proc connect_to_rtd {} {
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

puts "testing the RTD remote interface..."

# open the connection
set rtd_fd [connect_to_rtd]

# this allows us to use the remote rtdimage as if it were local
set image send_to_rtd

# send some commands to RTD to be evaluated 
puts "WCS image center is: [$image wcscenter]"
puts "image type: [$image bitpix]"
puts "current scale factor: [$image scale]"
puts "image dimensions: [$image width] x [$image height] pixels"
puts "loading a new file: ngc1316r.fits [$image config -file ngc1316r.fits]"
puts "setting cut levels: [$image autocut]"
puts "new image dimensions: [$image width] x [$image height] pixels"

puts "shared memory Id for image data: [$image shm get data]"
puts "shared memory Id for image header: [$image shm get header]"
