#! /usr/bin/wish
#
# This TCL script (for Tk version 4.1 or later) implements a very very
# simple web server.  Only the GET method is supported.  There is no CGI.
# There are no logs.  There is no security.  You have been warned!
#
wm title . {Demo Web Server}
wm iconname . {DemoWebServer}
option add *highlightThickness 0

#
# Construct the main menu
#
frame .mbar -bd 2 -relief raised
pack .mbar -side top -fill x -expand 1
menubutton .mbar.file -text File -underline 0 -menu .mbar.file.menu
menubutton .mbar.edit -text Edit -underline 0 -menu .mbar.edit.menu
menubutton .mbar.help -text Help -underline 0 -menu .mbar.help.menu
pack .mbar.file .mbar.edit .mbar.help -side left -pady 0 -padx 10
set m [menu .mbar.file.menu]
$m add command -label Exit -underline 1 -command exit
set m [menu .mbar.edit.menu]
$m add command -label {Start Server} -command StartServer
$m add command -label {Stop Server} -command StopServer -state disabled
set m [menu .mbar.help.menu]
$m add command -label Instructions -underline 0 -state disabled
$m add command -label {About This Program} -underline 0 -state disabled

#
# Construct a panel used to enter server parameters
#
frame .param -bd 1 -relief raised
pack .param -side top -fill x -expand 1
frame .param.port
pack .param.port -side top -padx 5 -pady 5 -anchor w
label .param.port.label -text {Port: } -anchor e -width 16
entry .param.port.entry -bd 1 -relief sunken -width 7
.param.port.entry insert end 80
pack .param.port.label .param.port.entry -side left -anchor w
frame .param.root
pack .param.root -side top -padx 5 -pady 5 -anchor w
label .param.root.label -text {Root Directory: } -anchor e -width 16
entry .param.root.entry -bd 1 -relief sunken -width 30
.param.root.entry insert end .
pack .param.root.label .param.root.entry -side left -anchor w
frame .param.hit
pack .param.hit -side top -padx 5 -pady 5 -anchor w
label .param.hit.label -text {Hits: } -anchor e -width 16
label .param.hit.cnt -text 0 -anchor w -width 4
pack .param.hit.label .param.hit.cnt -side left -anchor w
set hitcnt 0

#
# Construct a row of buttons for controlling the system
#
frame .btn -bd 1 -relief raised
pack .btn -side top -fill x -expand 1
button .btn.start -text Start -command StartServer
button .btn.stop -text Stop -command StopServer -state disabled
button .btn.quit -text Exit -command exit
pack .btn.start .btn.stop .btn.quit -side left -padx 10 -pady 10 -expand 1

#
# Call this function in order to start the HTTP server running
#
proc StartServer {} {
  .btn.start config -state disabled
  .btn.stop config -state normal
  .mbar.edit.menu entryconfig {Stop Server} -state normal
  .mbar.edit.menu entryconfig {Start Server} -state disabled
  global sock root
  set sock [socket -server Hit [.param.port.entry get]]
  set root [.param.root.entry get]
}

#
# Call this function to stop the HTTP server
#
proc StopServer {} {
  .btn.stop config -state disabled
  .btn.start config -state normal
  .mbar.edit.menu entryconfig {Stop Server} -state disabled
  .mbar.edit.menu entryconfig {Start Server} -state normal
  global sock
  catch {close $sock}
  set sock {}
}

#
# This function is called whenever anyone attempts to access the HTTP
# server.
#
proc Hit {s addr port} {
  fconfigure $s -blocking 0
  fileevent $s readable "DoGet $s"
}

#
# This function is called to process the GET message sent by the
# web browser to this HTTP server.  It decodes the GET message and
# sends a suitable reply (or error message).
#
proc DoGet {s} {
  set x [gets $s]
  if {"$x"!=""} {
    global root hitcnt
    incr hitcnt
    .param.hit.cnt config -text $hitcnt
    if {"[lindex $x 0]"!="GET"} {
      puts $s "Unable to service request"
      close $s
      return
    }
    set file [lindex $x 1]
    if {![file readable $root/$file]} {
      puts $s "Can't access $file"
      close $s
      return
    }
    set f [open $root/$file r]
    while {![eof $f]} {
      puts $s [gets $f]
    }
    close $f
    close $s
  }
}
