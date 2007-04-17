source test.tcl
wm withdraw .
set w [TextDialog .d \
	   -text {This is a test message} \
	   -contents [exec cat ../library/TextDialog.tcl] \
	  ]
puts "Dialog returned: [$w activate]"
exit
