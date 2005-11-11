source test.tcl
wm withdraw .
set w [DialogWidget .d \
	   -text {This is a test message} \
	   -bitmap warning \
	   -default 1 \
	   -buttons {OK Cancel Help}]
#foreach i [$w config] {puts $i}
puts "Dialog returned: [$w activate]"
exit
