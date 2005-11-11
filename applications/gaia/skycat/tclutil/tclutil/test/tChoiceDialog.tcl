source test.tcl

wm withdraw .
set w [ChoiceDialog .d \
	   -text {Please choose...} \
	   -cols 2 \
	   -messagewidth 3i \
	   -choice {One Two Three Four Five Six Seven Eight} \
	   -value {One}]
puts "Dialog returned: [$w activate]"
exit
