source test.tcl
wm withdraw .
set w [ListDialog .d \
	   -text {Please choose} \
	   -choice {{first line} {second line} {third line} {fourth line} {...}} \
	   -modal 0 \
	  ]
puts "Dialog returned: [$w activate]"
exit
