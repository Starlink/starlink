source test.tcl

#
# test the tcscat tcl command
#

puts "testing the tcscat Tcl command..."
tcscat cat
cat open gsc@eso
puts "GSC headings: [cat headings]"
puts "GSC query:"
set list [cat query -pos {3:19:48 +41:30:39} -radius 10 -nrows 10]
foreach i $list {
    puts $i
}
exit 0
