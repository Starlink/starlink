source test.tcl

# test the PreviewPlot class

puts "auto_path = $auto_path"
util::setXdefaults
wm withdraw .

PreviewPlot .pp -file test.preview
tkwait window .pp
exit


