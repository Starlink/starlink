#
# test the EntryForm itk class
#

source test.tcl
wm withdraw .

EntryForm .ef \
    -title {Please enter the data:} \
    -labels {Name Address Tel Fax Age Height Weight} \
    -command puts

.ef config -values {{My Name} {My Address} {My Tel} {My Fax} {My Age} {My Height} {My Weight}}

tkwait window .ef
exit


