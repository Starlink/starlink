proc cgs4drSetXopts {} {
  global env
  global cgs4drXopts
  # Check if file exists, if not copy a default
  if {[file exists $cgs4drXopts(File)] == 0} {
    exec /usr/bin/cp $env(CGS4DR_ROOT)/tcl/cgs4dr.xopts $cgs4drXopts(File)
  }
  # Now read the options
  set fid [open $cgs4drXopts(File) r]
  while {[gets $fid text]>=0} {
    if {([llength $text] == 2) && ([string index $text 0] != "#")} {
      set cgs4drXopts([lindex $text 0]) [lindex $text 1]
    }
  }
  # Now set the options
  foreach option [array names cgs4drXopts {x*}] {
    option add *[string range $option 1 end] $cgs4drXopts($option)
  }
}
