#
proc UpdateSetting {key val} {
  global InitFile

  if {[file exists $InitFile] == 1} {
    set f1 [open $InitFile r]
    set f2 [open $InitFile~ w]
    while {[gets $f1 line] >=0} {
      puts $f2 $line
    }
    close $f1
    close $f2
    set f1 [open $InitFile w]
    set f2 [open $InitFile~ r]
    while {[gets $f2 line] >= 0} {
      if {[string first $key $line] < 0} {
        puts $f1 $line
      }
    }
    close $f2
  } else {
    set f1 [open $InitFile w]
  }
  puts $f1 $key=$val
  close $f1
}
