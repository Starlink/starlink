
set fid [open "../README" "r"]
set data [read $fid]
close $fid

regsub -all "\n|\t" $data " " data 
set data [split $data " "]

set count 0
set maxWords 500
foreach word $data {
    if { $count & 0x1 } {
	puts stderr "($word)"
	flush stderr
    } else {
	puts stdout "($word)"
	flush stdout
    }
    incr count
    if { $count > $maxWords } {
	break
    }
    after 500
}
exit 0

