proc red4ListIndex {taskname dest form} {
#+
# Creates a dialog box for red4 action
#-
    global env

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    set ifile \$CGS4_INDEX/cgs4_$env(CGS4_DATE).index
    if {$dest=="file"} {
      set ofile $env(CGS4_INDEX)/cgs4_$env(CGS4_DATE)_index.lis
    } elseif {$dest=="screen"} {
      set ofile "screen"
    } elseif {$dest=="printer"} {
      set ofile "printer"
    }
    set message "Listing index file to $ofile"
    cgs4drInform $taskname $message
    $taskname obey list_index "index_file=$ifile oformat=$form destination=$dest file=$ofile" -inform "cgs4drInform $taskname %V"
}
