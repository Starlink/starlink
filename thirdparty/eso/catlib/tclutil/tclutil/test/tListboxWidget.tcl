source test.tcl

set w [ListboxWidget .lb \
	   -title "The Listbox" \
	  ]
pack $w -fill x -expand 1

$w set_contents {
    {one   two   three ...}
    {four  five  six ...}
    {seven eight nine ...}
}

$w select_row 1
puts "selected [$w get_selected]"
puts "internal listbox name is: [$w component listbox]"
 
bind [$w component listbox] <ButtonRelease-1> "puts \[$w get_selected\]"
