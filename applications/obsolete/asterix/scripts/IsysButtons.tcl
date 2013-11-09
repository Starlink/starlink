#
proc StaticButton {w group txt} {
  global ContextHelp
  global Txt
  global Buttons
  set Buttons($w,help) $txt
  set Buttons($w,type) static
  set Buttons($w,group) $group
  set Txt $txt
}
#
#
proc DynamicButton {w group txt} {
  global ContextHelp
  global Txt
  global Buttons
  set Buttons($w,help) $txt
  set Buttons($w,type) dynamic
  set Buttons($w,group) $group
  $w configure -relief flat
  set Txt $txt
}
#
#
proc DisableButton {w} {
  global Buttons
  global ContextHelp
  global Txt
  
  set Txt $Buttons($w,help)
  $w configure -state disabled
  bind $w <Enter> "set ContextHelp \"$Txt\""
  bind $w <Leave> "set ContextHelp \" \""
}
#
#
proc EnableButton {w} {
  global Buttons
  global ContextHelp
  global Txt
  
  set Txt $Buttons($w,help)
  $w configure -state normal

    if {$Buttons($w,type) == "dynamic"} {
      bind $w <Enter> "set ContextHelp \"$Txt\";$w configure -relief raised"
      bind $w <Leave> "set ContextHelp \" \";$w configure -relief flat"
    } else {
      bind $w <Enter> "set ContextHelp \"$Txt\""
      bind $w <Leave> "set ContextHelp \" \""
    }
}
#
#
proc DisableGroup {group} {
  global Buttons

    foreach i [array names Buttons] {
	if {[string first group $i] >= 0} {
	    if {$Buttons($i) == $group} {
		set p [string first "," $i]
                incr p -1
		set name [string range $i 0 $p]
                DisableButton $name
            }
        }
    } 

}
#
#
proc EnableGroup {group} {
  global Buttons

    foreach i [array names Buttons] {
	if {[string first group $i] >= 0} {
	    if {$Buttons($i) == $group} {
		set p [string first "," $i]
                incr p -1
		set name [string range $i 0 $p]
                EnableButton $name
            }
        }
    } 

}
#
#
