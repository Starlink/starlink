#
proc edit_cols {cwin} {

  global nbid
  global coltab

  global Wcol
  set Wcol $cwin

  toplevel $Wcol -class Dialog
  wm title $Wcol "Colour Table"
  wm iconname $Wcol Colours


  pack [frame $Wcol.left] [frame $Wcol.right -relief groove -bd 2] \
                                             -side left -padx 5m

  set wid 100
  set ht 320
  pack [canvas [set c $Wcol.right.display] -width $wid -height $ht \
                   -closeenough 1]
  HelpText $c \
    "Drag with left button for red, middle for green, right for blue"

  button $Wcol.left.def -text "Default" -command \
        "edit_cols_update def $c $wid $ht"
  HelpText $Wcol.left.def "Default Asterix colour table"
  button $Wcol.left.red -text "Red" -command \
        "edit_cols_update red $c $wid $ht"
  HelpText $Wcol.left.red "Reddish B-B type table"
  button $Wcol.left.blue -text "Blue" -command \
        "edit_cols_update blue $c $wid $ht"
  HelpText $Wcol.left.blue "Bluey/greeny ethereal sort of table"
  button $Wcol.left.rain -text "Rainbow" -command \
        "edit_cols_update rain $c $wid $ht"
  HelpText $Wcol.left.rain "All the colours of the rainbow"
  button $Wcol.left.grey -text "Greyscale" -command \
        "edit_cols_update grey $c $wid $ht"
  HelpText $Wcol.left.grey "Plain, boring grey table"
  button $Wcol.left.inv -text "Invert" -command \
        "edit_cols_update invert $c $wid $ht"
  HelpText $Wcol.left.inv "Turn the table on its head"
  button $Wcol.left.neg -text "Negative" -command \
        "edit_cols_update negative $c $wid $ht"
  HelpText $Wcol.left.neg "Create negative of current colours"
  button $Wcol.left.rot -text "Rotate" -command \
        "edit_cols_update rotate $c $wid $ht"
  HelpText $Wcol.left.rot "Rotate RGB intensities"

  button $Wcol.left.load -text Load -command \
        "edit_cols_update read $c $wid $ht"
  HelpText $Wcol.left.load "Load colour table from file"

  button $Wcol.left.save -text Save -command \
        "edit_cols_update write $c $wid $ht"
  HelpText $Wcol.left.save "Save current colour table to file"

  button $Wcol.left.quit -text "Close" -command {
     ImgExecWait icolour "mode=save"
     ImgExecWaitNoMsg igui "action=update" 
     set Quit yes}
  HelpText $Wcol.left.quit "Close this window"

  pack $Wcol.left.def $Wcol.left.red $Wcol.left.blue $Wcol.left.rain \
       $Wcol.left.grey $Wcol.left.inv $Wcol.left.neg $Wcol.left.rot \
            $Wcol.left.load $Wcol.left.save $Wcol.left.quit -side top  \
                                    -expand 1 -fill both

  Centre_Window $Wcol

  edit_cols_gettab
  edit_cols_setup $c $wid $ht

  tkwait variable Quit
  destroy $Wcol
}
#
proc edit_cols_setup {c wid ht} {

  global coltab 
  global xoffset xrange
  global yconst yscale
  global rednodes greennodes bluenodes

  set ncol 16.0
  set yscale [expr $ht/$ncol]
  set yoffset [expr $yscale/2.0]
  set yconst [expr $ht-$yoffset+$yscale]
  set xoffset [expr $wid/20.0]
  set xrange [expr $wid-2.0*$xoffset]
  set i 0
  foreach rgb {red green blue} {
    incr i
    set nodes ""
    bind $c <ButtonPress-$i> "edit_cols_startchange $c $rgb $i %y"
    foreach col {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16} {
      set x [expr $xoffset+$coltab($col,$rgb)*$xrange]
      set y [expr $ht-($yoffset+($col-1)*$yscale)]
      $c create oval [expr $x-2]  [expr $y+2] [expr $x+2] [expr $y-2] \
              -fill $rgb -outline $rgb -tags $rgb$col
#      $c bind $rgb$col <ButtonPress-$i> "edit_cols_startchange $c $rgb $col $i"
      lappend nodes $x $y
    }
    $c create line 0.0 0.0 0.0 0.0 -fill $rgb -width 2 -tags $rgb
    set done [eval "$c coords $rgb $nodes"]
    if {$rgb=="red"} {
      set rednodes $nodes
    } elseif {$rgb=="green"} {
      set greennodes $nodes
    } elseif {$rgb=="blue"} {
      set bluenodes $nodes
    }
  }

}
#

proc edit_cols_update {table c wid ht} {

  global ColTabDir ColTab coltab xoffset xrange
  global rednodes greennodes bluenodes

#  get application to read or modify colour table
  if {($table == "read") || ($table == "write")} {
    get_coltab
    if {$ColTab != ""} {
      ImgExecWait icolour "mode=$table file=$ColTabDir/$ColTab"
    }
  } else {
    ImgExecWait icolour "mode=$table" 
  }
  edit_cols_gettab

  set ncol 16.0
  set yscale [expr $ht/$ncol]
  set yoffset [expr $yscale/2.0]
  set xoffset [expr $wid/20.0]
  set xrange [expr $wid-2.0*$xoffset]
  set i 0
  foreach rgb {red green blue} {
    incr i
    set nodes ""
    foreach col {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16} {
      set x [expr $xoffset+$coltab($col,$rgb)*$xrange]
      set y [expr $ht-($yoffset+($col-1)*$yscale)]
      set done [eval "$c coords $rgb$col \
              [expr $x-2]  [expr $y+2] [expr $x+2] [expr $y-2]" ]
      lappend nodes $x $y
    }
    set done [eval "$c coords $rgb $nodes"]
    if {$rgb=="red"} {
      set rednodes $nodes
    } elseif {$rgb=="green"} {
      set greennodes $nodes
    } elseif {$rgb=="blue"} {
      set bluenodes $nodes
    }
  }

}

#
proc edit_cols_startchange {c rgb i y} {

  global nbid
  global yconst yscale
  global rednodes greennodes bluenodes
  global coltab

  $c configure -cursor hand2

# which colour to start with?
  set col [expr int(($yconst -$y)/$yscale)]
  if {$col<1} {
    set col 1
  } elseif {$col>16} {
    set col 16
  }

  nbs put $nbid.flag 0
  nbs put $nbid.colour $col
  nbs put $nbid.red$col $coltab($col,red)
  nbs put $nbid.green$col $coltab($col,green)
  nbs put $nbid.blue$col $coltab($col,blue)

# get application to change colour on screen in response to GUI
  ImgExecNoWait icolour "mode=update"

  bind $c <ButtonRelease-$i> "edit_cols_endchange $c $i"
  bind $c <Motion> "edit_cols_change $c $rgb %x %y"


}
#
proc edit_cols_endchange {c i} {

  global nbid
  global rednodes greennodes bluenodes

#  signal to application that activity has stopped
  nbs put $nbid.flag 1

  bind $c <ButtonRelease-$i> {}
  bind $c <Motion> {}
  $c configure -cursor arrow

}
#
proc edit_cols_change {c rgb x y} {

  global nbid
  global coltab 
  global xoffset xrange
  global yconst yscale
  global rednodes greennodes bluenodes

# which colour?
  set col [expr int(0.5 + ($yconst -$y)/$yscale)]
  if {$col<1} {
    set col 1
  } elseif {$col>16} {
    set col 16
  }
  nbs put $nbid.colour $col

# update colour table
  set xx [expr ($x-$xoffset)/$xrange]
  if {$xx>1.0} {
    set xx 1.0
  } elseif {$xx<0.0} {
    set xx 0.0
  }
  set coltab($col,$rgb) [string range $xx 0 3]
  nbs put $nbid.red$col $coltab($col,red)
  nbs put $nbid.green$col $coltab($col,green)
  nbs put $nbid.blue$col $coltab($col,blue)

  set ix [expr ($col-1)*2]
  set iy [expr $ix+1]
  if {$rgb=="red"} {
    set y [lindex $rednodes $iy]
    set rednodes [lreplace $rednodes $ix $iy $x $y]
    set nodes $rednodes
  } elseif {$rgb=="green"} {
    set y [lindex $greennodes $iy]
    set greennodes [lreplace $greennodes $ix $iy $x $y]
    set nodes $greennodes
  } elseif {$rgb=="blue"} {
    set y [lindex $bluenodes $iy]
    set bluenodes [lreplace $bluenodes $ix $iy $x $y]
    set nodes $bluenodes
  }

  set done [eval "$c coords $rgb$col \
            [expr $x-2]  [expr $y+2] [expr $x+2] [expr $y-2]" ]
  set done [eval "$c coords $rgb $nodes"]

  update idletasks


}

#
proc edit_cols_gettab {} {

  global nbid
  global coltab

# get icolour to read colour table from GCB
  ImgExecWait icolour "mode=load" 

# get individual colours from table
  for {set icol 1} {$icol<=16} {incr icol 1} {
    set coltab($icol,red) [nbs get $nbid.red$icol]
    set coltab($icol,green) [nbs get $nbid.green$icol]
    set coltab($icol,blue) [nbs get $nbid.blue$icol]
  }

}
#
#
#
#
#
#
proc get_coltab {} {


  global Wfind
  set Wfind .cfind_dialogue
  toplevel $Wfind -class Dialog
  wm title $Wfind "Find File"
  wm iconname $Wfind Find

  global ColTab
  global Filter
  global Fok
  global CurrDir
  global ColTabDir ColTab ColTabUseFav

  if {$ColTabUseFav == 0} {
    set ColTabDir $CurrDir
  } else {    
    cd $ColTabDir
  }   
  set ColTabDir [pwd]
  set ColTabDir $ColTabDir
  set ColTab ""

  pack [frame $Wfind.top] [frame $Wfind.bot] -side top -padx 2m -pady 2m
  pack [frame $Wfind.bot.left] [frame $Wfind.bot.right] -side left -padx 2m -pady 2m
  pack [frame $Wfind.bot.right.buttons] [frame $Wfind.bot.right.filter] -side top

  pack [frame $Wfind.bot.left.file] [frame $Wfind.bot.left.choose]
  pack [frame $Wfind.bot.left.dir1] -anchor nw -side top
  pack [frame $Wfind.bot.left.dir2]  -side top 

  label $Wfind.top.lbl -text "Directory:"
  label $Wfind.top.dir -textvariable ColTabDir
  pack $Wfind.top.lbl $Wfind.top.dir -side top -pady 1m

  label $Wfind.bot.left.file.lbl -text "Filename:"
  entry $Wfind.bot.left.file.name -width 22 -textvariable ColTab

  pack $Wfind.bot.left.file.lbl -side top -anchor nw 
  pack $Wfind.bot.left.file.name -side top -anchor nw

  listbox $Wfind.bot.left.choose.box \
           -yscrollcommand "$Wfind.bot.left.choose.scroll set"
  scrollbar $Wfind.bot.left.choose.scroll -command "$Wfind.bot.left.choose.box yview"

  pack $Wfind.bot.left.choose.box $Wfind.bot.left.choose.scroll -side left -fill y

  HelpText $Wfind.bot.left.choose.box "Click to select file"

  set Filter "*.act"
  foreach i [lsort [glob -nocomplain $Filter]] {
     $Wfind.bot.left.choose.box insert end $i
  }

  bind $Wfind.bot.left.choose.box <ButtonRelease-1> {set ColTab [selection get]}



  label $Wfind.bot.left.dir1.lbl -text "Directory:"
  pack $Wfind.bot.left.dir1.lbl -side left -anchor w

  menubutton $Wfind.bot.left.dir1.opt -text "Options" -relief raised \
                    -menu [set m $Wfind.bot.left.dir1.opt.menu]
  menu $m
  $m add command -label "Set as default colour table directory" \
       -command "UpdateSetting ColTabDir $ColTabDir"
  $m add separator
  $m add radiobutton -label "Always start with current directory" \
         -variable ColTabUseFav -value 0
  $m add radiobutton -label "Always start with default directory" \
         -variable ColTabUseFav -value 1
  pack $Wfind.bot.left.dir1.opt -side right -fill x

  listbox $Wfind.bot.left.dir2.box \
           -yscrollcommand "$Wfind.bot.left.dir2.scroll set"
  scrollbar $Wfind.bot.left.dir2.scroll -command "$Wfind.bot.left.dir2.box yview"

  pack $Wfind.bot.left.dir2.box $Wfind.bot.left.dir2.scroll -side left -fill y

  HelpText $Wfind.bot.left.dir2.box "Click to select directory"

  $Wfind.bot.left.dir2.box insert end "../"
  foreach i [lsort [glob -nocomplain */]] {
     $Wfind.bot.left.dir2.box insert end $i
  }

  bind $Wfind.bot.left.dir2.box <ButtonRelease-1> {
                                    set newdir [selection get]
                                    cd $newdir
                                    set CurrDir [pwd]
                                    set n [$Wfind.bot.left.dir2.box size]
                                    $Wfind.bot.left.dir2.box delete 0 $n
                                    $Wfind.bot.left.dir2.box insert end "../" 
                                    foreach i [lsort [glob -nocomplain */]] {
                                    $Wfind.bot.left.dir2.box insert end $i
                                    }
                                    $Wfind.bot.right.buttons.filter invoke
                                   }



  pack [frame $Wfind.bot.right.filter.f1] [frame $Wfind.bot.right.filter.f2] \
                                                            -side top

  label $Wfind.bot.right.filter.f1.lbl -text "Filter:"
  entry $Wfind.bot.right.filter.f1.name -width 22 -textvariable Filter

  pack $Wfind.bot.right.filter.f1.lbl -side top -anchor nw 
  pack $Wfind.bot.right.filter.f1.name -side top -anchor nw

  listbox $Wfind.bot.right.filter.f2.box -height 5 \
       -yscrollcommand "$Wfind.bot.right.filter.f2.scroll set" 
  scrollbar $Wfind.bot.right.filter.f2.scroll \
            -command "$Wfind.bot.right.filter.f2.box yview"

  set flist {"*.act" "*.sdf" "*.*"}

  foreach i $flist {
    $Wfind.bot.right.filter.f2.box insert end $i
  }

  pack $Wfind.bot.right.filter.f2.box \
          $Wfind.bot.right.filter.f2.scroll -side left -fill y

  HelpText $Wfind.bot.right.filter.f2.box "Click to select filter"


                                 
  bind $Wfind.bot.right.filter.f2.box <ButtonRelease-1> {
                                    set Filter [selection get]
                                    set n [$Wfind.bot.left.choose.box size]
                                    $Wfind.bot.left.choose.box delete 0 $n
                                    foreach i [lsort\
                                       [glob -nocomplain $Filter]] {
                                      $Wfind.bot.left.choose.box insert end $i
                                    }

                                   }


  button $Wfind.bot.right.buttons.ok -text Ok -command { set Fok 1}

  button $Wfind.bot.right.buttons.cancel -text Cancel -command {set Fok 999}

  button $Wfind.bot.right.buttons.filter -text Filter -command {
                                    set n [$Wfind.bot.left.choose.box size]
                                    $Wfind.bot.left.choose.box delete 0 $n
                                    foreach i [lsort\
                                       [glob -nocomplain $Filter]] {
                                      $Wfind.bot.left.choose.box insert end $i
                                    }
                                   }

  pack $Wfind.bot.right.buttons.ok $Wfind.bot.right.buttons.cancel \
        $Wfind.bot.right.buttons.filter \
                    -side top -pady 2m  -anchor n -fill x -expand 1

  Centre_Window $Wfind

  bind $Wfind <Return> {$Wfind.bot.right.buttons.ok flash
                    $Wfind.bot.right.buttons.ok invoke
                   }

  set oldFocus [focus]

  focus $Wfind.bot.left.file.name


  tkwait variable Fok

  monitor_standard

  UpdateSetting ColTabUseFav $ColTabUseFav
  destroy $Wfind
  if {($CurrDir != "") && ($CurrDir != " ")} {
    cd $CurrDir
  }
  focus $oldFocus
}
#
#
