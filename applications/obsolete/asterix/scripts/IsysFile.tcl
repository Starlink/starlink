#
proc find_file {w} {

  toplevel $w -class Dialog
  wm title $w "Find File"
  wm iconname $w Find

  global Wfind
  set Wfind $w

  global FileName FileType
  global Filter
  global Fok
  global CurrDir

  pack [frame $w.top] [frame $w.bot] -side top -padx 2m -pady 2m
  pack [frame $w.bot.left] [frame $w.bot.right] -side left -padx 2m -pady 2m
  pack [frame $w.bot.right.buttons] [frame $w.bot.right.filter] -side top

  pack [frame $w.bot.left.file] [frame $w.bot.left.choose]
  pack [frame $w.bot.left.dir1] -anchor nw -side top
  pack [frame $w.bot.left.dir2]  -side top 

  label $w.top.lbl -text "Current directory:"
  label $w.top.dir -textvariable CurrDir
  set CurrDir [pwd]/
  pack $w.top.lbl $w.top.dir -side top -pady 1m

  label $w.bot.left.file.lbl -text "Filename:"
  entry $w.bot.left.file.name -width 22 -textvariable FileName

  pack $w.bot.left.file.lbl -side top -anchor nw 
  pack $w.bot.left.file.name -side top -anchor nw

  listbox $w.bot.left.choose.box \
           -yscrollcommand "$w.bot.left.choose.scroll set"
  scrollbar $w.bot.left.choose.scroll -command "$w.bot.left.choose.box yview"

  pack $w.bot.left.choose.box $w.bot.left.choose.scroll -side left -fill y

  HelpText $w.bot.left.choose.box "Click to select file"


  foreach i [lsort [glob -nocomplain $Filter]] {
     $w.bot.left.choose.box insert end $i
  }

  bind $w.bot.left.choose.box <ButtonRelease-1> {set FileName [selection get]}
  bind $w.bot.left.choose.box <Double-Button-1> {set FileName [selection get]
                                             set Fok 1
                                            }

  label $w.bot.left.dir1.lbl -text "Directory:"
  pack $w.bot.left.dir1.lbl -side top -anchor w -fill x

  listbox $w.bot.left.dir2.box \
           -yscrollcommand "$w.bot.left.dir2.scroll set"
  scrollbar $w.bot.left.dir2.scroll -command "$w.bot.left.dir2.box yview"

  pack $w.bot.left.dir2.box $w.bot.left.dir2.scroll -side left -fill y

  HelpText $w.bot.left.dir2.box "Click to select directory"

  $w.bot.left.dir2.box insert end "../"
  foreach i [lsort [glob -nocomplain */]] {
     $w.bot.left.dir2.box insert end $i
  }

  bind $w.bot.left.dir2.box <ButtonRelease-1> {
                                    set newdir [selection get]
                                    cd $newdir
                                    set CurrDir [pwd]/
                                    set n [$Wfind.bot.left.dir2.box size]
                                    $Wfind.bot.left.dir2.box delete 0 $n
                                    $Wfind.bot.left.dir2.box insert end "../" 
                                    foreach i [lsort [glob -nocomplain */]] {
                                    $Wfind.bot.left.dir2.box insert end $i
                                    }
                                    $Wfind.bot.right.buttons.filter invoke
                                   }



  pack [frame $w.bot.right.filter.f1] [frame $w.bot.right.filter.f2] \
                                                            -side top

  label $w.bot.right.filter.f1.lbl -text "Filter:"
  entry $w.bot.right.filter.f1.name -width 22 -textvariable Filter

  pack $w.bot.right.filter.f1.lbl -side top -anchor nw 
  pack $w.bot.right.filter.f1.name -side top -anchor nw

  listbox $w.bot.right.filter.f2.box -height 5 \
       -yscrollcommand "$w.bot.right.filter.f2.scroll set" 
  scrollbar $w.bot.right.filter.f2.scroll \
            -command "$w.bot.right.filter.f2.box yview"

#  set filters for each file type
  if {$FileType == "IMAGE"} {
    set flist {"*.sdf" "i*.sdf" "*.fits" "i*.fits" "*.*"}
  } elseif {$FileType == "ONED"} {
    set flist {"*.sdf" "*.fits" "*.*"}
  } elseif {$FileType == "ARD"} {
    set flist {"*.ard" "*.*"}
  } elseif {$FileType == "POS"} {
    set flist {"*.pos" "*.lis" "*.txt" "*.TXT" "*.*"}
  }

  foreach i $flist {
    $w.bot.right.filter.f2.box insert end $i
  }

  pack $w.bot.right.filter.f2.box \
          $w.bot.right.filter.f2.scroll -side left -fill y

  HelpText $w.bot.right.filter.f2.box "Click to select filter"


                               
  bind $w.bot.right.filter.f2.box <ButtonRelease-1> {
                                    set Filter [selection get]
                                    set n [$Wfind.bot.left.choose.box size]
                                    $Wfind.bot.left.choose.box delete 0 $n
                                    foreach i [lsort\
                                       [glob -nocomplain $Filter]] {
                                      $Wfind.bot.left.choose.box insert end $i
                                    }

                                   }


  button $w.bot.right.buttons.ok -text Ok -command { set Fok 1}

  button $w.bot.right.buttons.cancel -text Cancel -command {set Fok 999}

  button $w.bot.right.buttons.filter -text Filter -command {
                                    set n [$Wfind.bot.left.choose.box size]
                                    $Wfind.bot.left.choose.box delete 0 $n
                                    foreach i [lsort\
                                       [glob -nocomplain $Filter]] {
                                      $Wfind.bot.left.choose.box insert end $i
                                    }
                                   }

  pack $w.bot.right.buttons.ok $w.bot.right.buttons.cancel \
        $w.bot.right.buttons.filter \
                    -side top -pady 2m  -anchor n -fill x -expand 1

  Centre_Window $w

  bind $w <Return> {$Wfind.bot.right.buttons.ok flash
                    $Wfind.bot.right.buttons.ok invoke
                   }

  set oldFocus [focus]

  focus $w.bot.left.file.name

  tkwait variable Fok

  destroy $w
  focus $oldFocus
}
#
#

#
#
#
proc load_img {w } {

  toplevel $w -class Dialog
  wm title $w "Load Image"
  wm iconname $w Load

  global Top Ok
  set Top $w
  global FileName FileType Filter
  global CurrDir
  global ImageName ImageOpen
  global ImgFilter
  global AutoSaveSettings
  set FileType "IMAGE"
  set Filter $ImgFilter
  set FileName ""
  global nbid

  set Ok 0

  pack [frame $w.top] [frame $w.bot] -side top

  pack [frame $w.top.left] [frame $w.top.right] -side left

  pack [frame $w.top.left.img] [frame $w.top.left.gcb] -side top

  label $w.top.left.img.lbl -text "Filename:"
  entry $w.top.left.img.name -width 30 -textvariable FileName

  pack $w.top.left.img.lbl -side top -anchor nw 
  pack $w.top.left.img.name -side top -anchor nw

 


 
  label $w.top.left.gcb.lbl -text "Load Graphics Control?"

  global gcb
  set gcb yes
  radiobutton $w.top.left.gcb.yes -text Yes -variable gcb -value yes
  radiobutton $w.top.left.gcb.no -text No -variable gcb -value no

  pack $w.top.left.gcb.lbl $w.top.left.gcb.yes \
        $w.top.left.gcb.no -side left  -pady 2m -padx 2m

 
  button $w.bot.ok -text Ok -command {
          $Top configure -cursor watch
          . configure -cursor watch
          set path $CurrDir$FileName
          ImgExecWaitNoMsg icheck "inp=$path"
          set shape [nbs get $nbid.options]
          update idletasks
          set shape [string trim $shape]
          if {$shape == "CUBE"} {
            cube_load .cube_dialogue
          }
          if {$Ok != 999} {
            nbs put $nbid.flag 1
            update idletasks
            ImgExecWait iload "inp=$path gcb=$gcb disp=yes"
            $Top configure -cursor arrow
            . configure -cursor arrow
            set flag [nbs get $nbid.flag]
            update idletasks
            if { $flag == 0 } {
              ImgExecWaitNoMsg istats "suppress=yes"
              ImgExecWaitNoMsg igui "action=update"
              set ImageName $FileName
              wm title . "Isys: $ImageName"
              set Ok 1
            } else {
              set errmsg "Error loading file -see main message box for details"
              Show_Error .error $errmsg
            }
	 }
  }


  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.cancel  \
                    -side left -pady 2m


  button $w.top.right.browse -text "Choose.." -command {
                                    find_file .file_chooser
                                   }
  pack $w.top.right.browse -padx 2m                   


  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }

  Centre_Window $w

  set oldFocus [focus]

  focus $w.top.left.img.name

  tkwait variable Ok

  if {$AutoSaveSettings == 1} {
    UpdateSetting ImgFilter $ImgFilter
  }

  if {$Ok==1} {
    set ImageOpen 1
    monitor_standard
  }

  destroy $w
  focus $oldFocus
}
#
#
proc cube_load {w} {

  toplevel $w -class Dialog
  wm title $w "Cube"
  wm iconname $w Cube

  global Ok
  global nbid
  global cube_opt cube_ok v1 v2
  global rngmin rngmax idxmin idxmax

  proc cube_load_setvals {a b c} {

    global cube_opt v1 v2
    global rngmin rngmax idxmin idxmax

      if {$cube_opt == 1} {
        set v1 ""
        set v2 ""
      } elseif {$cube_opt == 2} {
        set v1 $idxmin
        set v2 $idxmax
      } elseif {$cube_opt == 3} {
        set v1 $rngmin
        set v2 $rngmax
      }

  }


  set idxmin [nbs get $nbid.par_i1]
  set idxmax [nbs get $nbid.par_i2]
  set rngmin [nbs get $nbid.par_r1]
  set rngmax [nbs get $nbid.par_r2]

  pack [frame $w.f1] [frame $w.f2] [frame $w.f3] [frame $w.f4] \
                     -side top -padx 2m -pady 2m


  text $w.f1.text -width 30 -height 5 -wrap word

  $w.f1.text insert end "The dataset is a cube with "
  $w.f1.text insert end "a non-spatial axis of dimension $idxmax "
  $w.f1.text insert end "and axis values ranging from $rngmin to $rngmax"

  pack $w.f1.text

  set cube_opt 1
  trace variable cube_opt w cube_load_setvals
  radiobutton $w.f2.whole -text "Load whole cube" -variable cube_opt \
                                                     -value 1
  radiobutton $w.f2.index -text "Select by index" -variable cube_opt \
                                                     -value 2
  radiobutton $w.f2.value -text "Select by value" -variable cube_opt \
                                                     -value 3
  pack $w.f2.whole $w.f2.index $w.f2.value -side top


  label $w.f3.l1 -text "From:"
  entry $w.f3.v1 -textvariable v1 -width 12
  label $w.f3.l2 -text "To:"
  entry $w.f3.v2 -textvariable v2 -width 12
  pack $w.f3.l1 $w.f3.v1 $w.f3.l2 $w.f3.v2 -side left

  global cube_ok
  button $w.f4.ok -text Ok -command {
      if {$cube_opt == 1} {
        nbs put $nbid.options "WHOLE"
      } elseif {$cube_opt == 2} {
        nbs put $nbid.options "INDEX"
        nbs put $nbid.par_i1 $v1
        nbs put $nbid.par_i2 $v2
      } elseif {$cube_opt == 3} {
        nbs put $nbid.options "VALUE"
        nbs put $nbid.par_r1 $v1
        nbs put $nbid.par_r2 $v2
      }
      set cube_ok 1
  }

  button $w.f4.cancel -text Cancel -command "set cube_ok 999;set Ok 999"

  pack $w.f4.ok $w.f4.cancel -side left


  bind $w <Return> "$w.f4.ok flash;$w.f4.ok invoke"
   

  Centre_Window $w

  tkwait variable cube_ok
  destroy $w

}
#
#
#
proc save_data {w } {

  toplevel $w -class Dialog
  wm title $w "Save to File"
  wm iconname $w Save

  global name
  global Top
  set Top $w

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom

  label $w.top.lbl1 -text "File Name"
  entry $w.top.name -width 20 -textvariable name

  pack $w.top.lbl1 $w.top.name -side left -pady 2m -padx 2m

  global W
  set W $w
  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      ImgExecWait isave "out=$name" 
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      set Ok 1
                                     }

  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.cancel -pady 2m -side left

  set oldFocus [focus]
  focus $w.top.name

  tkwait variable Ok
  destroy $w
  focus $oldFocus
}
#
#
#
#
#
proc load_1D_data {w } {

  toplevel $w -class Dialog
  wm title $w "Load 1D data"
  wm iconname $w Load1D

  global FileName FileType Filter
  global CurrDir
  set FileType "ONED"
  set Filter "*.*"
  set FileName ""
  global Top
  set Top $w

  pack [frame $w.top] [frame $w.bot] -side top -pady 2m
  pack [frame $w.top.left] [frame $w.top.right] -side left

  pack [frame $w.top.left.img] [frame $w.top.left.gcb] -side top

  label $w.top.left.img.lbl -text "Filename:"
  entry $w.top.left.img.name -width 30 -textvariable FileName

  pack $w.top.left.img.lbl $w.top.left.img.name -side top -pady 1m -padx 2m

  label $w.top.left.gcb.lbl -text "Load Graphics Control"

  global gcb
  set gcb no
  radiobutton $w.top.left.gcb.yes -text Yes -variable gcb -value yes
  radiobutton $w.top.left.gcb.no -text No -variable gcb -value no

  pack $w.top.left.gcb.lbl $w.top.left.gcb.yes \
                  $w.top.left.gcb.no -side left  -pady 2m -padx 2m

  global W
  set W $w

  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      set path $CurrDir$FileName
                                      ImgExecWait iload1d \
                                        "inp=$path gcb=$gcb"
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      ImgExecWaitNoMsg igui "action=update"
                                      set Ok 1
                                     }
  button $w.bot.cancel -text Cancel -command {set Ok 999}

  button $w.top.right.browse -text "Choose.." -command {
                                    find_file .file_chooser
                                   }

  pack $w.bot.ok $w.bot.cancel  -side left -pady 2m \
                                                -fill x

  pack $w.top.right.browse -padx 2m

  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }
  Centre_Window $w

  set oldFocus [focus]

  focus $w.top.left.img.name

  tkwait variable Ok

  destroy $w
  focus $oldFocus
}
#
#
#
proc save_1D_data {w } {

  toplevel $w -class Dialog
  wm title $w "Save to File"
  wm iconname $w Save

  global name

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom

  label $w.top.lbl1 -text "File Name"
  entry $w.top.name -width 20 -textvariable name

  pack $w.top.lbl1 $w.top.name -side left -pady 2m -padx 2m

  global W
  set W $w
  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      ImgExecWait isave1d "out=$name" 
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      set Ok 1
                                     }

  bind $w <Return> {$W.bot.ok flash
                    $W.bot.ok invoke
                   }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.cancel -pady 2m -side left

  Centre_Window $w

  set oldFocus [focus]
  focus $w.top.name

  tkwait variable Ok
  destroy $w
  focus $oldFocus
}
#
#
#
#
proc load_ARD_data {w } {

  toplevel $w -class Dialog
  wm title $w "Load ARD data"
  wm iconname $w LoadARD

  global FileName FileType Filter
  global CurrDir
  set FileType "ARD"
  set Filter "*.ard"
  set FileName ""
  global Top
  set Top $w

  pack [frame $w.top] [frame $w.bot] -side top -pady 2m
  pack [frame $w.top.left] [frame $w.top.right] -side left -padx 2m


  label $w.top.left.lbl -text "Filename:"
  entry $w.top.left.name -width 30 -textvariable FileName

  pack $w.top.left.lbl $w.top.left.name -side top -pady 1m -padx 2m \
                               -anchor nw


  global W
  set W $w

  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      set path $CurrDir$FileName
                                      ImgExecWait iregion \
                                        "mode=imp text=$FileName"
                                      ImgExecWaitNoMsg istats "suppress=y"
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      set Ok 1
                                     }
  button $w.bot.cancel -text Cancel -command {set Ok 999}

  button $w.top.right.browse -text "Choose.." -command {
                                                find_file .file_chooser
                                               }

  pack $w.bot.ok $w.bot.cancel  -side left -pady 2m \
                                               -fill x
  pack $w.top.right.browse -padx 2m 


  Centre_Window $w

  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }

  set oldFocus [focus]

  focus $w.top.left.name

  tkwait variable Ok

  destroy $w
  focus $oldFocus
}
#
#
#
proc save_ARD_data {w } {

  toplevel $w -class Dialog
  wm title $w "Save ARD to File"
  wm iconname $w SaveARD

  global FileName

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom

  label $w.top.lbl -text "File Name"
  entry $w.top.name -width 20 -textvariable FileName

  pack $w.top.lbl $w.top.name -side left -pady 2m -padx 2m

  global W
  set W $w
  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      ImgExecWait iregion \
                                          "mode=exp file=$FileName" 
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      set Ok 1
                                     }

  bind $w <Return> {$W.bot.ok flash
                    $W.bot.ok invoke
                   }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.cancel -pady 2m -side left

  set oldFocus [focus]
  focus $w.top.name

  tkwait variable Ok
  destroy $w
  focus $oldFocus
}
#
#
#
#
#
#
#
proc load_pos_list {w } {

  toplevel $w -class Dialog
  wm title $w "Load Position List"
  wm iconname $w LoadPOS

  global FileName FileType Filter
  global CurrDir
  global FilePos
  set FileType "POS"
  set Filter "*.*"
  set FileName ""
  global PosW
  set PosW $w

  pack [frame $w.top] [frame $w.bot] -side top -pady 2m
  pack [frame $w.top.left] [frame $w.top.right] -side left -padx 2m


  label $w.top.left.lbl -text "Filename:"
  entry $w.top.left.name -width 30 -textvariable FileName

  pack $w.top.left.lbl $w.top.left.name -side top  -pady 1m -padx 2m \
                                    -anchor nw


  global W
  set W $w

  button $w.bot.ok -text Ok -command {set done 0
                                      $W configure -cursor watch
                                      . configure -cursor watch
                                      set path $CurrDir$FileName
                                      ImgExecWait iposit \
                                        "mode=ENT list=$FileName"
                                      ImgExecWaitNoMsg iposit \
                                        "mode=SAV file=$FilePos"
                                      $W configure -cursor arrow
                                      . configure -cursor arrow
                                      set Ok 1
                                     }
  button $w.bot.cancel -text Cancel -command {set Ok 999}

  button $w.top.right.browse -text "Choose.." -command {
                                                find_file .file_chooser
                                               }

  pack $w.bot.ok $w.bot.cancel  -side left -pady 2m \
                                   -anchor nw -fill x

  pack $w.top.right.browse -padx 2m 

  Centre_Window $w

  bind $w <Return> {$PosW.bot.ok flash
                    $PosW.bot.ok invoke
                   }

  set oldFocus [focus]

  focus $w.top.left.name

  tkwait variable Ok

  destroy $w
  focus $oldFocus
}
#
