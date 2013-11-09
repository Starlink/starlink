#
proc HelpControl {name element op} {
  global ContextHelp HelpString Waiting
  if {$Waiting==0} {
    set HelpString $ContextHelp
  }
}

proc HelpText {w txt} {
  global ContextHelp
  global Txt
  set Txt $txt
    bind $w <Enter> "set ContextHelp \"$Txt\""
  bind $w <Leave> {set ContextHelp " "}
}
#
#
#
proc guide_setup {w} {

  global Browser BrowseOk
  global W
  set W $w

  toplevel $w -class Dialog
  wm title $w "On-line Guide Setup"

  set text "Isys needs to know how to start Netscape.  Enter the command \
 that you normally use to run Netscape"

  pack [frame $w.top] [frame $w.mid] [frame $w.bot] -side top

  text $w.top.text -width 30 -height 5  -wrap word
  $w.top.text insert end $text
  pack $w.top.text

  entry $w.mid.cmd -width 30 -textvariable Browser
  pack $w.mid.cmd

  button $w.bot.ok -text Ok -command {set Browser [string trim $Browser]
                                      set Browser [string trim $Browser &]
                                      set Browser [string trim $Browser]
                                       UpdateSetting Browser $Browser
                                       set BrowseOk 1
                                       destroy $W
  }

  bind $w <Return> {$W.bot.ok flash
                    $W.bot.ok invoke
  }

  pack $w.bot.ok

  focus $w.mid.cmd

  Centre_Window $w
}
#
#
#
proc online_guide {w} {

  global Browser BrowseOk
  global AST_ROOT

  if {$Browser == " "} {
    guide_setup .guide_setup
    tkwait variable BrowseOk
  }


  toplevel $w
  label $w.text -text "Loading Netscape...."
  pack $w.text  -side left -padx 2m -pady 2m
  Centre_Window $w
  update idletasks
  catch {exec $Browser $AST_ROOT/docs/Isys/index.html &}
  after 6000
  destroy $w
}
#
#
#

proc help_about {w} {


#  create display window
  toplevel $w -class Dialog -background lightyellow
  wm title $w "Help About"
  wm iconname $w About


  proc get_about {t} {

    global AboutFile

    set f [open $AboutFile] 
    while {![eof $f]} {
      $t insert end [read $f 1000]
    }
    close $f
  }

  pack [frame $w.top -background lightyellow] \
       [frame $w.bot -background lightyellow] -side top

  text $w.top.text -width 40 -height 15 -wrap word \
                              -background lightyellow 
  get_about $w.top.text
  pack $w.top.text


  button $w.bot.close -text Close -background lightyellow -command \
                                                            "destroy $w"

  pack $w.bot.close -side left 

  Centre_Window $w


}
#
#
#
proc show_tips {w} {

  global ShowTips TipNum TipFile

#  create display window
  toplevel $w -class Dialog -background lightyellow
  wm title $w "Tips"
  wm iconname $w Tips


  proc get_tip {t} {

    global TipFile TipNum

    set nread 0
    set control 0
    set f [open $TipFile] 
    while {$control == 0} {
      set line [gets $f]
      if {$line != ""} {
        if {$nread == 0} {
          set first $line
        }
        incr nread
        if {$nread == $TipNum} {
          set control 1
          incr TipNum
        }
      } else {
        set control 2
        set TipNum 2
        set line $first
      }
    }
    close $f
    UpdateSetting TipNum $TipNum
    $t delete 1.0 end
    $t insert end $line
  }

  pack [frame $w.top -background lightyellow] \
       [frame $w.mid -background lightyellow] \
       [frame $w.bot -background lightyellow] -side top

  pack [text $w.top.text -width 30 -height 10 -wrap word \
                              -background lightyellow ]

  label $w.mid.lbl -text "Show tips on startup?" -background lightyellow
  checkbutton $w.mid.chk -variable ShowTips -onvalue 1 -offvalue 0 \
                                           -background lightyellow
  pack $w.mid.lbl $w.mid.chk -side left

  button $w.bot.next -text Next -command "get_tip $w.top.text" \
                            -background lightyellow

  button $w.bot.close -text Close -background lightyellow -command \
                                                            "destroy $w"

  pack $w.bot.next $w.bot.close -side left -pady 1m

  Centre_Window $w

  get_tip $w.top.text

}
#

