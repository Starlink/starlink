proc cred4Display {} {

# Get some global values
    global cgs4drXopts
    global Cred4Widgets
    global Cred4NoticeBoard
    global cgs4drBitmaps
    global cgs4drHtml

# Use double width for bitmap checkbutton
    set bw [expr $cgs4drXopts(xCheckbutton.width) * 2]

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Reduced Data Display Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout (int, obs, grp, spc)
    set top [frame $frame.top -relief sunken -bd 2]
    pack $top -fill x -expand yes
    set tl0 [label $top.tl0 -text "Plot Reduced"]
    set ir0 [radiobutton $top.ir0 -text "Integrations" -variable Cred4Widgets(DTYPE) -value INT]
    set or0 [radiobutton $top.or0 -text "Observations" -variable Cred4Widgets(DTYPE) -value OBS]
    set gr0 [radiobutton $top.gr0 -text "Groups"       -variable Cred4Widgets(DTYPE) -value GRP]
    set sr0 [radiobutton $top.sr0 -text "Spectra"      -variable Cred4Widgets(DTYPE) -value SPC]
    pack $tl0 -side left -fill x
    pack $sr0 $gr0 $or0 $ir0 -side right -fill x
    bind $tl0 <Button-2> "cred4Update cred4Display NONE"
    bind $ir0 <Button-2> "cred4Update cred4Display NONE"
    bind $or0 <Button-2> "cred4Update cred4Display NONE"
    bind $gr0 <Button-2> "cred4Update cred4Display NONE"
    bind $sr0 <Button-2> "cred4Update cred4Display NONE"
    bind $tl0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
    bind $ir0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
    bind $or0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
    bind $gr0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
    bind $sr0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"

# Create a flip out frame
    set flip [frame $frame.flip -relief sunken -bd 2]
    pack $flip -fill x -expand yes

# Set flip out frame for integrations
    set ifliptop [frame $flip.ifliptop]
    pack $ifliptop -fill x -expand y
    set ifr 0
    while {$ifr <= 8} {
      set iflip($ifr) [frame $ifliptop.iflip$ifr -relief sunken -bd 2]
      pack $iflip($ifr) -side top -fill x -expand y
      set ic($ifr) [checkbutton $iflip($ifr).ic$ifr -bitmap @$cgs4drBitmaps/port$ifr.xbm -variable Cred4Widgets(IP$ifr) -width $bw]
      set il($ifr) [label $iflip($ifr).il$ifr -text "Overgraph Command:"]
      set Cred4Widgets(DIS_IP$ifr) [entry $iflip($ifr).ie$ifr -width 60]
      pack $ic($ifr) -in $iflip($ifr) -side left -fill x -expand yes
      pack $Cred4Widgets(DIS_IP$ifr) $il($ifr) -in $iflip($ifr) -side right -fill x -expand y
      $Cred4Widgets(DIS_IP$ifr) delete 0 end
      bind $ic($ifr) <Button-2> "cred4Update cred4Display DIS_IP$ifr"
      bind $ic($ifr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $il($ifr) <Button-2> "cred4Update cred4Display ALL"
      bind $il($ifr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $Cred4Widgets(DIS_IP$ifr) <Button-2> "cred4Update cred4Display DIS_IP$ifr"
      bind $Cred4Widgets(DIS_IP$ifr) <Double-Button-2> "$Cred4Widgets(DIS_IP$ifr) delete 0 end"
      bind $Cred4Widgets(DIS_IP$ifr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      set istring [string trim [string toupper [nbs get ${Cred4NoticeBoard}.display.int_p$ifr]]]
      set Cred4Widgets(IP$ifr) 0
      if {[string range $istring 0 8]=="OVERGRAPH"} {
        set Cred4Widgets(IP$ifr) 1
        $Cred4Widgets(DIS_IP$ifr) insert 0 $istring
      } elseif {[string range $istring 0 2]=="YES"} {
        set Cred4Widgets(IP$ifr) 1
      }
      incr ifr
    }

# Set flip out frame for observations
    set ofliptop [frame $flip.ofliptop]
    pack $ofliptop -fill x -expand y
    set ofr 0
    while {$ofr <= 8} {
      set oflip($ofr) [frame $ofliptop.oflip$ofr -relief sunken -bd 2]
      pack $oflip($ofr) -side top  -fill x -expand y
      set oc($ofr) [checkbutton $oflip($ofr).oc$ofr -bitmap @$cgs4drBitmaps/port$ofr.xbm -variable Cred4Widgets(OP$ofr) -width $bw]
      set ol($ofr) [label $oflip($ofr).ol$ofr -text "Overgraph Command:"]
      set Cred4Widgets(DIS_OP$ofr) [entry $oflip($ofr).oe$ofr -width 60]
      pack $oc($ofr) -in $oflip($ofr) -side left -fill x -expand yes
      pack $Cred4Widgets(DIS_OP$ofr) $ol($ofr) -in $oflip($ofr) -side right -fill x -expand y
      $Cred4Widgets(DIS_OP$ofr) delete 0 end
      bind $oc($ofr) <Button-2> "cred4Update cred4Display DIS_OP$ofr"
      bind $oc($ofr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $ol($ofr) <Button-2> "cred4Update cred4Display ALL"
      bind $ol($ofr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $Cred4Widgets(DIS_OP$ofr) <Button-2> "cred4Update cred4Display DIS_OP$ofr"
      bind $Cred4Widgets(DIS_OP$ofr) <Double-Button-2> "$Cred4Widgets(DIS_OP$ofr) delete 0 end"
      bind $Cred4Widgets(DIS_OP$ofr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      set ostring [string trim [string toupper [nbs get ${Cred4NoticeBoard}.display.obs_p$ofr]]]
      set Cred4Widgets(OP$ofr) 0
      if {[string range $ostring 0 8]=="OVERGRAPH"} {
        set Cred4Widgets(OP$ofr) 1
        $Cred4Widgets(DIS_OP$ofr) insert 0 $ostring
      } elseif {[string range $ostring 0 2]=="YES"} {
        set Cred4Widgets(OP$ofr) 1
      }
      incr ofr
    }

# Set flip out frame for groups
    set gfliptop [frame $flip.gfliptop]
    pack $gfliptop -fill x -expand y
    set gfr 0
    while {$gfr <= 8} {
      set gflip($gfr) [frame $gfliptop.gflip$gfr -relief sunken -bd 2]
      pack $gflip($gfr) -side top  -fill x -expand y
      set gc($gfr) [checkbutton $gflip($gfr).gc$gfr -bitmap @$cgs4drBitmaps/port$gfr.xbm -variable Cred4Widgets(GP$gfr) -width $bw]
      set gl($gfr) [label $gflip($gfr).gl$gfr -text "Overgraph Command:"]
      set Cred4Widgets(DIS_GP$gfr) [entry $gflip($gfr).ge$gfr -width 60]
      pack $gc($gfr) -in $gflip($gfr) -side left -fill x -expand yes
      pack $Cred4Widgets(DIS_GP$gfr) $gl($gfr) -in $gflip($gfr) -side right -fill x -expand y
      $Cred4Widgets(DIS_GP$gfr) delete 0 end
      bind $gc($gfr) <Button-2> "cred4Update cred4Display DIS_GP$gfr"
      bind $gc($gfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $gl($gfr) <Button-2> "cred4Update cred4Display ALL"
      bind $gl($gfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $Cred4Widgets(DIS_GP$gfr) <Button-2> "cred4Update cred4Display DIS_GP$gfr"
      bind $Cred4Widgets(DIS_GP$gfr) <Double-Button-2> "$Cred4Widgets(DIS_GP$gfr) delete 0 end"
      bind $Cred4Widgets(DIS_GP$gfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      set gstring [string trim [string toupper [nbs get ${Cred4NoticeBoard}.display.grp_p$gfr]]]
      set Cred4Widgets(GP$gfr) 0
      if {[string range $gstring 0 8]=="OVERGRAPH"} {
        set Cred4Widgets(GP$gfr) 1
        $Cred4Widgets(DIS_GP$gfr) insert 0 $gstring
      } elseif {[string range $gstring 0 2]=="YES"} {
        set Cred4Widgets(GP$gfr) 1
      }
      incr gfr
    }

# Set flip out frame for spectra
    set sfliptop [frame $flip.sfliptop]
    pack $sfliptop -fill x -expand y
    set sfr 0
    while {$sfr <= 8} {
      set sflip($sfr) [frame $sfliptop.sflip$sfr -relief sunken -bd 2]
      pack $sflip($sfr) -side top  -fill x -expand y
      set sc($sfr) [checkbutton $sflip($sfr).sc$sfr -bitmap @$cgs4drBitmaps/port$sfr.xbm -variable Cred4Widgets(SP$sfr) -width $bw]
      set sl($sfr) [label $sflip($sfr).sl$sfr -text "Overgraph Command:"]
      set Cred4Widgets(DIS_SP$sfr) [entry $sflip($sfr).se$sfr -width 60]
      pack $sc($sfr) -in $sflip($sfr) -side left -fill x -expand yes
      pack $Cred4Widgets(DIS_SP$sfr) $sl($sfr) -in $sflip($sfr) -side right -fill x -expand y
      $Cred4Widgets(DIS_SP$sfr) delete 0 end
      bind $sc($sfr) <Button-2> "cred4Update cred4Display DIS_SP$sfr"
      bind $sc($sfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $sl($sfr) <Button-2> "cred4Update cred4Display ALL"
      bind $sl($sfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      bind $Cred4Widgets(DIS_SP$sfr) <Button-2> "cred4Update cred4Display DIS_SP$sfr"
      bind $Cred4Widgets(DIS_SP$sfr) <Double-Button-2> "$Cred4Widgets(DIS_SP$sfr) delete 0 end"
      bind $Cred4Widgets(DIS_SP$sfr) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
      set sstring [string trim [string toupper [nbs get ${Cred4NoticeBoard}.display.spc_p$sfr]]]
      set Cred4Widgets(SP$sfr) 0
      if {[string range $sstring 0 8]=="OVERGRAPH"} {
        set Cred4Widgets(SP$sfr) 1
        $Cred4Widgets(DIS_SP$sfr) insert 0 $sstring
      } elseif {[string range $sstring 0 2]=="YES"} {
        set Cred4Widgets(SP$sfr) 1
      }
      incr sfr
    }

# Trace the DTYPE and flip frames
    global dtypelist
    set dtypelist [list INT OBS GRP SPC]
    global dwidgetlist
    set dwidgetlist [list $ifliptop $ofliptop $gfliptop $sfliptop]
    trace variable Cred4Widgets(DTYPE) w "CNewDisplayType $flip"

# If NONE< default to OBS otherwise re-write same value so trace will work
    if {$Cred4Widgets(DTYPE) == "NONE"} {
      set Cred4Widgets(DTYPE) "OBS"
    } else {
      set Cred4Widgets(DTYPE) $Cred4Widgets(DTYPE)
    }

# Display the widget
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

# Integrations
      set ifr 0
      while {$ifr <= 8} {
        if {$Cred4Widgets(IP$ifr) == 0} {
          nbs put ${Cred4NoticeBoard}.display.int_p$ifr NO
        } else {
          set istring [string toupper [string trim [$Cred4Widgets(DIS_IP$ifr) get]]]
          if {$istring == "" || $istring == "OVERGRAPH PORT= CUT=X SPOS= EPOS= COLOUR="} {
            nbs put ${Cred4NoticeBoard}.display.int_p$ifr YES
          } else {
            nbs put ${Cred4NoticeBoard}.display.int_p$ifr $istring
          }
        }
        incr ifr
      }

# Observations
      set ofr 0
      while {$ofr <= 8} {
        if {$Cred4Widgets(OP$ofr) == 0} {
          nbs put ${Cred4NoticeBoard}.display.obs_p$ofr NO
        } else {
          set ostring [string toupper [string trim [$Cred4Widgets(DIS_OP$ofr) get]]]
          if {$ostring == "" || $ostring == "OVERGRAPH PORT= CUT=X SPOS= EPOS= COLOUR="} {
            nbs put ${Cred4NoticeBoard}.display.obs_p$ofr YES
          } else {
            nbs put ${Cred4NoticeBoard}.display.obs_p$ofr $ostring
          }
        }
        incr ofr
      }

# Groups
      set gfr 0
      while {$gfr <= 8} {
        if {$Cred4Widgets(GP$gfr) == 0} {
          nbs put ${Cred4NoticeBoard}.display.grp_p$gfr NO
        } else {
          set gstring [string toupper [string trim [$Cred4Widgets(DIS_GP$gfr) get]]]
          if {$gstring == "" || $gstring == "OVERGRAPH PORT= CUT=X SPOS= EPOS= COLOUR="} {
            nbs put ${Cred4NoticeBoard}.display.grp_p$gfr YES
          } else {
            nbs put ${Cred4NoticeBoard}.display.grp_p$gfr $gstring
          }
        }
        incr gfr
      }

# Spectra
      set sfr 0
      while {$sfr <= 8} {
        if {$Cred4Widgets(SP$sfr) == 0} {
          nbs put ${Cred4NoticeBoard}.display.spc_p$sfr NO
        } else {
          set sstring [string toupper [string trim [$Cred4Widgets(DIS_SP$sfr) get]]]
          if {$sstring == "" || $sstring == "OVERGRAPH PORT= CUT=X SPOS= EPOS= COLOUR="} {
            nbs put ${Cred4NoticeBoard}.display.spc_p$sfr YES
          } else {
            nbs put ${Cred4NoticeBoard}.display.spc_p$sfr $sstring
          }
        }
        incr sfr
      }
    }

# Remove the trace and dialogue box
    trace vdelete Cred4Widgets(DTYPE) w "CNewDisplayType $flip"
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}

proc CNewDisplayType {w name element operation} {
    global $name
    global dtypelist
    global dwidgetlist
    set val [string toupper [string trim [set ${name}($element)]]]
    set i [lsearch -exact $dtypelist $val]
    foreach currentSlave [pack slaves $w] {pack forget $currentSlave}
    pack forget $w
    pack $w -fill x
    set newSlave [lindex $dwidgetlist $i]
    if {$newSlave != ""} {pack $newSlave -in $w -side left}
}
