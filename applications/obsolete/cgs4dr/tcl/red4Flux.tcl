proc red4Flux {taskname} {
#+
# Creates a dialog box for red4 action
#-
    global Red4Widgets
    global cgs4drBitmaps

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Flux Calibrate" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top -relief sunken -bd 2]
    set mid [frame $frame.mid -relief sunken -bd 2]
    set bot [frame $frame.bot -relief sunken -bd 2]
    pack $top $mid $bot -in $frame -side top -expand yes -fill both

    set Red4Widgets(FC_LAB01) [label $top.l0 -text "Filename"]
    set Red4Widgets(FC_ENT01) [entry $top.e0 -width 60]
    pack $Red4Widgets(FC_LAB01) $Red4Widgets(FC_ENT01) -in $top -side left -expand yes -fill x
    $Red4Widgets(FC_ENT01) insert end $Red4Widgets(SP)
    bind $Red4Widgets(FC_LAB01) <Button-2> "red4Update red4Flux ALL"
    bind $Red4Widgets(FC_ENT01) <Button-2> "red4Update red4Flux FC_ENT01"
    bind $Red4Widgets(FC_ENT01) <Double-Button-2> "$Red4Widgets(FC_ENT01)  delete 0 end"

    set umid [frame $mid.um]
    set bmid [frame $mid.bm]
    pack $umid $bmid -in $mid -side top

    set Red4Widgets(FC_LAB02) [label $umid.l1 -text "Input Value/Units" -width 15]
    set Red4Widgets(FC_ENT02) [entry $umid.e1]
    $Red4Widgets(FC_ENT02) insert end $Red4Widgets(FC_IN)
    set ijm [radiobutton $umid.jm -width 28m -height 5m -bitmap @$cgs4drBitmaps/jmag.xbm -variable Red4Widgets(FC_FIN) -value "J"]
    set ihm [radiobutton $umid.hm -width 28m -height 5m -bitmap @$cgs4drBitmaps/hmag.xbm -variable Red4Widgets(FC_FIN) -value "H"]
    set ikm [radiobutton $umid.km -width 28m -height 5m -bitmap @$cgs4drBitmaps/kmag.xbm -variable Red4Widgets(FC_FIN) -value "K"]
    set ilm [radiobutton $umid.lm -width 28m -height 5m -bitmap @$cgs4drBitmaps/lmag.xbm -variable Red4Widgets(FC_FIN) -value "L"]
    set ilpm [radiobutton $umid.lpm -width 28m -height 5m -bitmap @$cgs4drBitmaps/lpmag.xbm -variable Red4Widgets(FC_FIN) -value "L'"]
    pack $Red4Widgets(FC_LAB02) $Red4Widgets(FC_ENT02) $ijm $ihm $ikm $ilm $ilpm -in $umid -side left -padx 1
    bind $Red4Widgets(FC_LAB02) <Button-2> "red4Update red4Flux ALL"
    bind $Red4Widgets(FC_ENT02) <Button-2> "red4Update red4Flux FC_ENT02"
    bind $Red4Widgets(FC_ENT02) <Double-Button-2> "$Red4Widgets(FC_ENT02) delete 0 end"
    bind $ijm <Button-2> "red4Update red4Flux FC_FIN"
    bind $ihm <Button-2> "red4Update red4Flux FC_FIN"
    bind $ikm <Button-2> "red4Update red4Flux FC_FIN"
    bind $ilm <Button-2> "red4Update red4Flux FC_FIN"
    bind $ilpm <Button-2> "red4Update red4Flux FC_FIN"

    set imm [radiobutton $bmid.mm -width 28m -height 5m -bitmap @$cgs4drBitmaps/mmag.xbm \
      -variable Red4Widgets(FC_FIN) -value "M"]
    set inm [radiobutton $bmid.nm -width 28m -height 5m -bitmap @$cgs4drBitmaps/nmag.xbm \
      -variable Red4Widgets(FC_FIN) -value "N"]
    set iwu [radiobutton $bmid.wu -width 28m -height 5m -bitmap @$cgs4drBitmaps/wmu.xbm \
      -variable Red4Widgets(FC_FIN) -value "W/m2/um"]
    set iwh [radiobutton $bmid.wh -width 28m -height 5m -bitmap @$cgs4drBitmaps/wmh.xbm \
      -variable Red4Widgets(FC_FIN) -value "W/m2/Hz"]
    set ier [radiobutton $bmid.er -width 28m -height 5m -bitmap @$cgs4drBitmaps/escu.xbm \
      -variable Red4Widgets(FC_FIN) -value "ergs/s/cm2/um"]
    set imj [radiobutton $bmid.mj -width 28m -height 5m -bitmap @$cgs4drBitmaps/mjy.xbm \
      -variable Red4Widgets(FC_FIN) -value "mJy"]
    pack $imm $inm $iwu $iwh $ier $imj -in $bmid -side left -padx 1
    bind $imm <Button-2> "red4Update red4Flux FC_FIN"
    bind $inm <Button-2> "red4Update red4Flux FC_FIN"
    bind $iwu <Button-2> "red4Update red4Flux FC_FIN"
    bind $iwh <Button-2> "red4Update red4Flux FC_FIN"
    bind $ier <Button-2> "red4Update red4Flux FC_FIN"
    bind $imj <Button-2> "red4Update red4Flux FC_FIN"

    set Red4Widgets(FC_LAB03) [label $bot.l2 -text "Output Units"]
    set owu [radiobutton $bot.wu -width 28m -height 5m -bitmap @$cgs4drBitmaps/wmu.xbm \
      -variable Red4Widgets(FC_FOUT) -value "W/m2/um"]
    set owh [radiobutton $bot.wh -width 28m -height 5m -bitmap @$cgs4drBitmaps/wmh.xbm \
      -variable Red4Widgets(FC_FOUT) -value "W/m2/Hz"]
    set oer [radiobutton $bot.er -width 28m -height 5m -bitmap @$cgs4drBitmaps/escu.xbm \
      -variable Red4Widgets(FC_FOUT) -value "ergs/s/cm2/um"]
    set omj [radiobutton $bot.mj -width 28m -height 5m -bitmap @$cgs4drBitmaps/mjy.xbm \
      -variable Red4Widgets(FC_FOUT) -value "mJy"]
    pack $Red4Widgets(FC_LAB03) $owu $owh $oer $omj -in $bot -side left -padx 1 -expand yes -fill x
    bind $Red4Widgets(FC_LAB03) <Button-2> "red4Update red4Flux ALL"
    bind $owu <Button-2> "red4Update red4Flux FC_FOUT"
    bind $owh <Button-2> "red4Update red4Flux FC_FOUT"
    bind $oer <Button-2> "red4Update red4Flux FC_FOUT"
    bind $omj <Button-2> "red4Update red4Flux FC_FOUT"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(SP) [string trim [$Red4Widgets(FC_ENT01) get]]
      if {$Red4Widgets(SP)=="" || $Red4Widgets(SP)==$Red4Widgets(DSP)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Flux error : A dataset has not been specified properly!"
      } else {
        set out $Red4Widgets(SP)_fc
        set Red4Widgets(FC_IN) [string trim [$Red4Widgets(FC_ENT02) get]]
        set band $Red4Widgets(FC_FIN)
        set iun $Red4Widgets(FC_FIN)
        if {$band=="J" || $band=="H" || $band=="K" || $band=="L" || $band=="L'" || $band=="M" || $band=="N"} {
          set iun "mag"
        } else {
          set band "Flux"
        }
        set uout $Red4Widgets(FC_FOUT)
        set message "Flux calibrating $Red4Widgets(SP) into $out"
        cgs4drInform $taskname $message
        set param "input=$Red4Widgets(SP) output=$out flux=$Red4Widgets(FC_IN) band=$band input_units=$iun output_units=$uout"
        $taskname obey flux_calibrate "$param" -inform "cgs4drInform $taskname %V"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
