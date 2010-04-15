proc cgs4drPokenbs {taskname} {
#+
# Puts value into nbs
#-
    global Cred4NoticeBoard
    global C4UserNb
    global Cred4Task
    global P4NoticeBoard
    global P4UserNb
    global P4Task
    global cgs4drHtml

# Set default
    if {[string match $Cred4Task $taskname]} {
      set noticeboard $C4UserNb
    } elseif {[string match $P4Task $taskname]} {
      set noticeboard $P4UserNb
    } else {
      set noticeboard ""
    }

# Create a dialog box
    if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
    set frame [dialogStart .cgs4drDialogue "NBS Poke Value" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cgs4drDialogue config -cursor {arrow green black}
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -side top -fill both -expand yes
    set ilab [label $top.ilab -text Item]
    set item [entry $top.item -width 50]
    pack $item -in $top -side right
    pack $ilab -in $top -side left
    $item insert 0 $noticeboard
    set vlab [label $bot.vlab -text Value]
    set valu [entry $bot.valu -width 50]
    pack $vlab -in $bot -side left
    pack $valu -in $bot -side right
    $valu delete 0 end

# Set some bindings
    if {[string match $Cred4Task $taskname]} {
      bind $ilab <Button-2> "$item delete 0 end; $item insert 0 $Cred4NoticeBoard; $valu delete 0 end"
      bind $vlab <Button-2> "$item delete 0 end; $item insert 0 $Cred4NoticeBoard; $valu delete 0 end"
      bind $valu <Button-2> "$valu delete 0 end"
      bind $valu <Double-Button-2> "$valu delete 0 end"
      bind $item <Button-2> "$item delete 0  end; $item insert 0 $Cred4NoticeBoard"
      bind $item <Double-Button-2> "$item delete 0  end"
      set noticeboard $C4UserNb
    } elseif {[string match $P4Task $taskname]} {
      bind $ilab <Button-2> "$item delete 0 end; $item insert 0 $P4NoticeBoard; $valu delete 0 end"
      bind $vlab <Button-2> "$item delete 0 end; $item insert 0 $P4NoticeBoard; $valu delete 0 end"
      bind $valu <Button-2> "$valu delete 0 end"
      bind $valu <Double-Button-2> "$valu delete 0 end"
      bind $item <Button-2> "$item delete 0  end; $item insert 0 $P4NoticeBoard"
      bind $item <Double-Button-2> "$item delete 0  end"
      set noticeboard $P4UserNb
    } else {
      bind $ilab <Button-2> "$item delete 0 end; $valu delete 0 end"
      bind $vlab <Button-2> "$item delete 0 end; $valu delete 0 end"
      bind $valu <Button-2> "$valu delete 0 end"
      bind $valu <Double-Button-2> "$valu delete 0 end"
      bind $item <Button-2> "$item delete 0  end"
      bind $item <Double-Button-2> "$item delete 0  end"
      set noticeboard ""
    }
    bind $ilab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PokeNbsBox1.html"
    bind $vlab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PokeNbsBox1.html"
    bind $valu <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PokeNbsBox1.html"
    bind $item <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PokeNbsBox1.html"

# If user presses OK, send it to the task
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set nb [string trim [$item get]]
      set vb [string trim [$valu get]]
      if {$nb=="" || $vb==""} {
        cgs4drClear $taskname
        cgs4drInform $taskname "cgs4drPokenbs error : Noticeboard or value incorrectly specified!"
      } else {

#     Set default
        if {[string match $Cred4Task $taskname]} {
          set C4UserNb $nb
        } elseif {[string match $P4Task $taskname]} {
          set P4UserNb $nb
        }

        set status [catch {nbs put $nb $vb}]
        if {$status!=0} {
          cgs4drClear $taskname
          cgs4drInform $taskname "cgs4drPokenbs error : Cannot put incorrect value or type!"
        }
      }
    }

#  Destroy box
    cgs4drCursor arrow green black
    destroy .cgs4drDialogue
}
