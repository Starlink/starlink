# IsysPssDialog.tcl
#
# This file defines the procedure CLPcalcDialog, which creates a modal
# dialog box for setting the values of those parameters controlling profile
# generation in the CLPLOT task

# IsysPssDialog
#
# This procedure creates a modal dialog box with controls for setting the
# the extent and spacing of the radial samples in CLPLOT.
#
# Arguments:
#
# w -           Window to use for dialog top-level.
#

proc IsysPssClose {} {
  global PSS
  if { $PSS(loaded_mono) } {
    src kill
    }
  }

proc IsysPssClear {} {
  global PSS
  set PSS(inp) 		im
  set PSS(bgnd) 	none
  set PSS(slice) 	"*:*,*:*"
  set PSS(region)       0
  set PSS(map) 		"!"
  set PSS(out)	 	srclist
  set PSS(ssub) 	"!"
  set PSS(psf) 	        auto
  set PSS(mask) 	auto
  set PSS(aux) 	        auto
  set PSS(psfcon) 	auto
  set PSS(psfpix) 	"68%"
  set PSS(sigmin) 	5
  set PSS(ferl) 	"1 sigma"
  set PSS(perl) 	"90.0%"
  set PSS(loaded_mono)  0
  set PSS(mark)         1
  set PSS(mark_n)       y
  set PSS(dump)         0
  set PSS(dump_d)       TERMINAL
  }

proc IsysPssHandleOutput {w txt} {
  global msg_data

  $w configure -state normal
  $w insert end "$txt\n"
  $w yview moveto 1
  $w configure -state disabled
  }


proc IsysPssDialog {w} {

    global PSS IsysBGM Mean
    global isyspss_priv
    global env

# mark as unfinished
    set PSS(finished) 0

# load task and wait for communication path to be established
    if { ! $PSS(loaded_mono) } {
      adamtask src $env(AST_BIN)/src_mono
      while {[src path]==0} {
        after 100
        }
      set PSS(loaded_mono) 1
      }

# Can we use the background modeller model?
    if { ($PSS(bgnd) == "none") && [info exists IsysBGM] } {
      if { $IsysBGM(ExportModel) != "none" } {
        set PSS(bgnd) $IsysBGM(ExportModel)
        }
    } elseif { $PSS(bgnd) == "none" } {
      set PSS(bgnd) $Mean
      }

 
# Create the top level window.
    toplevel $w -class Dialog -bd 10
    wm title $w "Isys PSS Control"
    wm iconname $w PSS
    wm transient $w .

# Create and pack three frames one above the other and divide the top frame
# into two side by side.
    frame $w.files -relief groove -bd 2
    frame $w.middle 
    frame $w.middle.psf -relief groove -bd 2
    frame $w.middle.fit -relief groove -bd 2
    frame $w.post
    frame $w.post.mark -bd 2
    frame $w.post.disp -bd 2

# Make context sensitive help
    MakeHelpWindow $w.help

    pack $w.middle.psf $w.middle.fit -side left -fill x -fill y \
            -ipadx 5 -ipady 5
    pack $w.post.mark $w.post.disp -side left -fill x -fill y \
            -ipadx 5 -ipady 5
    frame $w.ctrl -relief groove -bd 2

# Create filenames editing window
    label $w.files.lab -text "Input and Output Files" -relief raised
    frame $w.files.inp
      label $w.files.inp.lab -text "Input: "
      entry $w.files.inp.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(inp)
      pack $w.files.inp.lab -side left 
      pack $w.files.inp.dat -side right
      SetHelpText $w.files.inp "Specify the name of the image to be searched"

    frame $w.files.bg
      label $w.files.bg.lab -text "Background: "
      entry $w.files.bg.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(bgnd)
      pack $w.files.bg.lab -side left 
      pack $w.files.bg.dat -side right
      SetHelpText $w.files.bg "Specify the background value or filename"

    frame $w.files.slice
      label $w.files.slice.lab -text "Search region: "
      entry $w.files.slice.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(slice)
      pack $w.files.slice.lab -side left 
      pack $w.files.slice.dat -side right
      SetHelpText $w.files.slice "Rectangular sub-region of image to search"

    frame $w.files.sm
      label $w.files.sm.lab -text "Significance map: "
      entry $w.files.sm.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(map)
      pack $w.files.sm.lab -side left 
      pack $w.files.sm.dat -side right
      SetHelpText $w.files.sm "Name of output significance map, or \! for none"

    frame $w.files.ss
      label $w.files.ss.lab -text "Source subtracted image : "
      entry $w.files.ss.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(ssub)
      pack $w.files.ss.lab -side left 
      pack $w.files.ss.dat -side right
      SetHelpText $w.files.ss "Name of source subtracted image, or \! for none"

    frame $w.files.out
      label $w.files.out.lab -text "Output source list: "
      entry $w.files.out.dat -fg blue -width 50 -relief sunken -bd 2 \
            -textvariable PSS(out)
      pack $w.files.out.lab -side left 
      pack $w.files.out.dat -side right
      SetHelpText $w.files.out "Name of output source list, or \! for none"

    pack $w.files.lab -side top -anchor nw
    pack $w.files.inp $w.files.bg $w.files.slice $w.files.sm \
         $w.files.ss $w.files.out -fill x -side top -anchor nw

    label $w.middle.psf.lab -text "Point Response Setup" -relief raised
    frame $w.middle.psf.psf
      label $w.middle.psf.psf.lab -text "Psf name: "
      entry $w.middle.psf.psf.dat -fg blue -width 20 -relief sunken -bd 2 \
            -textvariable PSS(psf)
      pack $w.middle.psf.psf.lab -side left 
      pack $w.middle.psf.psf.dat -side right
      SetHelpText $w.middle.psf.psf "Name of psf, or AUTO for default"

    frame $w.middle.psf.mask
      label $w.middle.psf.mask.lab -text "Psf mask name: "
      entry $w.middle.psf.mask.dat -fg blue -width 20 -relief sunken -bd 2 \
            -textvariable PSS(mask)
      pack $w.middle.psf.mask.lab -side left 
      pack $w.middle.psf.mask.dat -side right
      SetHelpText $w.middle.psf.mask "Name of psf sub-option, or AUTO for default"

    frame $w.middle.psf.aux
      label $w.middle.psf.aux.lab -text "Psf extra data: "
      entry $w.middle.psf.aux.dat -fg blue -width 20 -relief sunken -bd 2 \
            -textvariable PSS(aux)
      pack $w.middle.psf.aux.lab -side left 
      pack $w.middle.psf.aux.dat -side right
      SetHelpText $w.middle.psf.aux "Psf AUX value, eg. Gaussian width or PSPC mean photon energy"

    frame $w.middle.psf.con
      label $w.middle.psf.con.lab -text "Psf constant across field: "
      entry $w.middle.psf.con.dat -fg blue -width 20 -relief sunken -bd 2 \
            -textvariable PSS(psfcon)
      pack $w.middle.psf.con.lab -side left 
      pack $w.middle.psf.con.dat -side right
      SetHelpText $w.middle.psf.con \
	"Use single psf for whole field (Y), or let it vary (N) (AUTO knows best)"

    frame $w.middle.psf.pix
      label $w.middle.psf.pix.lab -text "Psf box radii: "
      entry $w.middle.psf.pix.dat -fg blue -width 20 -relief sunken -bd 2 \
            -textvariable PSS(psfpix)
      pack $w.middle.psf.pix.lab -side left 
      pack $w.middle.psf.pix.dat -side right
      SetHelpText $w.middle.psf.pix \
        "Fraction of psf to enclose, a single radius in pixels, or 3 radii"

    pack $w.middle.psf.lab -side top -anchor nw -fill x
    pack $w.middle.psf.psf $w.middle.psf.mask $w.middle.psf.aux \
          $w.middle.psf.con $w.middle.psf.pix -fill x -side top -anchor nw


    label $w.middle.fit.lab -text "Search & fitting control" -relief raised
    frame $w.middle.fit.min
      label $w.middle.fit.min.lab -text "Search threshold (sigma) : "
      entry $w.middle.fit.min.dat -fg blue -width 10 -relief sunken -bd 2 \
            -textvariable PSS(sigmin)
      pack $w.middle.fit.min.lab -side left 
      pack $w.middle.fit.min.dat -side right
      SetHelpText $w.middle.fit.min \
        "Significance threshold in output source list"

    frame $w.middle.fit.ferl
      label $w.middle.fit.ferl.lab -text "Flux error confidence : "
      entry $w.middle.fit.ferl.dat -fg blue -width 10 -relief sunken -bd 2 \
            -textvariable PSS(ferl)
      pack $w.middle.fit.ferl.lab -side left 
      pack $w.middle.fit.ferl.dat -side right
      SetHelpText $w.middle.fit.ferl \
        "Flux error confidence, either <N> sigma, or a percentage"

    frame $w.middle.fit.perl
      label $w.middle.fit.perl.lab -text "Position error confidence : "
      entry $w.middle.fit.perl.dat -fg blue -width 10 -relief sunken -bd 2 \
            -textvariable PSS(perl)
      pack $w.middle.fit.perl.lab -side left 
      pack $w.middle.fit.perl.dat -side right
      SetHelpText $w.middle.fit.perl \
        "Position error confidence, either <N> sigma, or a percentage"

    pack $w.middle.fit.lab -side top -anchor nw -fill x
    pack $w.middle.fit.min $w.middle.fit.ferl \
	$w.middle.fit.perl -fill x -side top -anchor nw

    label $w.post.mark.lab -text "Source Marking" -relief raised
    frame $w.post.mark.onoff
      label $w.post.mark.onoff.lab -text "Source marking: "
      checkbutton $w.post.mark.onoff.on -text "On" -variable PSS(mark) \
          -onvalue 1
      checkbutton $w.post.mark.onoff.off -text "Off" -variable PSS(mark) \
          -onvalue 0
      pack $w.post.mark.onoff.lab -side left
      pack $w.post.mark.onoff.off $w.post.mark.onoff.on -side right
      SetHelpText $w.post.mark.onoff \
        "Mark sources on image when PSS is finished?"

    frame $w.post.mark.num
      label $w.post.mark.num.lab -text "Numbering: "
      checkbutton $w.post.mark.num.on -text "On" -variable PSS(mark_n) \
          -onvalue y
      checkbutton $w.post.mark.num.off -text "Off" -variable PSS(mark_n) \
          -onvalue n
      pack $w.post.mark.num.lab -side left
      pack $w.post.mark.num.off $w.post.mark.num.on -side right
      SetHelpText $w.post.mark.num \
        "If marking is on, number them too?"

    pack $w.post.mark.lab $w.post.mark.onoff $w.post.mark.num \
	 -side top -anchor nw -fill x

    label $w.post.disp.lab -text "Source List Display" -relief raised
#    frame $w.post.disp.onoff
#      label $w.post.disp.onoff.lab -text "Source marking: "
#      checkbutton $w.post.disp.onoff.on -text "On" -variable PSS(dump) \
#          -onvalue 1
#      checkbutton $w.post.disp.onoff.off -text "Off" -variable PSS(dump) \
#          -onvalue 0
#      pack $w.post.disp.onoff.lab -side left
#      pack $w.post.disp.onoff.off $w.post.disp.onoff.on -side right

    frame $w.post.disp.dev
      label $w.post.disp.dev.lab -text "Dump device : "
      entry $w.post.disp.dev.dat -fg blue -width 10 -relief sunken -bd 2 \
            -textvariable PSS(dump_d)
      menubutton $w.post.disp.dev.list -text "Select" \
	-menu $w.post.disp.dev.list.menu -relief raised
      menu $w.post.disp.dev.list.menu
      $w.post.disp.dev.list.menu add command -label "TERMINAL" \
        -command {set PSS(dump_d) TERMINAL}
      $w.post.disp.dev.list.menu add command -label "PRINTER" \
        -command {set PSS(dump_d) PRINTER}
      $w.post.disp.dev.list.menu add command -label "auto name" \
        -command {set PSS(dump_d) $PSS(out).lis}
      SetHelpText $w.post.disp.dev.list \
        "Choose ascii device for source search output"

      pack $w.post.disp.dev.lab -side left 
      pack $w.post.disp.dev.list $w.post.disp.dev.dat -side right

    pack $w.post.disp.lab $w.post.disp.dev \
	 -side top -anchor nw -fill x

    pack $w.files $w.middle $w.post $w.help $w.ctrl \
             -side top -anchor nw -ipadx 5 -ipady 5
      
# Create an "OK" and a "Cancel" button.
    button $w.ctrl.ok -text "Start PSS" -width 6 -command {set isyspss_priv(button) "ok"}
    button $w.ctrl.can -text Cancel -width 6 \
	-command {set isyspss_priv(button) "can"}
    SetHelpText $w.ctrl.ok \
        "This will start PSS with these settings"
    SetHelpText $w.ctrl.can \
        "This will cancel this dialogue, but retain the changes you've made"

# Pack them into the bottom frame with a default border around the OK
# button.
    frame $w.ctrl.default -relief sunken -bd 1
    raise $w.ctrl.ok $w.ctrl.default
    pack $w.ctrl.default -side left -expand 1 -padx 3m -pady 2m
    pack $w.ctrl.ok -in $w.ctrl.default -padx 1m -pady 1m -ipadx 1m

    pack $w.ctrl.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

# Bind the return key to the OK button.
    bind $w <Return> "$w.ctrl.ok flash; set isyspss_priv(button) ok"

# Centre the window
    WinCentre $w

#  Set a grab and claim the focus.
#    set oldFocus [focus]
#    grab $w
#    focus $w

# Wait for the user to respond.
    for {} {1} {} {
	tkwait variable isyspss_priv(button)

	if ![string compare $isyspss_priv(button) ok] {

	# The OK button as pressed so set global variables to values in entry
	# fields
        set pstring ""
        foreach p {psf mask aux psfcon} {
          if { $PSS($p) != "auto" } {
            set pstring "$pstring $p=$PSS($p)"
            }
          }

        set done 0
        $w configure -cursor watch

        pack forget $w.files $w.middle $w.post $w.help $w.ctrl $w.ctrl.can

	$w.ctrl.ok configure -text OK
	pack $w.ctrl.ok

        set msg_window [frame $w.op]

        set msg_scroll [scrollbar $w.op.yscroll -orient vertical  \
         -relief sunken  -bd 2]

        set msg_data [text $w.op.txt -wrap none -state disabled -height 20 \
		-width 79 -font -adobe-courier-medium-r-normal--*-100-*-*-*-*-*-*
		]

        pack $msg_data -side left -fill x -fill y -anchor w
        pack $msg_scroll -side left -fill y

        update idletasks

        src obey pss "inp=$PSS(inp) bgnd=$PSS(bgnd) ssub=$PSS(ssub) \
map=$PSS(map) out=$PSS(out) notry_again expert=y dev=terminal \
psfpix='$PSS(psfpix)' \
sigmin=$PSS(sigmin) $pstring ferl='$PSS(perl)' perl='$PSS(perl)' \\" \
-inform "IsysPssHandleOutput $msg_data %V" -endmsg {set done 1}
        set PSS(finished) 1
 
        pack $msg_window $w.ctrl \
             -side top -anchor nw -ipadx 5 -ipady 5
              
        tkwait variable done
        $w configure -cursor xterm

	tkwait variable isyspss_priv(button)
	}
      destroy $w
#      focus $oldFocus
      break
    }
}

