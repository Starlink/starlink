#!/usr/local/bin/wish -f
#+
#  Name:
#     IsysBgnd.tcl

#  Purpose:
#     Interactively create a background model using Isys

#  Language:
#     tcl/tk

#  Type of Module:
#     GUI dialog

#  Invocation:
#     IsysBgnd parent

#  Description:
#     {routine_description}

#  Usage:
#     IsysBgndDialog window

#  Environment Parameters:
#     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
#        {parameter_description}

#  Examples:
#     {routine_example_text}
#        {routine_example_description}

#  Pitfalls:
#     {pitfall_description}...

#  Notes:
#     {routine_notes}...

#  Prior Requirements:
#     {routine_prior_requirements}...

#  Side Effects:
#     {routine_side_effects}...

#  Algorithm:
#     {algorithm_description}...

#  Accuracy:
#     {routine_accuracy}

#  Timing:
#     {routine_timing}

#  Implementation Status:
#     {routine_implementation_status}

#  External Routines Used:
#     {name_of_facility_or_package}:
#        {routine_used}...

#  Implementation Deficiencies:
#     {routine_deficiencies}...

#  Keywords:
#     package:ifedit, usage:public

#  Copyright:
#     Copyright (C) University of Birmingham, 1995

#  Authors:
#     DJA: David J. Allan (Jet-X, University of Birmingham)
#     {enter_new_authors_here}

#  History:
#     23 Jan 1996 V2.0-0 (DJA):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

proc IsysBgndInit {} {

  global IsysBGM nbid

# Extract field with for default of annular shell width
  set lx [nbs get $nbid.regxmin]
  set hx [nbs get $nbid.regxmax]


  set    IsysBGM(Sample) WHOLE
  set    IsysBGM(FitMode) CONS
  set    IsysBGM(CurSampPack) ""
  set    IsysBGM(RbinSize) [expr abs($hx - $lx) / 20.0]
  set    IsysBGM(Xcentre) 0.0
  set    IsysBGM(Ycentre) 0.0
  set    IsysBGM(BoxPixSize) 16
  set    IsysBGM(Nsrc) 0
  set    IsysBGM(SrcX) {}
  set    IsysBGM(SrcY) {}
  set    IsysBGM(SrcR) {}
  set    IsysBGM(ExportModel) none
  set    IsysBGM(RMS)  0.0
  set    IsysBGM(SaveSrcMode) internal
  set    IsysBGM(SaveSrcFile) sources.txt
  set    IsysBGM(Mean) 0.0
  }

#
# Load globals first time through
#
IsysBgndInit

global BGnsrc BGsrcx BGsrcy BGsrcr

set    BGnsrc   0


#
# Configure the fitting mode radio buttons depending on the sampling choice
#
proc IsysBgndConfigFbuts {} {
  global IsysBGM Widget

  if { $IsysBGM(Sample) == "WHOLE" } {
    $Widget(FIT_MODE_POLY) configure -state disabled
    $Widget(FIT_MODE_SPLINE) configure -state disabled
  } elseif { $IsysBGM(Sample) == "ANN" } {
    $Widget(FIT_MODE_POLY) configure -state normal
    $Widget(FIT_MODE_SPLINE) configure -state disabled
  } elseif { $IsysBGM(Sample) == "BOX" } {
    $Widget(FIT_MODE_NONE) configure -state normal
    $Widget(FIT_MODE_MEAN) configure -state normal
    $Widget(FIT_MODE_POLY) configure -state normal
    $Widget(FIT_MODE_SPLINE) configure -state normal
    }
  }


#
# Create string describing source 'src'
#
proc SourceFormat {src} {
  global IsysBGM
  set isrc [expr $src - 1]
  return [format "%3i %9.4f %9.4f %9.4f" $src \
		[lindex $IsysBGM(SrcX) $isrc] \
		[lindex $IsysBGM(SrcY) $isrc] \
		[lindex $IsysBGM(SrcR) $isrc] ]
  }


#
# Pack the current sampling shape control. Unpack the existing first if
# it is defined
#
proc IsysBgndPackSamp {} {
  global IsysBGM Widget

  if { $IsysBGM(CurSampPack) != "" } {
    set shp $IsysBGM(CurSampPack)
    pack forget $Widget(SAMP_SHP_CTRL_$shp)
    }

  set shp $IsysBGM(Sample)
  pack $Widget(SAMP_SHP_CTRL_$shp)
  set IsysBGM(CurSampPack) $shp
  }



#
# The procedure which is invoked when the user changes the sampling form
#
proc IsysBgndChangeSampling {name element op} {
  global Widget

# Repack the sampling widget
  IsysBgndPackSamp

# Mark sampling as out of date
  $Widget(FIT_RMS_DAT) configure -fg grey

# Update the fit mode allowed buttons
  IsysBgndConfigFbuts
  }


#
# The procedure which is invoked when the user changes the fitting form
#
proc IsysBgndChangeFitMode {name element op} {
  global Widget

# Update the fitting mode
  IsysBgndReFit
  }


#
# The procedure invoked when the user changes the value of IsysBGM(Nsrc)
#
proc IsysBgndChangeNsrc {name element op} {
  global Widget IsysBGM

  if { $IsysBGM(Nsrc) == 0 } {
    $Widget(MENUBAR).file.menu entryconfigure "Save Sources" -state disabled
    $Widget(SRC_DEL_BUTTON) configure -state disabled
    $Widget(SRC_MARK_BUTTON) configure -state disabled
  } else {
    $Widget(MENUBAR).file.menu entryconfigure "Save Sources" -state normal
    $Widget(SRC_DEL_BUTTON) configure -state normal
    $Widget(SRC_MARK_BUTTON) configure -state normal
    }
  }


proc IsysBgndResample {} {
  global Widget IsysBGM

  $Widget(SAMP_BOX_OK) flash

  ImgExecWait ibgnd \
	"SETSAMP SAMPLING=$IsysBGM(Sample) X=$IsysBGM(Xcentre) Y=$IsysBGM(Ycentre) RBIN=$IsysBGM(RbinSize)"

  $Widget(FIT_RMS_DAT) configure -fg black
  }


proc IsysBgndReFit {} {
  global Widget IsysBGM

  ImgExecWait ibgnd "SETFIT SAMPFIT=$IsysBGM(FitMode)"
  }



proc IsysBgndMonitor {} {

  global nbid
  global BGnsrc BGsrcx BGsrcy BGsrcr IsysBGM

  nbs stop
  nbs clear
  nbs monitor $nbid.bg_rms  IsysBGM(RMS)
  nbs monitor $nbid.bg_nsrc BGnsrc
  nbs monitor $nbid.bg_srcx BGsrcx
  nbs monitor $nbid.bg_srcy BGsrcy
  nbs monitor $nbid.bg_srcr BGsrcr
  nbs monitor $nbid.bg_mean IsysBGM(Mean)

  nbs start 200
  }



proc IsysBgndSaveModelDialog {w} {

# Declare globals
  global isysbgndsave_priv env IsysBGM

  set isysbgndsave_priv 0

  set fname ""
  if { $IsysBGM(ExportModel) != "none" } { 
    set fname $IsysBGM(ExportModel)
    }

# Create the top level window.
  toplevel $w -class Dialog -bd 10
  wm title $w "Isys Background Model Save"
  wm iconname $w "Isys Background Modeller"
  wm transient $w .
  wm iconbitmap $w @$env(AST_ETC)/asterix.bitmap

# Create a frame with a label and entry box
  frame $w.top
  label $w.top.lab -text "Model filename: "
  entry $w.top.dat -width 40 -relief sunken -fg blue -textvariable fname
  pack $w.top.lab $w.top.dat -side top

  frame $w.bot
  button $w.bot.save -text Save -command {set isysbgndsave_priv 1}
  button $w.bot.can -text Cancel -command {set isysbgndsave_priv 99}
  pack  $w.bot.save $w.bot.can -side left

  bind $w.top.dat <Return> "$w.bot.save flash; set isysbgndsave_priv 1"

  pack $w.top $w.bot -side top

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
  wm withdraw $w
  update idletasks
  set parent [winfo parent $w]
  set x [expr [winfo x $parent] - [winfo reqwidth $w]]
  set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
        + [winfo y $parent]]
  wm geom $w +$x+$y
  wm deiconify $w

#  Set a grab and claim the focus.
  set oldFocus [focus]
  focus $w.top.dat

# Wait for the user to give the exit signal
  update idletasks
  tkwait variable isysbgndsave_priv

# Save it?
  if { $isysbgndsave_priv == 1 } {

# Get file name
    set fname [$w.top.dat get]

# Save the file
    ImgExecWait ibgnd "SAVE OUT=$fname"

# Export the model name
    set IsysBGM(ExportModel) $fname
    }
 
# Restore focus
  destroy $w
  focus $oldFocus
  }


proc IsysBgndSaveSrcDialog {w} {

# Declare globals
  global isysbgndsrc_priv env IsysBGM W

  set isysbgndsrc_priv 0

# Create the top level window.
  toplevel $w -class Dialog -bd 10
  wm title $w "Isys Background Sources Save"
  wm iconname $w "Isys Background Modeller"
  wm transient $w .
  wm iconbitmap $w @$env(AST_ETC)/asterix.bitmap

# Create a frame with a label and entry box
  set W $w
  frame $w.top
  frame $w.top.int -bd 2 -relief groove
  radiobutton $w.top.int.but -variable IsysBGM(SaveSrcMode) -anchor w \
	-text "Isys current source list" -value internal -command {
	if { $IsysBGM(SaveSrcMode) == "internal" } {
          $W.top.txt.dat configure -state disabled
        } else {
          $W.top.txt.dat configure -state normal
          }
        }

  pack $w.top.int.but -side left
 
  frame $w.top.txt -bd 2 -relief groove
  radiobutton $w.top.txt.but -variable IsysBGM(SaveSrcMode) \
	-text "Ascii text file" -anchor w -command {
	if { $IsysBGM(SaveSrcMode) == "internal" } {
          $W.top.txt.dat configure -state disabled
        } else {
          $W.top.txt.dat configure -state normal
          }
        }

  entry $w.top.txt.dat -width 40 -relief sunken -fg blue \
	-textvariable IsysBGM(SaveSrcFile)

  if { $IsysBGM(SaveSrcMode) == "internal" } {
    $w.top.txt.dat configure -state disabled
  } else {
    $w.top.txt.dat configure -state normal
  }

  pack $w.top.txt.but $w.top.txt.dat -side left -fill x
 
  pack $w.top.int $w.top.txt -fill x


  frame $w.bot
  button $w.bot.save -text Save -command {set isysbgndsrc_priv 1}
  button $w.bot.can -text Cancel -command {set isysbgndsrc_priv 99}
  pack  $w.bot.save $w.bot.can -side left

  bind $w.top.txt.dat <Return> "$w.bot.save flash; set isysbgndsrc_priv 1"

  pack $w.top $w.bot -side top

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
  wm withdraw $w
  update idletasks
  set parent [winfo parent $w]
  set x [expr [winfo x $parent] - [winfo reqwidth $w]]
  set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
        + [winfo y $parent]]
  wm geom $w +$x+$y
  wm deiconify $w

#  Set a grab and claim the focus.
  set oldFocus [focus]
  focus $w.top.txt.dat

# Wait for the user to give the exit signal
  update idletasks
  tkwait variable isysbgndsrc_priv

# Save it?
  if { $isysbgndsrc_priv == 1 } {

# Save the file
    ImgExecWait ibgnd "SAVESRC SUBMODE=$IsysBGM(SaveSrcMode) OUT=$IsysBGM(SaveSrcFile)"
    }
 
# Restore focus
  destroy $w
  focus $oldFocus
  }



proc IsysBgndNew {} {
  global Widget

# Reset the globals
  IsysBgndInit 

# Reset list box contents
  $Widget(SRC_DATA_LIST) delete 0 end
  $Widget(SRC_DATA_LIST) yview end 

# Restart modeller
  ImgExecWait ibgnd "NEW"
  }


proc IsysBgndDialog {w} {

# Declare globals
  global Widget isysbgnd_priv env IsysBGM nbid BGnsrc BGsrcx \
	BGsrcy BGsrcr

  set isysbgnd_priv 0

# Create the top level window.
  toplevel $w -class Dialog -bd 10
  wm title $w "Isys Background Modeller"
  wm iconname $w "Isys Background Modeller"
  wm transient $w .
  wm iconbitmap $w @$env(AST_ETC)/asterix.bitmap

# The help window
  set Widget(HELP) [MakeHelpWindow $w.help 32]

# The main windows consist of the menu bar, the source box, the sampling
# box, the fit box and the image display button box
  set Widget(MENUBAR) [
    MkMenuBar $w.mbar \
     {MkMenu &File \
      {MkMenuCmd &New {IsysBgndNew} } \
      {MkMenuCmd "Save &Model" {IsysBgndSaveModelDialog .save_model} } \
      {MkMenuCmd "Save &Sources" {IsysBgndSaveSrcDialog .save_src} -state disabled} \
      {MkMenuCmd &Close {puts "Not implemented yet"} \
               -state disabled } \
      MkMenuSep \
      {MkMenuCmd E&xit {set isysbgnd_priv -1}} \
      } \
     {MkMenu &Edit \
      {MkMenuCmd "New Parameter" {puts "Not implemented yet"} } \
      MkMenuSep \
      {MkMenuCmd &Cut {puts "Not implemented yet"} } \
      {MkMenuCmd &Paste {puts "Not implemented yet"} } \
      -state disabled \
      } \
    > \
    {MkMenu &Help -state disabled
      }
  ]

# Enable the source menu item if sources present
  if { $IsysBGM(Nsrc) > 0 } {
    $Widget(MENUBAR).file.menu entryconfigure "Save Sources" -state normal
    }

set Widget(SRC_BOX) [
  frame $w.src -relief groove -bd 2
  ]

  set Widget(SRC_DATA_LAB) [
    label $Widget(SRC_BOX).lab -text "Source Database" \
	-font -adobe-times-bold-i-normal--*-140-*-*-*-*-*-*
    ]

  set Widget(SRC_LIST) [
    frame $Widget(SRC_BOX).slist
    ]

    set Widget(SRC_DATA_LIST) [
      listbox $Widget(SRC_LIST).list -relief sunken -bd 2 \
      	-yscrollcommand "$Widget(SRC_LIST).scroll set" \
	-font -adobe-courier-medium-r-normal--*-100-*-*-*-*-*-* \
	-height 8 -width 33
	]

    SetHelpText $Widget(SRC_DATA_LIST) \
	"Areas to be excluded, click to select"

# Load sources
    if { $IsysBGM(Nsrc) > 0 } {
      for {set i 1} {$i <= $IsysBGM(Nsrc)} {incr i} {
        $Widget(SRC_DATA_LIST) insert end [SourceFormat $i]
        }
      $Widget(SRC_DATA_LIST) yview end
      }
 
    set Widget(SRC_DATA_SCROLL) [
      scrollbar $Widget(SRC_LIST).scroll -command "$Widget(SRC_DATA_LIST) yview"
      ]
      
    pack $Widget(SRC_DATA_LIST) -side left
    pack $Widget(SRC_DATA_SCROLL) -side right -fill y
   
  set Widget(SRC_BUTTONS) [
    frame $Widget(SRC_BOX).buts
    ]

    set Widget(SRC_ADD_BUTTON) [
      button $Widget(SRC_BUTTONS).add -text Add \
	-command {
                  $Widget(CANVAS) configure -cursor crosshair
                  bind $Widget(CANVAS) <ButtonPress-1> {put_pos %x %y}
                  set onsrc $BGnsrc
                  ImgExecWait ibgnd "ADDSRC"
                  if { $BGnsrc == $onsrc } {
  		    tkwait variable BGnsrc
                    }

#              Add source to list
		  incr IsysBGM(Nsrc)
		  lappend IsysBGM(SrcX) $BGsrcx
		  lappend IsysBGM(SrcY) $BGsrcy
		  lappend IsysBGM(SrcR) $BGsrcr

#              Add new text to list box and make sure its displayed
		  $Widget(SRC_DATA_LIST) insert end \
			[SourceFormat $IsysBGM(Nsrc)]
		  $Widget(SRC_DATA_LIST) yview end 

 		  update idletasks
                  $Widget(CANVAS) configure -cursor arrow
                  bind $Widget(CANVAS) <ButtonPress-1> {}
         }
      ]
      
    SetHelpText $Widget(SRC_ADD_BUTTON) \
	"Click to add a new source"

    set Widget(SRC_ADD3_BUTTON) [
      button $Widget(SRC_BUTTONS).auto -text AutoAdd -state disabled
      ]
      
    set Widget(SRC_DEL_BUTTON) [
      button $Widget(SRC_BUTTONS).del -text Delete -command {
        if { $BGnsrc > 0 } {
  	  set isrc [$Widget(SRC_DATA_LIST) curselection]
          if { $isrc != "" } {

#        Delete source and wait for task to decrement source counter
            set onsrc $BGnsrc
            ImgExecWait ibgnd "DELSRC ISRC=[expr $isrc + 1]"
            if { $onsrc == $BGnsrc } {
              tkwait variable BGnsrc
              }

#        Delete the source from our list
            set IsysBGM(SrcX) [lreplace $IsysBGM(SrcX) $isrc $isrc]
            set IsysBGM(SrcY) [lreplace $IsysBGM(SrcY) $isrc $isrc]
            set IsysBGM(SrcR) [lreplace $IsysBGM(SrcR) $isrc $isrc]
            incr IsysBGM(Nsrc) -1

#        Delete from the selected line to the end of the list box
            $Widget(SRC_DATA_LIST) delete $isrc end

#        Re-format subsequent sources
            for {set i [expr $isrc + 1]} {$i <= $IsysBGM(Nsrc)} {incr i} {
	      $Widget(SRC_DATA_LIST) insert end \
			[SourceFormat $i]
              }

#         Add new text to list box and make sure its displayed
	    $Widget(SRC_DATA_LIST) yview end 
            }
          }
	}
      ]
      
    SetHelpText $Widget(SRC_DEL_BUTTON) \
	"Click to delete a selected source"

    set Widget(SRC_MARK_BUTTON) [
      button $Widget(SRC_BUTTONS).mrk -text Mark \
	-command {ImgExecWait ibgnd "MARKSRC"}
      ]
      
    SetHelpText $Widget(SRC_MARK_BUTTON) \
	"Click to mark source areas on image"

    pack $Widget(SRC_ADD_BUTTON) $Widget(SRC_ADD3_BUTTON) \
    	$Widget(SRC_DEL_BUTTON) $Widget(SRC_MARK_BUTTON) \
	-side left -fill y
      
  pack $Widget(SRC_DATA_LAB) -ipady 2 \
	-side top -anchor nw -fill x
  pack $Widget(SRC_LIST) $Widget(SRC_BUTTONS) \
	-side top -anchor nw -fill x

set Widget(SAMP_BOX) [
  frame $w.sam -bd 2 -relief groove
  ]

  set Widget(SAMP_BOX_LAB) [
    label $Widget(SAMP_BOX).lab -text "Sampling Control" \
	-font -adobe-times-bold-i-normal--*-140-*-*-*-*-*-*
    ]

  set Widget(SAMP_BOX_SHP) [
    frame $Widget(SAMP_BOX).shps
    ]

  set Widget(SAMP_SHP_WHOLE) [
    radiobutton $Widget(SAMP_BOX_SHP).whole -text "Whole image" \
	-variable IsysBGM(Sample) -value WHOLE
    ]

  SetHelpText $Widget(SAMP_SHP_WHOLE) \
	"Change to single sample from image"

  set Widget(SAMP_SHP_ANN) [
    radiobutton $Widget(SAMP_BOX_SHP).ann -text Annuli \
	-variable IsysBGM(Sample) -value ANN
    ]

  SetHelpText $Widget(SAMP_SHP_ANN) \
	"Change to annular sampling"

  set Widget(SAMP_SHP_BOXES) [
    radiobutton $Widget(SAMP_BOX_SHP).box \
	-text Boxes -variable IsysBGM(Sample) -value BOX -state disabled
    ]

  SetHelpText $Widget(SAMP_SHP_BOXES) \
	"Change to box sampling"

  pack $Widget(SAMP_SHP_WHOLE) $Widget(SAMP_SHP_ANN) $Widget(SAMP_SHP_BOXES) \
	-side left -anchor nw -padx 5 -pady 5

  set Widget(SAMP_SHP_CTRL) [
    frame $Widget(SAMP_BOX).ctrl
    ]

#
# 3 frames to hold the different sampling packing info
#
  set Widget(SAMP_SHP_CTRL_WHOLE) [
    frame $Widget(SAMP_SHP_CTRL).whole
    ]

  set Widget(SAMP_SHP_CTRL_ANN) [
    frame $Widget(SAMP_SHP_CTRL).ann
    ]

  set Widget(SAMP_SHP_CTRL_BOX) [
    frame $Widget(SAMP_SHP_CTRL).box
    ]

#
# The annular sampling stuff
#
  set Widget(SAMP_SHP_CTRL_ANN_X) [
    frame $Widget(SAMP_SHP_CTRL_ANN).xc
    ]

    set Widget(SAMP_SHP_CTRL_ANN_X_LAB) [
      label $Widget(SAMP_SHP_CTRL_ANN_X).lab -text "X centre: " ]

    set Widget(SAMP_SHP_CTRL_ANN_X_DAT) [
      entry $Widget(SAMP_SHP_CTRL_ANN_X).dat -width 6 -relief sunken \
		-fg blue -textvariable IsysBGM(Xcentre)]

    pack $Widget(SAMP_SHP_CTRL_ANN_X_LAB) $Widget(SAMP_SHP_CTRL_ANN_X_DAT) \
	-side left

    SetHelpText $Widget(SAMP_SHP_CTRL_ANN_X) \
  	"Specify the X centre of the annuli"

  set Widget(SAMP_SHP_CTRL_ANN_Y) [
    frame $Widget(SAMP_SHP_CTRL_ANN).yc
    ]

    set Widget(SAMP_SHP_CTRL_ANN_Y_LAB) [
      label $Widget(SAMP_SHP_CTRL_ANN_Y).lab -text "Y centre: " ]

    set Widget(SAMP_SHP_CTRL_ANN_Y_DAT) [
      entry $Widget(SAMP_SHP_CTRL_ANN_Y).dat -width 6 -relief sunken \
		-fg blue -textvariable IsysBGM(Xcentre)]

    pack $Widget(SAMP_SHP_CTRL_ANN_Y_LAB) $Widget(SAMP_SHP_CTRL_ANN_Y_DAT) \
	-side left

    SetHelpText $Widget(SAMP_SHP_CTRL_ANN_Y) \
  	"Specify the Y centre of the annuli"

  set Widget(SAMP_SHP_CTRL_ANN_R) [
    frame $Widget(SAMP_SHP_CTRL_ANN).rb
    ]

    set Widget(SAMP_SHP_CTRL_ANN_R_LAB) [
      label $Widget(SAMP_SHP_CTRL_ANN_R).lab -text "Bin width: " ]

    set Widget(SAMP_SHP_CTRL_ANN_R_DAT) [
      entry $Widget(SAMP_SHP_CTRL_ANN_R).dat -width 6 -relief sunken \
		-fg blue -textvariable IsysBGM(RbinSize)]

    pack $Widget(SAMP_SHP_CTRL_ANN_R_LAB) $Widget(SAMP_SHP_CTRL_ANN_R_DAT) \
	-side left

    SetHelpText $Widget(SAMP_SHP_CTRL_ANN_R) \
  	"Specify the annnular bin width"

  pack $Widget(SAMP_SHP_CTRL_ANN_X) $Widget(SAMP_SHP_CTRL_ANN_Y) \
	$Widget(SAMP_SHP_CTRL_ANN_R) -side left

  IsysBgndPackSamp

  IsysBgndChangeNsrc IsysBGM Nsrc w

  pack $Widget(SAMP_BOX_LAB) -ipady 2 -side top
  pack $Widget(SAMP_BOX_SHP) -side top

  pack $Widget(SAMP_SHP_CTRL) -side left -anchor nw

  set Widget(SAMP_BOX_BUTS) [
    frame $Widget(SAMP_BOX).buts
    ]

    set Widget(SAMP_BOX_OK) [
      button $Widget(SAMP_BOX_BUTS).ok -text Ok \
	-command {IsysBgndResample}
      ]

    SetHelpText $Widget(SAMP_BOX_OK) \
  	"Recompute the samples"

    pack $Widget(SAMP_BOX_OK)

  pack $Widget(SAMP_BOX_BUTS) -side bottom -anchor sw


  bind $Widget(SAMP_SHP_CTRL_ANN_X_DAT) <Return> {IsysBgndResample}
  bind $Widget(SAMP_SHP_CTRL_ANN_Y_DAT) <Return> {IsysBgndResample}
  bind $Widget(SAMP_SHP_CTRL_ANN_R_DAT) <Return> {IsysBgndResample}


set Widget(FIT_BOX) [
  frame $w.fit -bd 2 -relief groove
  ]

  set Widget(FIT_BOX_LAB) [
    label $Widget(FIT_BOX).lab -text "Surface Definition" \
	-font -adobe-times-bold-i-normal--*-140-*-*-*-*-*-*
    ]

  set Widget(FIT_MODES) [
    frame $Widget(FIT_BOX).modes
    ]

  set Widget(FIT_MODE_NONE) [
    radiobutton $Widget(FIT_MODES).none -text "none" \
	-variable IsysBGM(FitMode) -value NONE
    ]

  SetHelpText $Widget(FIT_MODE_NONE) \
  	"Raw sample values"

  set Widget(FIT_MODE_MEAN) [
    radiobutton $Widget(FIT_MODES).mean -text "mean" \
	-variable IsysBGM(FitMode) -value CONS
    ]

  SetHelpText $Widget(FIT_MODE_MEAN) \
  	"Mean of all samples"

  set Widget(FIT_MODE_POLY) [
    radiobutton $Widget(FIT_MODES).poly -text "poly" \
	-variable IsysBGM(FitMode) -value POLY
    ]

  SetHelpText $Widget(FIT_MODE_POLY) \
  	"Polynomial fit to sample means"

  set Widget(FIT_MODE_SPLINE) [
    radiobutton $Widget(FIT_MODES).spline -text "spline" \
	-variable IsysBGM(FitMode) -value SPLINE
    ]

  SetHelpText $Widget(FIT_MODE_SPLINE) \
  	"Spline fit to sample means"

  pack $Widget(FIT_MODE_NONE) $Widget(FIT_MODE_MEAN) $Widget(FIT_MODE_POLY) \
	$Widget(FIT_MODE_SPLINE) -side left

  set Widget(FIT_RMS_BOX) [
    frame $Widget(FIT_BOX).rms
    ]

    set Widget(FIT_RMS_LAB) [
      label $Widget(FIT_RMS_BOX).lab -text "RMS fractional residual"
      ]

    set Widget(FIT_RMS_DAT) [
      label $Widget(FIT_RMS_BOX).dat -textvariable IsysBGM(RMS) -relief sunken
      ]
 
    SetHelpText $Widget(FIT_RMS_BOX) \
  	"The mean fractional residual over the field"

    pack $Widget(FIT_RMS_DAT) -side right
    pack $Widget(FIT_RMS_LAB) -side left -fill x

  pack $Widget(FIT_BOX_LAB) -ipady 2 -side top
  pack $Widget(FIT_MODES) $Widget(FIT_RMS_BOX) -side top


set Widget(DISP_BOX) [
  frame $w.dis -bd 2 -relief groove
  ]

  set Widget(DISP_BOX_LAB) [
    label $Widget(DISP_BOX).lab -text "Display Images" \
	-font -adobe-times-bold-i-normal--*-140-*-*-*-*-*-*
    ]

  set Widget(DISP_DATA) [
    button $Widget(DISP_BOX).data -text "Data" \
	-command {ImgExecWait ibgnd "DDISP"}
    ]
  
  SetHelpText $Widget(DISP_DATA) \
  	"Display the current data"

  set Widget(DISP_RESID) [
    button $Widget(DISP_BOX).resid -text "Residuals" \
	-command {ImgExecWait ibgnd "RDISP"}
    ]
  
  SetHelpText $Widget(DISP_RESID) \
  	"Display the current (data - model)"

  set Widget(DISP_MOD) [
    button $Widget(DISP_BOX).mod -text "Model" \
	-command {ImgExecWait ibgnd "MDISP"}
    ]
    
  SetHelpText $Widget(DISP_MOD) \
  	"Display the current model surface"

  pack $Widget(DISP_BOX_LAB) -ipady 2 -side top -anchor nw -fill x 
  pack $Widget(DISP_DATA) $Widget(DISP_RESID) $Widget(DISP_MOD) \
  	-side left 

  pack $Widget(MENUBAR) -fill x
  pack $Widget(SRC_BOX) $Widget(SAMP_BOX) $Widget(FIT_BOX) \
	$Widget(DISP_BOX) $Widget(HELP) -side top -fill x 

# Make sure fit mode buttons are up to date
  IsysBgndConfigFbuts

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
  wm withdraw $w
  update idletasks
  set parent [winfo parent $w]
  set x [expr [winfo x $parent] - [winfo reqwidth $w]]
  set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
        + [winfo y $parent]]
  wm geom $w +$x+$y
  wm deiconify $w

#  Set a grab and claim the focus.
  set oldFocus [focus]
  focus $w

# Monitor the noticeboard values
  IsysBgndMonitor

# Set a trace on the sampling form
  trace variable IsysBGM(Sample) w IsysBgndChangeSampling
  trace variable IsysBGM(FitMode) w IsysBgndChangeFitMode
  trace variable IsysBGM(Nsrc) w IsysBgndChangeNsrc

# Start background modeller
  $w configure -cursor watch
  ImgExecWait ibgnd "START"
  $w configure -cursor arrow

# Wait for the user to give the exit signal
  update idletasks
  tkwait variable isysbgnd_priv

# Store mode if single sample and not exported
  if { $IsysBGM(ExportModel) == "none" && $IsysBGM(Sample) == "WHOLE" } {
    set IsysBGM(ExportModel) $IsysBGM(Mean)
    }

# Restore focus
  destroy $w
  focus $oldFocus
  }
