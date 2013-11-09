
global AST_ETC AST_ROOT AST_BIN
set AST_ETC $env(AST_ETC)
set AST_ROOT $env(AST_ROOT)
set AST_BIN $env(AST_BIN)
set AST_TCL_LIB $env(AST_TCL_LIB)

lappend auto_path $AST_ETC

lappend auto_path $AST_TCL_LIB

#  Look for Cursa
global CURSA_DIR CursaThere
if {[catch {set CURSA_DIR $env(CURSA_DIR)}] == 0} {
  set CursaThere 1
} else {
  set CursaThere 0
}

wm withdraw .


#
#
global ImageOpen
global remoteCatsShort
global remoteCatsLong
global ContextHelp
global nbid
global InitFile
global TipFile
global AboutFile
global SymbolFile 
global FileARD
global CatListFile
global Control

#
#  Perform initialisations
IsysInit
#
#
#
#create top level frame
wm title . "Isys"
wm iconname . Isys
wm iconbitmap . @$env(AST_ETC)/asterix.bitmap
frame .top 

#
#  Define the output region for the messages window.
#

set msg_window [
  frame .top.msg 
  ]

set msg_scroll [
  scrollbar .top.msg.yscroll -orient vertical  \
         -relief sunken  -bd 2
  ]

set msg_data [
  text .top.msg.output -wrap none -state disabled -height 6 \
         -relief sunken
  ]

pack $msg_data -side left -fill x -fill y -anchor w
pack $msg_scroll -side left -fill y
#pack $msg_window  -pady 2m  

$msg_scroll configure -command "$msg_data yview"
$msg_data configure -yscroll "$msg_scroll set"

#  arrange top level frames in main window
global mbar tbar stbar cbar attrib
pack [frame [set mbar .top.mbar] -relief raised -bd 2] -side top -fill x
pack [frame [set tbar .top.tbar]] -side top
pack [frame .top.dummyt1] -side top
pack [frame .top.dummyt2] -side top

set stbar [frame .top.stbar]
if {$ShowShape == 1} {
  pack $stbar -after .top.dummyt1 -side top
}

set cbar [frame .top.cbar]
if {$ShowCache == 1} {
  pack $cbar -after .top.dummyt2 -side top
}

pack [frame .top.dummyb] -side bottom
if {$ShowMsg == 1} {
  pack .top.msg -side bottom -anchor w -fill x
}
pack [frame [set help .top.help]] -side bottom -anchor w -pady 2m  -fill x
pack [frame .top.dummyl] -side left
if {$ShowAttrib == 1} {
  pack [frame [set attrib .top.attrib]] -side left
}
pack [frame [set status .top.status]] -side right


# create the graphics display window
global Width Height

set dispframe [frame .top.display]

# create a canvas as background to display
set canv .top.display.canv
set args " -width $Width -height $Height "
set args [concat $args " -closeenough 3 -background Black "]
set mkcanv [concat "canvas $canv" $args]
eval $mkcanv


# For use by external procs
global Widget
set Widget(CANVAS) $canv

# now create gwm widget on canvas
set args " -width $Width -height $Height "
set args [concat $args " -tags gwm -name xwindows3 "]
set args [concat $args " -background Black -foreground White "]
set mkgwm [concat "$canv create gwm 0 0 " $args]


global LocalTable
set LocalTable 0
if {[catch $mkgwm] != 0} {
  destroy $canv
  destroy $dispframe
  frame $dispframe -colormap new
  eval $mkcanv
  eval $mkgwm
  set LocalTable 1
}
  
pack $canv
pack $dispframe

#

#
#
# initialise help string
global HelpString
set HelpString " "
label $help.lbl -text "Help:"  \
           -font -*-times-bold-r-normal--*-140-*-*-*-*-*-*
label $help.text -textvariable HelpString -relief sunken  \
           -font -*-times-bold-i-normal--*-140-*-*-*-*-*-* \
           -foreground Blue -background LightYellow
pack $help.lbl -side left -anchor w -ipadx 2m
pack $help.text -side left -anchor w -ipadx 2m
#
#
global ImageName
global PSS
#
#
# initialise PSS dialog globals
#
IsysPssClear

#
#

global Cache Buffer
trace variable Cache w Cache_onoff
trace variable Buffer w Buffer_onoff
  




#
#  define main buttons
menubutton $mbar.file -text "File" -menu $mbar.file.menu -underline 0
menu $mbar.file.menu
$mbar.file.menu add command -label "Open.." -command {
                                            load_img .load_dialogue
                                            if {$ImageOpen==1} {
                                              active_state
                                            }
                                           }
$mbar.file.menu add command -label "Save As.." \
                                -command {save_data .save_dialogue}
$mbar.file.menu add command -label "Import 1D data.." \
                                -command {load_1D_data .load_1D_dialogue}
$mbar.file.menu add command -label "Export 1D data.." \
                                -command {save_1D_data .save_1D_dialogue}
$mbar.file.menu add command -label "Import ARD data.." \
                                -command {load_ARD_data .load_ARD_dialogue}
$mbar.file.menu add command -label "Export ARD data.." \
                                -command {save_ARD_data .save_ARD_dialogue}
$mbar.file.menu add command -label "Import position list.." \
                                -command {load_pos_list .load_pos_dialogue}
$mbar.file.menu add separator
$mbar.file.menu add command -label Exit -command {
                            catch {ImgExecWaitNoMsg igui "action=destroy" }
                            catch {ImgExecWait iclose " " }
                            catch {img kill} 
                            IsysPssClose
                            exit
                           }

HelpText $mbar.file "Load/Save image, or exit system"


menubutton $mbar.opt -text "Options" -menu $mbar.opt.menu -underline 0
menu $mbar.opt.menu
$mbar.opt.menu add checkbutton -label "Auto Display Refresh" \
                   -variable AutoRefresh -onvalue 1 -offvalue 0 
$mbar.opt.menu add checkbutton -label "Auto Save Settings" \
                   -variable AutoSaveSettings -onvalue 1 -offvalue 0
$mbar.opt.menu add checkbutton -label "Show Tips on Startup" \
                   -variable ShowTips -onvalue 1 -offvalue 0
$mbar.opt.menu add checkbutton -label "External 1D/line Plotting" \
                   -variable Ext1DWnd -onvalue 1 -offvalue -0

#$mbar.opt.menu add separator
#$mbar.opt.menu add cascade -label "Catalogue Server" \
#                   -menu $mbar.opt.menu.server
#menu $mbar.opt.menu.server
#$mbar.opt.menu.server add radiobutton -label "LEDAS" \
#                   -variable CatServer -value "LEDAS"
#$mbar.opt.menu.server add radiobutton -label "ESO" \
#                   -variable CatServer -value "ESO"

$mbar.opt.menu add separator

$mbar.opt.menu add cascade -label "Resize Display" \
                   -menu $mbar.opt.menu.resize
menu $mbar.opt.menu.resize
$mbar.opt.menu.resize add radiobutton -label "400 x 300" \
                   -variable DisplaySize -value 1
$mbar.opt.menu.resize add radiobutton -label "500 x 375" \
                   -variable DisplaySize -value 2
$mbar.opt.menu.resize add radiobutton -label "600 x 450" \
                   -variable DisplaySize -value 3
$mbar.opt.menu.resize add radiobutton -label "700 x 525" \
                   -variable DisplaySize -value 4
$mbar.opt.menu.resize add radiobutton -label "800 x 600" \
                   -variable DisplaySize -value 5
$mbar.opt.menu.resize add radiobutton -label "900 x 675" \
                   -variable DisplaySize -value 6
$mbar.opt.menu.resize add radiobutton -label "1000 x 750" \
                   -variable DisplaySize -value 7

trace variable DisplaySize w ChangeDisplay
trace variable CatServer w ChangeCatServer
trace variable AutoRefresh w AutoRefresh_OnOff
trace variable Ext1DWnd w Ext1DWnd_OnOff
trace variable ShowTips w ShowTips_OnOff
trace variable AutoSaveSettings w AutoSaveSettings_OnOff

HelpText $mbar.opt  "Set various options"

menubutton $mbar.view -text "View" -menu $mbar.view.menu -underline 0
menu $mbar.view.menu
$mbar.view.menu add checkbutton -label "Show Messages" \
                   -variable ShowMsg -onvalue 1 -offvalue 0
$mbar.view.menu add checkbutton -label "Attribute Toolbar" \
                   -variable ShowAttrib -onvalue 1 -offvalue 0
$mbar.view.menu add checkbutton -label "Region Toolbar" \
                   -variable ShowShape -onvalue 1 -offvalue 0
$mbar.view.menu add checkbutton -label "Cache/Buffer Toolbar" \
                   -variable ShowCache -onvalue 1 -offvalue 0
trace variable ShowMsg w Msg_OnOff
trace variable ShowAttrib w Attrib_OnOff
trace variable ShowShape w Shape_OnOff
trace variable ShowCache w Cache_OnOff

menubutton $mbar.edit -text "Edit" -menu $mbar.edit.menu -underline 0
menu $mbar.edit.menu
$mbar.edit.menu add command -label "Undo" -command {
                                            ImgExecWait iundo " "
                                            ImgExecWaitNoMsg istats " "
                                            if {$AutoRefresh == 1} {
                                              ImgExecWait idisplay " "
                                            }
                                           }
$mbar.edit.menu add separator
$mbar.edit.menu add command -label "Smooth.." \
                                -command {smooth_data .smooth_dialogue}
$mbar.edit.menu add command -label "Add Noise.." \
                                -command {add_noise .noise_dialogue}
$mbar.edit.menu add cascade -label "Patch.." \
                                -menu $mbar.edit.menu.patch
menu $mbar.edit.menu.patch
$mbar.edit.menu.patch add command -label "AutoPatch" -command {
                                           ImgExecWait ipatch "mode=interp"
                                           ImgExecWaitNoMsg istats " "
                                          }
$mbar.edit.menu.patch add command -label "Cut'N'Paste" -command {
                                           patch_paste .patch_dialogue
                                          }



HelpText $mbar.edit "Make alterations to image or Undo last change"


menubutton $mbar.source -text "Search" -menu $mbar.source.menu \
                                            -underline 0
menu $mbar.source.menu
#$mbar.source.menu add command -label "Background model" -underline 0 -command {
#                                    IsysBgndDialog .bgnd_dialogue
#				  monitor_standard
#                   }

$mbar.source.menu add command -label "Source-search Image" -underline 0 -command {
                                    set PSS(inp) $ImageName
                                    set lx [nbs get $nbid.regxmin]
                                    set hx [nbs get $nbid.regxmax]
                                    set ly [nbs get $nbid.regymin]
                                    set hy [nbs get $nbid.regymax]
                                    set PSS(slice) "${lx}:${hx},${ly}:${hy}"
				    if { [string range $Region 0 4] == "WHOLE" } {
				      set PSS(region) 0
				    } else {
				      set PSS(region) 1
				      }
                                    IsysPssDialog .search_dialogue

                   if {$PSS(finished)} {
                     if {$PSS(mark)} {
                       if [file exists $PSS(out).sdf] {
                         ImgExecWait imark "list=$PSS(out) hdb=n number=$PSS(mark_n) \\"
                         }
                       }
                     }
                   }

#$mbar.source.menu add command -label "Search HEASARC Catalogue" -underline 7 -command {
#                   set lx [nbs get $nbid.regxmin]
#                   set hx [nbs get $nbid.regxmax]
#                   set wtodeg [nbs get $nbid.wtodeg]
#                   set wid [expr abs($hx - $lx)*1.4* $wtodeg *60.0]
#                   set ra1950 [nbs get $nbid.ra1950]
#                   set dec1950 [nbs get $nbid.dec1950]
#                   IsysHdbDialog .cat_dialogue $ra1950 $dec1950 $wid
#                   if {$HDB(finished) && $HDB(mark) } {
#                     if [file exists $HDB(file)] {
#                       ImgExecWait imark "list=$HDB(file) hdb=y number=$HDB(mark_n) \\"
#                       }
#                     }
#                   }

$mbar.source.menu add command -label "Search Remote Catalogues" -underline 7 -command {
                   IsysCursaInit
                   IsysCursa .cursa_dialogue
}

$mbar.source.menu add command -label "Retrieve Archive Images" -underline 12 -command {
                   set lx [nbs get $nbid.regxmin]
                   set hx [nbs get $nbid.regxmax]
                   set wtodeg [nbs get $nbid.wtodeg]
                   set wid [expr abs($hx - $lx)*1.4* $wtodeg]
                   set ra1950 [nbs get $nbid.ra1950]
                   set dec1950 [nbs get $nbid.dec1950]
                   IsysSkyviewDialog .sky_dialog $ra1950 $dec1950 $wid
                   }


HelpText $mbar.source "Search image or catalogue for sources"

menubutton $mbar.mark -text "Mark" -menu $mbar.mark.menu -underline 0
menu $mbar.mark.menu
$mbar.mark.menu add command -label "Current position" -command {
                            ImgExecWait imark \
                               "all=n curr=y symbol=2 size=3.0 accept"
                            ImgExecWaitNoMsg igui "action=update"
                           }

$mbar.mark.menu add command -label "Current list" -command {
                                    mark_list .mark_dialogue
                                   }


$mbar.mark.menu add command -label "Remove marks" -command {
                                      ImgExecWaitNoMsg igui \
                                         "action=synchro value=twod"
                                      ImgExecWait gset "switch=mark cancel=y"
                                      ImgExecWaitNoMsg igui "action=update"
                                      if {$AutoRefresh == 1} {
                                        ImgExecWait idisplay " "
                                      }
                                     }

HelpText $mbar.mark "Mark positions on image"

#
menubutton $mbar.profile -text "Profiles" -menu $mbar.profile.menu \
                                                  -underline 0
menu $mbar.profile.menu
$mbar.profile.menu add command -label Slice -command {
                            if {$Ext1DWnd == 1} {
                              nbs put $nbid.options "E"
                            } else {
                              nbs put $nbid.options "I"
                            }
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                                   {slice_form .slice_dialogue}
                            ImgExecWait islice " "
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                            if {$Ext1DWnd == 1} {
                              ImgExecWait isave1d "out=$File1D"
                              catch {exec gdraw "inp=$File1D dev=x4win" &}
                            }
                           }
$mbar.profile.menu add command -label Radial -command {
                            if {$Ext1DWnd == 1} {
                              nbs put $nbid.options "E"
                            } else {
                              nbs put $nbid.options "I"
                            }
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                               {circle_form .circle_dialogue}
                            ImgExecWait iradial " "
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                            if {$Ext1DWnd == 1} {
                              ImgExecWait isave1d "out=$File1D"
                              catch {exec gdraw "inp=$File1D dev=x4win" &}
                            }
                           }

$mbar.profile.menu add command -label Azimuthal -command {
                            if {$Ext1DWnd == 1} {
                              nbs put $nbid.options "E"
                            } else {
                              nbs put $nbid.options "I"
                            }
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                                {circle_form .circle_dialogue}
                            ImgExecWait iazimuth "nbin=12"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                            if {$Ext1DWnd == 1} {
                              ImgExecWait isave1d "out=$File1D"
                              catch {exec gdraw "inp=$File1D dev=x4win" &}
                            }
                           }
$mbar.profile.menu add command -label Z-profile -command {
                            if {$Ext1DWnd == 1} {
                              nbs put $nbid.options "E"
                            } else {
                              nbs put $nbid.options "I"
                            }
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                                {circle_form .circle_dialogue}
                            ImgExecWait ized " "
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                            if {$Ext1DWnd == 1} {
                              ImgExecWait isave1d "out=$File1D"
                              catch {exec gdraw "inp=$File1D dev=x4win" &}
                            }
                           }
$mbar.profile.menu add separator
$mbar.profile.menu add command -label "Export to QDP" -command {
    set file /tmp/1d_[pid]
    set qfile /tmp/1d_[pid].qdp
    set qprog [IsysGetXanProgDialog .get_qdp qdp "QDP"]
    if { $qprog != "" } {
      . configure -cursor watch
      ImgExecWait isave1d "out=$file"
      exec $env(AST_BIN)/ast2qdp $file $qfile
      if { ! $CreatedXanPgServer } {
        set sprog [IsysGetXanProgDialog .get_pgx pgxwin_server "XANADU pgplot server"]
        exec $sprog -win_maxColors 8 -win_geometry 700x450+150-180 &
        set CreatedXanPgServer 1
        }
      exec xterm -geometry 80x8+200-0 -ls -e $qprog $qfile /xw
      exec rm -f $file $qfile
      . configure -cursor arrow
      }
    }

HelpText $mbar.profile "Extract 1D profiles and optionally export to QDP"

#
menubutton $mbar.reg -text "Regions" -menu $mbar.reg.menu -underline 0
menu $mbar.reg.menu
$mbar.reg.menu add command -label Box -command {
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                                {box_form .box_dialogue}
                            ImgExecWait iregion "mode=box "
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                           }

$mbar.reg.menu add command -label Slice -command {
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                               {slice_form .slice_dialogue}
                            ImgExecWait iregion "mode=slice "
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                           }

$mbar.reg.menu add command -label Circle -command {
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3> \
                               {circle_form .circle_dialogue}
                            ImgExecWait iregion "mode=circ"
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                           }

$mbar.reg.menu add command -label Annulus -command {
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            bind $canv <ButtonPress-3>  \
                               {annulus_form .annulus_dialogue}
                            ImgExecWait iregion "mode=ann"
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                           }

$mbar.reg.menu add command -label Ellipse -command {
                            $canv configure -cursor crosshair
                            bind $canv <ButtonPress-1> {put_pos %x %y}
                            ImgExecWait iregion "mode=ellipse "
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                            $canv configure -cursor arrow
                            bind $canv <ButtonPress-1> {}
                            bind $canv <ButtonPress-3> {}
                           }

$mbar.reg.menu add separator
$mbar.reg.menu add command -label Whole -command {
                            ImgExecWait iregion "mode=whole"
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                           }
$mbar.reg.menu add separator
$mbar.reg.menu add command -label Show -command {
                            ImgExecWait iregion "mode=show"
                           }
$mbar.reg.menu add separator
$mbar.reg.menu add command -label Remove -command {
                            ImgExecWait iexclude "mode=reg curr=y"
                            ImgExecWaitNoMsg iregion "mode=whole"
                            ImgExecWaitNoMsg istats " "
                            ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                           }
HelpText $mbar.reg "Define regions"

menubutton $mbar.zoom -text "Zoom" -menu $mbar.zoom.menu -underline 0
menu $mbar.zoom.menu
$mbar.zoom.menu add command -label "x 1.0" -command {
                                     ImgExecWait izoom "mode=off"
                                    }
$mbar.zoom.menu add command -label "x 1.5" -command {
                                     ImgExecWait izoom "mode=fac factor=1.5"
                                    }
$mbar.zoom.menu add command -label "x 2.0" -command {
                                     ImgExecWait izoom "mode=fac factor=2.0"
                                    }
$mbar.zoom.menu add command -label "x 5.0" -command {
                                     ImgExecWait izoom "mode=fac factor=5.0"
                                    }
$mbar.zoom.menu add command -label "x 10.0" -command {
                                     ImgExecWait izoom "mode=fac factor=10.0"
                                    }
$mbar.zoom.menu add command -label "x 20.0" -command {
                                     ImgExecWait izoom "mode=fac factor=20.0"
                                    }
$mbar.zoom.menu add command -label "Region" -command {
                                     ImgExecWait izoom "mode=reg "
                                    }
HelpText $mbar.zoom \
   "Zoom in on current position or current region"

menubutton $mbar.help -text "Help" -menu $mbar.help.menu -underline 0
menu $mbar.help.menu
$mbar.help.menu add command -label "On-line HTML Guide" -command {
                                                   online_guide .online_guide
}
$mbar.help.menu add command -label "On-line Guide setup" -command {
                                                   guide_setup .guide_setup
}
$mbar.help.menu add separator
$mbar.help.menu add command -label "About Isys..." -command {
                                                   help_about .about_dialogue
                                                  }
$mbar.help.menu add separator
$mbar.help.menu add command -label "Show Tips..." -command {
                                                       show_tips .tips_dialogue
                                                      }



pack $mbar.file  $mbar.edit $mbar.opt $mbar.view $mbar.source \
              $mbar.profile $mbar.reg $mbar.mark $mbar.zoom -side left
pack $mbar.help -side right 

tk_menuBar $mbar $mbar.file $mbar.opt $mbar.source  $mbar.profile \
                 $mbar.reg $mbar.mark $mbar.zoom  $mbar.help


button $tbar.disp -text "Image" -relief raised \
                                     -command {
                                               ImgExecWait idisplay " "  
                                              }
HelpText $tbar.disp "Display image"

button $tbar.smooth -text Smooth -relief raised \
                      -command {smooth_data .smooth_dialogue}
HelpText $tbar.smooth "Smooth data"

button $tbar.plot -text "1D-Plot" -relief raised \
                            -command {
                                 if {$Ext1DWnd == 1} {
                                 ImgExecWait isave1d "out=$File1D"
                                 catch {exec gdraw "inp=$File1D dev=x4win" &}
                                } else {
                                        ImgExecWait iplot " "
                                       }
                                      }
HelpText $tbar.plot "Plot 1D products"

button $tbar.browse -text "Browse" -command {browse_data .top.browse_dialogue }
HelpText $tbar.browse "Browse data values"
#
button $tbar.contour -text "Contours" -command {set_contour .contour_dialogue}
HelpText $tbar.contour "Set contour levels"
#
button $tbar.scale -text "Scaling" -command {set_scale .scale_dialogue}
HelpText $tbar.scale "Set scaling algorithm for pixel display"
#
#
#
button $tbar.setpos -text "SetPos" -command {
                                             $canv configure -cursor crosshair
                                             monitor_setpos
                                             nbs put $nbid.flag 1
                                             bind $canv <Enter> \
                                                {nbs put $nbid.flag 0}
                                             bind $canv <Motion> \
                                               "pos_update %x %y"
                                             nbs put $nbid.options "TRACK"
                                             bind $canv <ButtonPress-1> \
                                                {nbs put $nbid.flag 1}
                                             bind $canv <ButtonPress-3> \
                                                {posit_form .posit_dialogue}
                                             ImgExecWait iposit "mode=point"
                                             bind $canv <Enter> {}
                                             bind $canv <Motion> {}
                                             bind $canv <ButtonPress-1> {}
                                             bind $canv <ButtonPress-3> {}
                                             monitor_standard
                                             $canv configure -cursor arrow
                                            }
bind $canv <ButtonPress-2> {
                            pos_update %x %y
                            nbs put $nbid.options "POINT"
                            nbs put $nbid.flag 1
                            ImgExecWait iposit "mode=point"
                           }
bind $tbar.setpos <ButtonPress-3> {
                                    $tbar.setpos configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    posit_form .posit_dialogue
                                   }
bind $tbar.setpos <ButtonRelease-3> {
                                      $tbar.setpos configure -relief raised
                                      ImgExecWait iposit "mode=point"
                                     }
HelpText $tbar.setpos  "Set current position"
#
#
button $tbar.centroid -text "Centroid" -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  bind $canv <ButtonPress-3> {circle_form .circle_dialogue}
                  ImgExecWait icentroid "iter=5"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                  bind $canv <ButtonPress-3> {}
                 }
bind $tbar.centroid <ButtonPress-3> {
                                    $tbar.centroid configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    circle_form .circle_dialogue
                                   }
bind $tbar.centroid <ButtonRelease-3> {
                                      $tbar.centroid configure -relief raised
                                      $tbar.centroid invoke
                                      nbs put $nbid.options " "
                                     }
HelpText $tbar.centroid  "Find centroid of given circle"
#
#

#
pack $tbar.disp  $tbar.plot $tbar.scale $tbar.contour $tbar.smooth  \
     $tbar.browse $tbar.setpos $tbar.centroid \
       -side left

#
# define shape toolbar
global reg_and reg_not reg_add reg_pre
set reg_and 0
set reg_not 0
set reg_add 0
set reg_pre ""

proc reg_logic {} {
  global reg_and reg_not reg_add reg_pre

  set reg_pre ""
  if {$reg_add == 1} {
    if {$reg_not == 1} {
      set reg_pre "addnot"
    } else {
      set reg_pre "add"
    }
  } elseif {$reg_and == 1} {
    if {$reg_not == 1} {
      set reg_pre "andnot"
    } else {
      set reg_pre "and"
    }
  } else {
    if {$reg_not == 1} {
      set reg_pre "not"
    }
  }
}

button $stbar.box -bitmap @$env(AST_ETC)/box.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  bind $canv <ButtonPress-3> {box_form .box_dialogue}
                  ImgExecWait iregion "${reg_pre}box"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                  bind $canv <ButtonPress-3> {}
                 } 
bind $stbar.box <ButtonPress-3> {
                                    $stbar.box configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    box_form .box_dialogue
                                   }
bind $stbar.box <ButtonRelease-3> {
                                      $stbar.box configure -relief raised
                                      $stbar.box invoke
                                      nbs put $nbid.options " "
                                     }
DynamicButton $stbar.box region "Define rectangular box"

button $stbar.slice -bitmap @$env(AST_ETC)/slice.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  bind $canv <ButtonPress-3> {slice_form .slice_dialogue}
                  ImgExecWait iregion "${reg_pre}slice"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                  bind $canv <ButtonPress-3> {}
                 } 
bind $stbar.slice <ButtonPress-3> {
                                    $stbar.slice configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    slice_form .slice_dialogue
                                   }
bind $stbar.slice <ButtonRelease-3> {
                                      $stbar.slice configure -relief raised
                                      $stbar.slice invoke
                                      nbs put $nbid.options " "
                                     }
DynamicButton $stbar.slice region "Define arbitrary rectangular slice"

button $stbar.poly -bitmap @$env(AST_ETC)/poly.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  ImgExecWait iregion "${reg_pre}poly"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                 } 
DynamicButton $stbar.poly region "Define arbitrary polygon"

button $stbar.circle -bitmap @$env(AST_ETC)/circle.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  bind $canv <ButtonPress-3> {circle_form .circle_dialogue}
                  ImgExecWait iregion "${reg_pre}circle"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                  bind $canv <ButtonPress-3> {}
                 } 
bind $stbar.circle <ButtonPress-3> {
                                    $stbar.circle configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    circle_form .circle_dialogue
                                   }
bind $stbar.circle <ButtonRelease-3> {
                                      $stbar.circle configure -relief raised
                                      $stbar.circle invoke
                                      nbs put $nbid.options " "
                                     }
DynamicButton $stbar.circle region "Define circular region"

button $stbar.annulus -bitmap @$env(AST_ETC)/annulus.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  bind $canv <ButtonPress-3> {annulus_form .annulus_dialogue}
                  ImgExecWait iregion "${reg_pre}annulus"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                  bind $canv <ButtonPress-3> {}
                 } 
bind $stbar.annulus <ButtonPress-3> {
                                    $stbar.annulus configure -relief sunken
                                    nbs put $nbid.options "FORM"
                                    nbs put $nbid.flag 0
                                    update idletasks
                                    annulus_form .annulus_dialogue
                                   }
bind $stbar.annulus <ButtonRelease-3> {
                                      $stbar.annulus configure -relief raised
                                      $stbar.annulus invoke
                                      nbs put $nbid.options " "
                                     }
DynamicButton $stbar.annulus region "Define annular region"

button $stbar.ellipse -bitmap @$env(AST_ETC)/ellipse.xbm \
        -command {
                  $canv configure -cursor crosshair
                  bind $canv <ButtonPress-1> {put_pos %x %y}
                  ImgExecWait iregion "${reg_pre}ellipse"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                  $canv configure -cursor arrow
                  bind $canv <ButtonPress-1> {}
                 } 
DynamicButton $stbar.ellipse region "Define elliptical region"

button $stbar.contour -bitmap @$env(AST_ETC)/contour.xbm
DynamicButton $stbar.contour region "Define region within given contour"

button $stbar.add -bitmap @$env(AST_ETC)/or.xbm \
        -command {
                  if {$reg_add == 0} {
                    $stbar.add configure -relief sunken
                    set reg_add 1
                    if {$reg_and == 1} {
                      $stbar.and configure -relief raised
                      set reg_and 0
                    }
                  } else {
                    $stbar.add configure -relief raised
                    set reg_add 0
                  }
                  reg_logic
                }
StaticButton $stbar.add region "Accumulate regions"

button $stbar.and -bitmap @$env(AST_ETC)/and.xbm \
        -command {
                  if {$reg_and == 0} {
                    $stbar.and configure -relief sunken
                    set reg_and 1
                    if {$reg_add == 1} {
                      $stbar.add configure -relief raised
                      set reg_add 0
                    }
                  } else {
                    $stbar.and configure -relief raised
                    set reg_and 0
                  }
                  reg_logic
                }
StaticButton  $stbar.and region "Take overlap of regions"

button $stbar.not -bitmap @$env(AST_ETC)/not.xbm \
        -command {
                  if {$reg_not == 0} {
                    $stbar.not configure -relief sunken
                    set reg_not 1
                  } else {
                    $stbar.not configure -relief raised
                    set reg_not 0
                  }
                  reg_logic
                }
StaticButton  $stbar.not region "Use region outside given shape"

button $stbar.cut -bitmap @$env(AST_ETC)/cut.xbm \
        -command {
                  ImgExecWait iexclude "mode=reg curr=y"
                  ImgExecWait iregion "mode=whole"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                 } 
DynamicButton $stbar.cut region "Remove current region from image"

button $stbar.show -bitmap @$env(AST_ETC)/show.xbm \
        -command {
                  ImgExecWait iregion "mode=show"
                 } 
DynamicButton $stbar.show region "Show outline of current region"

button $stbar.whole -bitmap @$env(AST_ETC)/whole.xbm \
        -command {
                  ImgExecWait iregion "mode=whole"
                  ImgExecWaitNoMsg istats " "
                  ImgExecWaitNoMsg iregion "mode=exp file=$FileARD"
                 } 
DynamicButton $stbar.whole region "Reselect whole image"


label $stbar.dummy1 -text " "
label $stbar.dummy2 -text " "

pack $stbar.box $stbar.slice $stbar.poly $stbar.circle \
        $stbar.annulus $stbar.ellipse $stbar.contour \
         $stbar.dummy1 $stbar.add $stbar.and $stbar.not \
          $stbar.dummy2 $stbar.cut $stbar.show $stbar.whole \
             -side left

# Cache and Buffer toolbar
#
button $cbar.undo -bitmap @$env(AST_ETC)/undo.xbm -relief flat \
        -command {
                  ImgExecWait iundo " "
                  ImgExecWaitNoMsg istats " "
                  if {$AutoRefresh == 1} {
                    ImgExecWait idisplay " "
                  }
                 } 
DynamicButton $cbar.undo cache "Undo last changes made to image"

button $cbar.push -bitmap @$env(AST_ETC)/push.xbm -relief flat \
        -command {
                  ImgExecWait icache "mode=push "
                  ImgExecWaitNoMsg istats " "
                 } 
DynamicButton $cbar.push cache "Copy current image into cache memory"

button $cbar.pop -bitmap @$env(AST_ETC)/pop.xbm -relief flat \
        -command {
                  ImgExecWait icache "mode=pop "
                  ImgExecWaitNoMsg istats " "
                  if {$AutoRefresh == 1} {
                    ImgExecWait idisplay " "
                  }
                 } 
DynamicButton $cbar.pop cache "Make cached image the current image"

button $cbar.toggle -bitmap @$env(AST_ETC)/toggle.xbm -relief flat \
        -command {
                  ImgExecWait icache "mode=toggle "
                  ImgExecWaitNoMsg istats " "
                  if {$AutoRefresh == 1} {
                    ImgExecWait idisplay " "
                  }
                 } 
DynamicButton $cbar.toggle cache "Toggle current image and cached image"

button $cbar.clear -bitmap @$env(AST_ETC)/clear.xbm -relief flat \
        -command {
                  ImgExecWait icache "mode=clear "
                 } 
DynamicButton $cbar.clear cache "Clear image from cache memory"


button $cbar.add -bitmap @$env(AST_ETC)/add.xbm -relief flat \
        -command {
                  ImgExecWait iarith "oper=ADD cache=y"
                  ImgExecWaitNoMsg istats " "
                  if {$AutoRefresh == 1} {
                    ImgExecWait idisplay " "
                  }
                 } 
DynamicButton $cbar.add cache "Add cached image to current image"

button $cbar.subtract -bitmap @$env(AST_ETC)/subtract.xbm -relief flat \
        -command {
                  ImgExecWait iarith "oper=SUB cache=y"
                  ImgExecWaitNoMsg istats " "
                  if {$AutoRefresh == 1} {
                    ImgExecWait idisplay " "
                  }
                 } 
DynamicButton $cbar.subtract cache "Subtract cached image from current image"

label $cbar.dummy1 -text " "
label $cbar.dummy2 -text " "

pack $cbar.undo $cbar.dummy1 $cbar.push $cbar.pop $cbar.toggle \
      $cbar.clear $cbar.dummy2 $cbar.add $cbar.subtract -side left


#
# Region, position and stats.
#
pack [frame $status.reg -relief sunken -bd 2] \
     [frame $status.pos -relief sunken -bd 2] \
     [frame $status.stat -relief sunken -bd 2] \
     [frame $status.qual -relief sunken -bd 2] -side top -fill both -expand 1


pack [label $status.reg.lbl -text "Region:"] -side top -anchor nw
pack [label $status.reg.type -textvariable Region] -side top

pack [label $status.pos.lbl -text "Position:"] -side top -anchor nw
bind $status.pos.lbl <ButtonPress-1> {
     ImgExecWait imark "all=n curr=y symbol=2 size=3.0 accept"
    }
HelpText $status.pos "Click on the title in this box to mark this position"
pack [frame $status.pos.pixel] -side top
pack [frame $status.pos.xworld] -side top
pack [frame $status.pos.yworld] -side top
pack [frame $status.pos.ra] -side top
pack [frame $status.pos.dec] -side top

pack [label $status.stat.lbl -text "Statistics:"] -side top -anchor nw
pack [frame $status.stat.sum] -side top
pack [frame $status.stat.min] -side top
pack [frame $status.stat.max] -side top
pack [frame $status.stat.mean] -side top
pack [frame $status.stat.merr] -side top

pack [label $status.qual.lbl -text "Quality:"] -side top -anchor nw
pack [frame $status.qual.good] -side top
pack [frame $status.qual.bad] -side top

label $status.pos.pixel.pixel_lbl -text Pixel
label $status.pos.pixel.curr_pixel -textvariable Pixel -width 10
label $status.pos.xworld.xworld_lbl -text X
label $status.pos.xworld.curr_xworld -textvariable Xworld -width 10
label $status.pos.yworld.yworld_lbl -text Y
label $status.pos.yworld.curr_yworld -textvariable Yworld -width 10
pack $status.pos.pixel.pixel_lbl -side left
pack $status.pos.pixel.curr_pixel -side right
pack $status.pos.xworld.xworld_lbl -side left
pack $status.pos.xworld.curr_xworld -side right
pack $status.pos.yworld.yworld_lbl -side left
pack $status.pos.yworld.curr_yworld -side right
label $status.pos.ra.ra_lbl -text RA
label $status.pos.ra.curr_ra -textvariable Ra -width 12
label $status.pos.dec.dec_lbl -text DEC
label $status.pos.dec.curr_dec -textvariable Dec -width 12
pack $status.pos.ra.ra_lbl -side left
pack $status.pos.ra.curr_ra -side right
pack $status.pos.dec.dec_lbl -side left
pack $status.pos.dec.curr_dec -side right

label $status.stat.sum.sum_lbl -text "Sum"
label $status.stat.sum.curr_sum -textvariable Sum -width 10
label $status.stat.min.min_lbl -text "Min"
label $status.stat.min.curr_min -textvariable Min -width 10
label $status.stat.max.max_lbl -text "Max"
label $status.stat.max.curr_max -textvariable Max -width 10
label $status.stat.mean.mean_lbl -text "Mean"
label $status.stat.mean.curr_mean -textvariable Mean -width 10
label $status.stat.merr.merr_lbl -text " +/-"
label $status.stat.merr.curr_merr -textvariable Merr -width 10

pack $status.stat.sum.sum_lbl -side left
pack $status.stat.sum.curr_sum -side right
pack $status.stat.min.min_lbl -side left
pack $status.stat.min.curr_min -side right
pack $status.stat.max.max_lbl -side left
pack $status.stat.max.curr_max -side right
pack $status.stat.mean.mean_lbl -side left
pack $status.stat.mean.curr_mean -side right
pack $status.stat.merr.merr_lbl -side left
pack $status.stat.merr.curr_merr -side right

label $status.qual.good.lbl -text "Good"
label $status.qual.good.ngood -textvariable Ngood -width 10
label $status.qual.bad.lbl -text "Bad"
label $status.qual.bad.nbad -textvariable Nbad -width 10

pack $status.qual.good.lbl -side left
pack $status.qual.good.ngood -side right
pack $status.qual.bad.lbl -side left
pack $status.qual.bad.nbad -side right

global Active_GCB
set Active_GCB 0
menubutton $attrib.change  -menu $attrib.change.menu \
  -relief raised  -bitmap @$env(AST_ETC)/image.xbm
menu $attrib.change.menu
$attrib.change.menu add command -label "Image display attributes" -command {
    set Active_GCB 2
    $attrib.change configure  -bitmap @$env(AST_ETC)/image.xbm
}
$attrib.change.menu add command -label "1D plot attributes" -command {
    set Active_GCB 1
    $attrib.change configure  -bitmap @$env(AST_ETC)/1dplot.xbm
}
#trace variable Active_GCB w change_GCB

button $attrib.def -text "Defaults"  -command { set_def .def_dialogue}
HelpText $attrib.def "Set default plotting attributes "
#
button $attrib.win -text Window -command {set_win .win_dialogue}
HelpText $attrib.win "Set position of plotting window "

button $attrib.axes -text Axes -command {set_axes .axes_dialogue}
HelpText $attrib.axes "Set axis format and attributes "

button $attrib.labels -text Labels -command {set_labels .labels_dialogue}
HelpText $attrib.labels "Set axis labels"
$attrib.labels configure -state disabled

button $attrib.titles -text Titles -command {set_title .title_dialogue}
HelpText $attrib.titles "Set title lines above plot"

button $attrib.annotate -text Annotate -command {set_note .note_dialogue}
HelpText $attrib.annotate "Annotate plot with arbitrary text"

button $attrib.format -text Format
HelpText $attrib.format "Not yet implemented "
$attrib.format configure -state  disabled

button $attrib.grid -text Grid -command {set_grid .grid_dialogue}
HelpText $attrib.grid "Set up coordinate grid "

button $attrib.key -text Key -command {set_key .key_dialogue}
HelpText $attrib.key "Put various keys on plot "

button $attrib.col -text Colours -command {edit_cols .col_dialogue}
HelpText $attrib.col "Change colour table"

button $attrib.reset -text Reset -command {
             ImgExecWaitNoMsg igui "action=synchro value=twod"
             ImgExecWaitNoMsg gset "switch=all cancel=yes"
             ImgExecWaitNoMsg igui "action=update"
             ImgExecWait gset "switch=pix scaling=lin accept"
             ImgExecWaitNoMsg igui "action=update"
             ImgExecWaitNoMsg igui "action=cache value=twod"
            }
  $attrib.reset configure -state disabled
HelpText $attrib.reset "Reset all plotting control and attributes"
#
#
pack  $attrib.def $attrib.win $attrib.axes  $attrib.titles\
     $attrib.labels $attrib.annotate $attrib.grid $attrib.key \
     $attrib.col -side top -fill both -expand 1
#
#
#
. configure -cursor watch
#
#
# load task and wait for communication path to be established
adamtask img $env(AST_BIN)/grf_mono
while {[img path]==0} {
  after 100
}
#
update idletasks
# initialise GCB system
ImgExecWaitNoMsg igui "action=init value=gcb"
update idletasks
#
#
# get task to create noticeboard for communication
#
set nbid img[pid]
ImgExecWaitNoMsg igui "action=create value=$nbid"
#
update idletasks
#
#
#
update idletasks
# open the graphic display
ImgExecWaitNoMsg gdevices "open=yes dev=x3win"
update idletasks
#
# make graphic display size available to applications
nbs put $nbid.xpmax $Width
nbs put $nbid.ypmax $Height
#
#
# see if image name has been supplied on command line
if {$argc > 0} {
  set ImageName [lindex $argv 0]
  ImgExecWaitNoMsg icheck "inp=$ImageName"
  set shape [nbs get $nbid.options]
  set shape [string trim $shape]
  if {$shape == "CUBE"} {
    nbs put $nbid.options "WHOLE"
  }
  ImgExecWait iload "inp=$ImageName gcb=y disp=y"
  wm title . "Isys: $ImageName"
  ImgExecWaitNoMsg istats "suppress=yes"
  ImgExecWaitNoMsg igui "action=update"
  active_state
  . configure -cursor arrow
  monitor_standard
} else {
# if not then suppress buttons
  startup_state
}
. configure -cursor arrow

#
#
update idletasks
pack .top
wm deiconify .
update idletasks
if {$ShowTips == 1} {
  show_tips .tips_dialogue
}
#
if {($ColourWarn == 1) && ($LocalTable == 1)} {
  colour_warning .colour_warning
}
#
#------------------------------------------------------------------------

#
#
#
proc crossHair {name element op} {
  global crosshair
  global canv
  if $crosshair {
    bind $canv <Any-Motion> {$canv set crosshair %x %y}
    bind $canv <Any-Enter> { %W configure -crosshair yes}
    bind $canv <Any-Leave> { %W configure -crosshair no}
  } {
    bind $canv <Any-Motion> {}
    bind $canv <Any-Enter> {}
    bind $canv <Any-Leave> {}
    $xwin configure -crosshair no
  }
}
#
#
proc set_iomode {name element op } {
  global iomode
  if {$iomode==1} {
    ImgExecWait imode "curs=yes key=no" 
  } elseif {$iomode==2} {
    ImgExecWait imode "curs=no key=yes" 
  }
}
#

#
#
#
#
#
#
#
#
#
proc set_win {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global AutoRefresh
  global nbid
  global canv
  global Width Height
  global X1 X2 Y1 Y2
  global Top

  set Top $w
  toplevel $w -class Dialog
  wm title $w "Window Position"
  wm iconname $w Posit

  pack [frame $w.top] -side top -fill both
  pack [frame $w.bot] -side bottom -fill both


  button $w.bot.ok -text Ok -command {
     if {$X1<=$X2} {
       set x1 double($X1)
       set x2 double($X2)
     } else {
       set x1 double($X2)
       set x2 double($X1)
     }
     if {$Y1>=$Y2} {
       set y1 double($Y1)
       set y2 double($Y2)
     } else {
       set y1 double($Y2)
       set y2 double($Y1)
     }
# convert from Tcl pixels to NDC
     set x1 [expr $x1/$Width]
     set x2 [expr $x2/$Width]
     set y1 [expr 1.0-($y1/$Height)]
     set y2 [expr 1.0-($y2/$Height)]
     ImgExecWait gset \
     "switch=pos x1=$x1 x2=$x2 y1=$y1 y2=$y2 \
           accept" 
     ImgExecWaitNoMsg igui "action=update" 
     ImgExecWaitNoMsg igui "action=cache value=twod"
     if {$AutoRefresh == 1} {
       ImgExecWait idisplay " "
     }
     set Ok 1}

  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }

  button $w.bot.reset -text Reset -command {ImgExecWait gset \
                                     "switch=pos off=yes" 
                             ImgExecWaitNoMsg igui "action=update"
                             ImgExecWaitNoMsg igui "action=cache value=twod"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             set Ok 2}

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m

  global HelpString
  set HelpString "Click and drag to define window"
  bind $canv <Any-Enter> {set HelpString "Click and drag to define window"}

  $canv bind gwm <ButtonPress-1> {startBox %x %y}
  $canv configure -cursor crosshair

  tkwait variable Ok
  $canv bind gwm <ButtonPress-1> {}
  bind $canv <Any-Enter> {}
  bind $canv <ButtonPress-1> {}
  set HelpString " "
  destroy $w
  deleteBox 
}
#
#
#
proc get_ch {w siz value} {
  upvar $siz size

  set size [expr $value/100.0]
  $w set $value
}
#
proc get_lw {w wid value} {
  upvar $wid width

  set width $value
  $w set $value
}
#


proc startBox {x y} {
  global canv
  global Top
  global X1 X2 Y1 Y2

  raise $Top .top

  $canv create line $x $y $x $y -tags box -fill white -smooth no
  $canv bind gwm <Motion> {sizeBox %x %y}
  $canv bind gwm <ButtonRelease-1> {endBox}
#  bind $canv <ButtonRelease-1> {endBox}
  $canv bind gwm <Leave> {leaveBox}
#  global x1 y1
  set X1 $x
  set Y1 $y
}

proc endBox {} {
  global canv
  global Top

  raise $Top .top
  update idletasks

  setBox
  $canv bind gwm <Motion> {}
  bind $canv <Motion> {}
  bind $canv <ButtonRelease-1> {}
  bind $canv <ButtonPress-1> {}
  $canv bind gwm <Leave> {}
  $canv bind gwm <Enter> {}
  $canv bind gwm <ButtonPress-1> {}
  $canv bind gwm <ButtonRelease-1> {}
  $canv bind box <ButtonPress-1> {startDrag %x %y}
  $canv bind box <ButtonPress-3> {deleteBox}
  $canv bind corner1 <ButtonPress-1> {resizeBox1}
  $canv bind corner2 <ButtonPress-1> {resizeBox2}
  $canv bind corner3 <ButtonPress-1> {resizeBox3}
  $canv bind corner4 <ButtonPress-1> {resizeBox4}
  $canv bind box <Enter> {$canv configure -cursor hand2}
  $canv bind box <Leave> {$canv configure -cursor arrow}
  foreach i {corner1 corner2 corner3 corner4} {
    $canv bind $i <Enter> {$canv configure -cursor fleur}
    $canv bind $i <Leave> {$canv configure -cursor arrow}
  }
  $canv configure -cursor arrow

  global HelpString
  set HelpString "Drag corners to resize - drag sides to move"
  bind $canv <Any-Enter> {set HelpString\
         "Drag corners to resize - drag sides to move"}
}

proc leaveBox {} {
  global canv
  $canv bind gwm <Motion> {}
  $canv bind gwm <Enter> {enterBox}
}


proc enterBox {} {
  global canv
  $canv bind gwm <Motion> {sizeBox %x %y}
}


proc sizeBox {x y} {
  global canv
  Box $canv box $x $y
}


proc resizeBox1 {} {
  global canv
  global X1 X2 Y1 Y2
  set X1 $X2
  set Y1 $Y2
  resizeBox
}

proc resizeBox2 {} {
  global canv
  global X1 X2 Y1 Y2
  set Y1 $Y2
  resizeBox
}

proc resizeBox3 {} {
  global canv
  global X1 X2 Y1 Y2
  resizeBox
}

proc resizeBox4 {} {
  global canv
  global X1 X2 Y1 Y2
  set X1 $X2
  resizeBox
}


proc resizeBox {} {
  global canv
  $canv bind box <Leave> {}
  $canv bind box <Enter> {}
  foreach i {corner1 corner2 corner3 corner4} {
    $canv bind $i <Enter> {}
    $canv bind $i <Leave> {}
  }
  $canv configure -cursor fleur
  bind $canv <Motion> {sizeBox %x %y}
  bind $canv <ButtonRelease-1> {endBox}
  $canv bind gwm <Leave> {leaveBox}
}



proc startDrag {x y} {
  global canv
  $canv configure -cursor hand2
  $canv bind box <Leave> {}
  $canv bind box <Enter> {}
  global x1 y1
  set x1 $x
  set y1 $y
  bind $canv <Motion> {dragBox %x %y}
  bind $canv <ButtonRelease-1> {endDrag}
}

proc dragBox {x y} {
  global canv
  global x1 y1
  set xshift [expr $x-$x1]
  set yshift [expr $y-$y1]
  moveBox $canv box $xshift $yshift
  set x1 $x
  set y1 $y
}
 
proc endDrag {} {
  global canv
  global Top
  raise $Top .top
  update idletasks

  setBox
  endBox
  bind $canv <Motion> {}
  $canv configure -cursor arrow
  $canv bind box <Enter> {$canv configure -cursor hand2}
  $canv bind box <Leave> {$canv configure -cursor arrow}
} 


proc deleteBox {} {
  global canv
  $canv delete box
  foreach i {corner1 corner2 corner3 corner4} {
    $canv delete $i
  }
  $canv configure -cursor arrow
}


proc Box {c item x y} {
  global X1 X2 Y1 Y2
  set X2 $x
  set Y2 $y
  lappend vertices $X1 $Y1
  lappend vertices $X2 $Y1
  lappend vertices $X2 $Y2
  lappend vertices $X1 $Y2
  lappend vertices $X1 $Y1
  return [eval "$c coords $item $vertices"]
}



proc moveBox {c item dx dy} {
  global X1 X2 Y1 Y2
  set X1 [expr $X1+$dx]
  set X2 [expr $X2+$dx]
  set Y1 [expr $Y1+$dy]
  set Y2 [expr $Y2+$dy]
  lappend vertices $X1 $Y1
  lappend vertices $X2 $Y1
  lappend vertices $X2 $Y2
  lappend vertices $X1 $Y2
  lappend vertices $X1 $Y1
  return [eval "$c coords $item $vertices"]
}



proc setBox {} {
  global X1 X2 Y1 Y2
  global canv   
  $canv delete corner1 
  $canv delete corner2 
  $canv delete corner3 
  $canv delete corner4 
  $canv create line $X1 $Y1 $X1 $Y1 -tags corner1 
  $canv create line $X2 $Y1 $X2 $Y1 -tags corner2 
  $canv create line $X2 $Y2 $X2 $Y2 -tags corner3 
  $canv create line $X1 $Y2 $X1 $Y2 -tags corner4 
}



#



proc pos_update {x y} {

  global nbid

  nbs put $nbid.xp $x
  nbs put $nbid.yp $y

}
#

proc put_pos {x y} {

  global nbid

  nbs put $nbid.xp $x
  nbs put $nbid.yp $y
  nbs put $nbid.flag 1


}



#
#
proc patch_paste {w } {

  global nbid
  global canv
  global Top Ok

  set Top $w

#  create display window
  toplevel $w -class Dialog
  wm title $w "Patch"
  wm iconname $w Patch



  $canv configure -cursor crosshair

  bind $canv <ButtonPress-1> {
                          raise $Top .top
                          put_pos %x %y
                         }
#  start patching application
  ImgExecNoWait ipatch "mode=paste "


  pack [frame $w.exit] -side bottom -padx 1m -pady 1m

  button $w.exit.ok -text Ok -command {
                            nbs put $nbid.flag 999
                            set Ok 1
                           }


  button $w.exit.cancel -text Cancel -command {
                            nbs put $nbid.flag -1
                            set Ok 999
                           }

  pack $w.exit.ok $w.exit.cancel -side left


  set oldfocus [focus]

  tkwait variable Ok
  ImgExecWaitNoMsg istats " "
  bind $canv <ButtonPress-1> {}
  $canv configure -cursor arrow
  destroy $w
  focus $oldfocus
}




#
#
#
#
#
#
proc Format_Text {w } {

  global Text_font Text_size Text_colour Text_bold Text_just
  global Text_format_done

  global TmpW
  set TmpW $w

  toplevel $w -class Dialog
  wm title $w "Format Text"
  wm iconname $w Format

  pack [frame $w.bot] -side bottom -fill both
  pack [frame $w.just] -side bottom -fill both
  pack [frame $w.col] -side left -fill y
  pack [frame $w.size] -side bottom -fill both
  pack [frame $w.bold] -side bottom -fill both
  pack [frame $w.font] -side left

  label $w.font.lbl -text Font
  radiobutton $w.font.f1 -text Normal -variable Text_font -value 1
  radiobutton $w.font.f2 -text Roman -variable Text_font -value 2
  radiobutton $w.font.f3 -text Italic -variable Text_font -value 3
  radiobutton $w.font.f4 -text Script -variable Text_font -value 4

  pack $w.font.lbl $w.font.f1 $w.font.f2 $w.font.f3 $w.font.f4 -side top \
              -anchor nw -padx 2m

  label $w.col.lbl -text Colour
  radiobutton $w.col.c0 -text "Black (bg)" -variable Text_colour -value 0
  radiobutton $w.col.c1 -text "White (fg)" -variable Text_colour -value 1
  radiobutton $w.col.c2 -text "Red" -variable Text_colour -value 2
  radiobutton $w.col.c3 -text "Green" -variable Text_colour -value 3
  radiobutton $w.col.c4 -text "Blue" -variable Text_colour -value 4
  radiobutton $w.col.c5 -text "Cyan" -variable Text_colour -value 5
  radiobutton $w.col.c6 -text "Magenta" -variable Text_colour -value 6
  radiobutton $w.col.c7 -text "Yellow" -variable Text_colour -value 7

  pack $w.col.lbl $w.col.c0 $w.col.c1 $w.col.c2 $w.col.c3 $w.col.c4 \
              $w.col.c5 $w.col.c6 $w.col.c7 -side top -anchor nw -padx 2m


  scale $w.size.ch -label "% Character Size" -showvalue yes \
       -orient horizontal -to 500 -from 1 -length 40m \
       -command "get_ch $w.size.ch Text_size"
  scale $w.bold.cb -label "Character Boldness" -showvalue yes \
       -orient horizontal -to 20 -from 1 -length 40m \
       -command "get_lw $w.bold.cb Text_bold"
  $w.size.ch set [expr $Text_size*100]
  $w.bold.cb set $Text_bold

  pack $w.size.ch 
  pack $w.bold.cb

  pack [label $w.just.lbl -text Alignment] [frame $w.just.choose] \
                                            -side top
  radiobutton $w.just.choose.left -text "Left" -variable Text_just \
                                                                  -value "L"
  radiobutton $w.just.choose.centre -text "Centre" -variable Text_just \
                                                                  -value "C"
  radiobutton $w.just.choose.right -text "Right" -variable Text_just \
                                                                  -value "R"
  pack $w.just.choose.left $w.just.choose.centre $w.just.choose.right \
                                           -side left -fill both

  button $w.bot.ok -text Close -command {
                                         set Text_format_done 1
                                         destroy $TmpW
                                        }

  button $w.bot.reset -text Reset -command {
                                            set Text_font 1
                                            set Text_size 1.0
                                            set Text_bold 1
                                            set Text_colour 1
                                            set Text_just "L"
                                           }

  pack $w.bot.ok $w.bot.reset \
                -side left -expand 1 -padx 3m -padx 4m

  Centre_Window $w

}
#
#
#
#
#
#
#
#
#
proc Gui2World {xc yc x y} {
  upvar $x xx
  upvar $y yy

  global Height Width
  global Xmin Xmax Ymin Ymax
  global Wxmin Wxmax Wymin Wymax

  set xp $xc
  set yp [expr $Height-$yc]
  set xprange [expr $Wxmax-$Wxmin]
  set yprange [expr $Wymax-$Wymin]
  set xrange [expr $Xmax-$Xmin]
  set yrange [expr $Ymax-$Ymin]

  set xx [expr $Xmin+($xp-$Wxmin)*$xrange/$xprange]
  set yy [expr $Ymin+($yp-$Wymin)*$yrange/$yprange]
}
#
#
#
#



