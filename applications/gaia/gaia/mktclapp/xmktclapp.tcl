# A Notebook widget for Tcl/Tk
# $Revision$
#
# Copyright (C) 1996,1997,1998 D. Richard Hipp
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.
#
# Author contact information:
#   drh@acm.org
#   http://www.hwaci.com/drh/

#
# Create a new notebook widget
#
proc Notebook:create {w args} {
  global Notebook
  set Notebook($w,width) 400
  set Notebook($w,height) 300
  set Notebook($w,pages) {}
  set Notebook($w,top) 0
  set Notebook($w,pad) 5
  set Notebook($w,fg,on) black
  set Notebook($w,fg,off) grey50
  canvas $w -bd 0 -highlightthickness 0 -takefocus 0
  set Notebook($w,bg) [$w cget -bg]
  bind $w <1> "Notebook:click $w %x %y"
  bind $w <Configure> "Notebook:scheduleExpand $w"
  eval Notebook:config $w $args
}

#
# Change configuration options for the notebook widget
#
proc Notebook:config {w args} {
  global Notebook
  foreach {tag value} $args {
    switch -- $tag {
      -width {
        set Notebook($w,width) $value
      }
      -height {
        set Notebook($w,height) $value
      }
      -pages {
        set Notebook($w,pages) $value
      }
      -pad {
        set Notebook($w,pad) $value
      }
      -bg {
        set Notebook($w,bg) $value
      }
      -fg {
        set Notebook($w,fg,on) $value
      }
      -disabledforeground {
        set Notebook($w,fg,off) $value
      }
    }
  }

  #
  # After getting new configuration values, reconstruct the widget
  #
  $w delete all
  set Notebook($w,x1) $Notebook($w,pad)
  set Notebook($w,x2) [expr $Notebook($w,x1)+2]
  set Notebook($w,x3) [expr $Notebook($w,x2)+$Notebook($w,width)]
  set Notebook($w,x4) [expr $Notebook($w,x3)+2]
  set Notebook($w,y1) [expr $Notebook($w,pad)+2]
  set Notebook($w,y2) [expr $Notebook($w,y1)+2]
  set Notebook($w,y5) [expr $Notebook($w,y1)+30]
  set Notebook($w,y6) [expr $Notebook($w,y5)+2]
  set Notebook($w,y3) [expr $Notebook($w,y6)+$Notebook($w,height)]
  set Notebook($w,y4) [expr $Notebook($w,y3)+2]
  set x $Notebook($w,x1)
  set cnt 0
  set y7 [expr $Notebook($w,y1)+10]
  foreach p $Notebook($w,pages) {
    set Notebook($w,p$cnt,x5) $x
    set id [$w create text 0 0 -text $p -anchor nw -tags "p$cnt t$cnt"]
    set bbox [$w bbox $id]
    set width [lindex $bbox 2]
    $w move $id [expr $x+10] $y7
    $w create line \
       $x $Notebook($w,y5)\
       $x $Notebook($w,y2) \
       [expr $x+2] $Notebook($w,y1) \
       [expr $x+$width+16] $Notebook($w,y1) \
       -width 2 -fill white -tags p$cnt
    $w create line \
       [expr $x+$width+16] $Notebook($w,y1) \
       [expr $x+$width+18] $Notebook($w,y2) \
       [expr $x+$width+18] $Notebook($w,y5) \
       -width 2 -fill black -tags p$cnt
    set x [expr $x+$width+20]
    set Notebook($w,p$cnt,x6) [expr $x-2]
    if {![winfo exists $w.f$cnt]} {
      frame $w.f$cnt -bd 0
    }
    $w.f$cnt config -bg $Notebook($w,bg)
    place $w.f$cnt -x $Notebook($w,x2) -y $Notebook($w,y6) \
      -width $Notebook($w,width) -height $Notebook($w,height)
    incr cnt
  }
  $w create line \
     $Notebook($w,x1) [expr $Notebook($w,y5)-2] \
     $Notebook($w,x1) $Notebook($w,y3) \
     -width 2 -fill white
  $w create line \
     $Notebook($w,x1) $Notebook($w,y3) \
     $Notebook($w,x2) $Notebook($w,y4) \
     $Notebook($w,x3) $Notebook($w,y4) \
     $Notebook($w,x4) $Notebook($w,y3) \
     $Notebook($w,x4) $Notebook($w,y6) \
     $Notebook($w,x3) $Notebook($w,y5) \
     -width 2 -fill black
  $w config -width [expr $Notebook($w,x4)+$Notebook($w,pad)] \
            -height [expr $Notebook($w,y4)+$Notebook($w,pad)] \
            -bg $Notebook($w,bg)
  set top $Notebook($w,top)
  set Notebook($w,top) -1
  Notebook:raise.page $w $top
}

#
# This routine is called whenever the mouse-button is pressed over
# the notebook.  It determines if any page should be raised and raises
# that page.
#
proc Notebook:click {w x y} {
  global Notebook
  if {$y<$Notebook($w,y1) || $y>$Notebook($w,y6)} return
  set N [llength $Notebook($w,pages)]
  for {set i 0} {$i<$N} {incr i} {
    if {$x>=$Notebook($w,p$i,x5) && $x<=$Notebook($w,p$i,x6)} {
      Notebook:raise.page $w $i
      break
    }
  }
}

#
# For internal use only.  This procedure raised the n-th page of
# the notebook
#
proc Notebook:raise.page {w n} {
  global Notebook
  if {$n<0 || $n>=[llength $Notebook($w,pages)]} return
  set top $Notebook($w,top)
  if {$top>=0 && $top<[llength $Notebook($w,pages)]} {
    $w move p$top 0 2
  }
  $w move p$n 0 -2
  $w delete topline
  if {$n>0} {
    $w create line \
       $Notebook($w,x1) $Notebook($w,y6) \
       $Notebook($w,x2) $Notebook($w,y5) \
       $Notebook($w,p$n,x5) $Notebook($w,y5) \
       $Notebook($w,p$n,x5) [expr $Notebook($w,y5)-2] \
       -width 2 -fill white -tags topline
  }
  $w create line \
    $Notebook($w,p$n,x6) [expr $Notebook($w,y5)-2] \
    $Notebook($w,p$n,x6) $Notebook($w,y5) \
    -width 2 -fill white -tags topline
  $w create line \
    $Notebook($w,p$n,x6) $Notebook($w,y5) \
    $Notebook($w,x3) $Notebook($w,y5) \
    -width 2 -fill white -tags topline
  set Notebook($w,top) $n
  raise $w.f$n
}

#
# Change the page-specific configuration options for the notebook
#
proc Notebook:pageconfig {w name args} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i<0} return
  foreach {tag value} $args {
    switch -- $tag {
      -state {
        if {"$value"=="disabled"} {
          $w itemconfig t$i -fg $Notebook($w,fg,off)
        } else {
          $w itemconfig t$i -fg $Notebook($w,fg,on)
        }
      }
      -onexit {
        set Notebook($w,p$i,onexit) $value
      }
    }
  }
}

#
# This procedure raises a notebook page given its name.  But first
# we check the "onexit" procedure for the current page (if any) and
# if it returns false, we don't allow the raise to proceed.
#
proc Notebook:raise {w name} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i<0} return
  if {[info exists Notebook($w,p$i,onexit)]} {
    set onexit $Notebook($w,p$i,onexit)
    if {"$onexit"!="" && [eval uplevel #0 $onexit]!=0} {
      Notebook:raise.page $w $i
    }
  } else {
    Notebook:raise.page $w $i
  }
}

#
# Return the frame associated with a given page of the notebook.
#
proc Notebook:frame {w name} {
  global Notebook
  set i [lsearch $Notebook($w,pages) $name]
  if {$i>=0} {
    return $w.f$i
  } else {
    return {}
  }
}

#
# Try to resize the notebook to the next time we become idle.
#
proc Notebook:scheduleExpand w {
  global Notebook
  if {[info exists Notebook($w,expand)]} return
  set Notebook($w,expand) 1
  after idle "Notebook:expand $w"
}

#
# Resize the notebook to fit inside its containing widget.
#
proc Notebook:expand w {
  global Notebook
  set wi [expr [winfo width $w]-($Notebook($w,pad)*2+4)]
  set hi [expr [winfo height $w]-($Notebook($w,pad)*2+36)]
  Notebook:config $w -width $wi -height $hi
  catch {unset Notebook($w,expand)}
}

# End of the notebook widget.
#################################

################################ Label Frame #############################
#
#
proc LabelFrame:create {w args} {
  frame $w -bd 0
  label $w.l
  frame $w.f -bd 2 -relief groove
  frame $w.f.f
  pack $w.f.f
  set text {}
  set font {}
  set padx 3
  set pady 7
  set ipadx 2
  set ipady 9
  foreach {tag value} $args {
    switch -- $tag {
      -font   {set font $value}
      -text   {set text $value}
      -padx   {set padx $value}
      -pady   {set pady $value}
      -ipadx  {set ipadx $value}
      -ipady  {set ipady $value}
      -bd     {$w.f config -bd $value}
      -relief {$w.f config -relief $value}
    }
  }
  if {"$font"!=""} {
    $w.l config -font $font
  }
  $w.l config -text $text
  pack $w.f -padx $padx -pady $pady -fill both -expand 1
  place $w.l -x [expr $padx+10] -y $pady -anchor w
  pack $w.f.f -padx $ipadx -pady $ipady -fill both -expand 1
  raise $w.l
  return $w.f.f
}
# End of the labeled frame widget.
########################################################

#########################################################
# Directory Selector TCL version 1.1
#
# Originally written by:
# Daniel Roche, <dan@lectra.com>
#
# Modified for xmktclapp (and for version of Tk prior to 8.0) by:
# D. Richard Hipp, <drh@hwaci.com>

# tk_getDirectory [option value ...]
#
#  options are :
#   [-initialdir dir]     display in dir
#   [-title string]       make string title of dialog window
#   [-ok string]          make string the label of OK button
#   [-open string]        make string the label of OPEN button
#   [-cancel string]      make string the label of CANCEL button
#   [-msg1 string]        make string the label of the first directory message
#   [-msg2 string]        make string the label of the second directory message
#
proc tk_getDirectory {args} {
    global tcl_platform tk_getDirectory

    #
    # arguments
    #
    set _titre "Directory Selector"
    set _ldir Directory:
    set _ldnam "Directory Name:"
    set _open Ok
    set _expand Open
    set _cancel Cancel
    if {![info exists tk_getDirectory(curdir)]} {
      set tk_getDirectory(curdir) [pwd]
    }
    
    set ind 0
    set max [llength $args]
    while { $ind < $max } {
	switch -exact -- [lindex $args $ind] {
	    "-initialdir" {
		incr ind
		set tk_getDirectory(curdir) [lindex $args $ind]
		incr ind
	    }
	    "-title" {
		incr ind
		set _titre [lindex $args $ind]
		incr ind
	    }
	    "-ok" {
		incr ind
		set _open [lindex $args $ind]
		incr ind
	    }
	    "-open" {
		incr ind
		set _expand [lindex $args $ind]
		incr ind
	    }
	    "-cancel" {
		incr ind
		set _cancel [lindex $args $ind]
		incr ind
	    }
	    "-msg1" {
		incr ind
		set _ldir [lindex $args $ind]
		incr ind
	    }
	    "-msg2" {
		incr ind
		set _ldnam [lindex $args $ind]
		incr ind
	    }
	    default {
		puts "unknown option [lindex $args $ind]"
		return ""
	    }
	}
    }
    
    #
    # variables et data
    #
    set tk_getDirectory(fini) 0
    
    image create bitmap tk_getDirectory:b_up -data "
    #define up_width 31
    #define up_height 23
    static unsigned char up_bits[] = {
	0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x80,
	0x00, 0x00, 0x00, 0x80, 0x00, 0x3f, 0x00, 0x80, 0x80, 0x40, 0x00, 0x80,
	0x40, 0x80, 0x00, 0x80, 0xe0, 0xff, 0xff, 0x83, 0x20, 0x00, 0x00, 0x82,
	0x20, 0x04, 0x00, 0x82, 0x20, 0x0e, 0x00, 0x82, 0x20, 0x1f, 0x00, 0x82,
	0x20, 0x04, 0x00, 0x82, 0x20, 0x04, 0x00, 0x82, 0x20, 0x04, 0x00, 0x82,
	0x20, 0xfc, 0x0f, 0x82, 0x20, 0x00, 0x00, 0x82, 0x20, 0x00, 0x00, 0x82,
	0xe0, 0xff, 0xff, 0x83, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x80,
	0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x80};"

    image create bitmap tk_getDirectory:b_dir -background #ffff80 -data "
    #define dir_width 17
    #define dir_height 16
    static unsigned char dir_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00, 0x10, 0x02, 0x00,
	0x08, 0x04, 0x00, 0xfc, 0x7f, 0x00, 0x04, 0x40, 0x00, 0x04, 0x40, 0x00,
	0x04, 0x40, 0x00, 0x04, 0x40, 0x00, 0x04, 0x40, 0x00, 0x04, 0x40, 0x00,
	0x04, 0x40, 0x00, 0xfc, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};" \
		-maskdata "
    #define dirm_width 17
    #define dirm_height 16
    static unsigned char dirm_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00, 0xf0, 0x03, 0x00,
	0xf8, 0x07, 0x00, 0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00,
	0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00,
	0xfc, 0x7f, 0x00, 0xfc, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
		
    switch -exact $tcl_platform(platform) {
	unix {
            set tk_getDirectory(myfont) \
               -adobe-helvetica-bold-r-normal-*-12-120-75-75-p-70-iso8859-1 
	}
	windows {
	    set tk_getDirectory(myfont) {Courier 12}
	}
    }

    #
    # widgets
    #
    if {[winfo exists .dirsel]} {destroy .dirsel}
    toplevel .dirsel
    grab set .dirsel
    wm geometry .dirsel 500x250
    wm title .dirsel $_titre
    
    frame .dirsel.f1 -relief flat -borderwidth 0
    frame .dirsel.f2 -relief sunken -borderwidth 2 
    frame .dirsel.f3 -relief flat -borderwidth 0
    frame .dirsel.f4 -relief flat -borderwidth 0
    
    pack .dirsel.f1 -fill x
    pack .dirsel.f2 -fill both -expand 1 -padx 6 -pady 6
    pack .dirsel.f3 -fill x
    pack .dirsel.f4 -fill x
    
    label .dirsel.f1.lab -text $_ldir
    menubutton .dirsel.f1.dir -relief raised -indicatoron 1 -anchor w \
	    -menu .dirsel.f1.dir.m
    menu .dirsel.f1.dir.m -tearoff 0
    button .dirsel.f1.up -image tk_getDirectory:b_up \
        -command tk_getDirectory:UpDir
    
    pack .dirsel.f1.up -side right -padx 4 -pady 4
    pack .dirsel.f1.lab -side left -padx 4 -pady 4
    pack .dirsel.f1.dir -side right -padx 4 -pady 4 -fill x -expand 1
    
    canvas .dirsel.f2.cv -borderwidth 0 -xscrollcommand ".dirsel.f2.sb set" \
       -height 10 -bg white
    scrollbar .dirsel.f2.sb -command ".dirsel.f2.cv xview" -orient horizontal
    pack .dirsel.f2.cv -side top -fill both -expand 1
    pack .dirsel.f2.sb -side top -fill x
    
    .dirsel.f2.cv bind TXT <Any-Button> tk_getDirectory:ClickItem
    .dirsel.f2.cv bind IMG <Any-Button> tk_getDirectory:ClickItem
    
    button .dirsel.f4.open -text $_open \
       -command {set tk_getDirectory(fini) 1}
    button .dirsel.f4.cancel -text $_cancel \
       -command {set tk_getDirectory(fini) -1}
    pack .dirsel.f4.open -side left -padx 25 -pady 4
    pack .dirsel.f4.cancel -side right -padx 25 -pady 4
    
    # Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw .dirsel
    update
    set p [winfo parent .dirsel]
    regsub -all {\+\-} [wm geometry $p] {-} geom
    scan $geom %dx%d%d%d pw ph px py
    set x [expr {$px + ($pw - 500)/2}]
    set y [expr {$py + ($ph - 250)/2}]
    if {$x<0} {set x 0}
    if {$y<0} {set y 0}
    wm geom .dirsel +$x+$y
    wm deiconify .dirsel

    #
    # realwork
    #
    tk_getDirectory:ShowDir $tk_getDirectory(curdir)
    
    #
    # wait user
    #
    tkwait variable tk_getDirectory(fini)

    if { $tk_getDirectory(fini) == 1 } {
	set retval [.dirsel.f1.dir cget -text]
    } else {
	set retval ""
    }
    
    destroy .dirsel
    unset tk_getDirectory
    return $retval
}

proc tk_getDirectory:ShowDir {curdir} {
    global tcl_platform tk_getDirectory

    set tk_getDirectory(curdir) $curdir    
    .dirsel.f1.dir configure -text $curdir
    
    set hi [image height tk_getDirectory:b_dir]
    set wi [image width tk_getDirectory:b_dir]
    incr wi 4
    set maxy [expr [winfo height .dirsel.f2.cv]-$hi]
    
    set lidir [list]
    foreach file [glob -nocomplain $curdir/*] {
	if [ file isdirectory $file ] { 
	    lappend lidir [file tail $file]
	}
    }
    set sldir [lsort $lidir]
    
    .dirsel.f2.cv delete all
    set ind 0
    set x 2
    set y 2
    foreach file $sldir {
      .dirsel.f2.cv create image $x $y \
         -anchor nw -image tk_getDirectory:b_dir -tags IMG
      .dirsel.f2.cv create text [expr $x+$wi] $y \
         -anchor nw -text $file -font $tk_getDirectory(myfont) -tags TXT
      incr y $hi
      if {$y>=$maxy} {
        set bbox [.dirsel.f2.cv bbox all]
        set x [expr [lindex $bbox 2]+10]
        set y 2
      }
    }
    .dirsel.f2.cv configure -scrollregion [.dirsel.f2.cv bbox all]
    
    set curlst [file split $curdir]
    set nbr [llength $curlst]
    
    .dirsel.f1.dir.m delete 0 last
    incr nbr -2
    for {set ind $nbr} {$ind >= 0} {incr ind -1} {
	set tmplst [ lrange $curlst 0 $ind] 
	set tmpdir [ eval file join $tmplst] 
	.dirsel.f1.dir.m add command -label $tmpdir \
            -command "tk_getDirectory:ShowDir [list $tmpdir]"
    }
    if {[info exist tk_getDirectory(drives)] == 0} {
       if {[catch {file volume} tk_getDirectory(drives)]} {
          set tk_getDirectory(drives) {}
       }
    }
    if ![string compare $tcl_platform(platform) windows] {
	foreach drive $tk_getDirectory(drives) {
	    .dirsel.f1.dir.m add command -label $drive \
                -command "tk_getDirectory:ShowDir [list $drive]"
	}
    }
    
}

proc tk_getDirectory:UpDir {} {
  set curdir [.dirsel.f1.dir cget -text]
  set curlst [file split $curdir]
  set nbr [llength $curlst]
  if { $nbr < 2 } {
    return
  }
  set tmp [expr $nbr - 2]
  set newlst [ lrange $curlst 0 $tmp ]
  set newdir [ eval file join $newlst ]
  tk_getDirectory:ShowDir $newdir
}

proc tk_getDirectory:ClickItem {} {
  set id [.dirsel.f2.cv find withtag current]
  set dir [.dirsel.f2.cv itemcget $id -text]
  if {[string length $dir]==0} return
  global tk_getDirectory
  tk_getDirectory:ShowDir [file join $tk_getDirectory(curdir) $dir]
}

#
# End tk_getDirectory widget
########################################################################

########################################################################
#
# This version of msgbox.tcl has been modified in two ways:
#
#     1.  Color icons are used on Unix displays that have a color
#         depth of 4 or more.  Most users like the color icons better.
#
#     2.  The button on error dialog boxes says "Bummer" instead of
#         "OK", because errors are not ok.
#
# Other than that, the code is identical and should be fully
# backwards compatible.
#

image create bitmap tkPriv:b1 -foreground black \
-data "#define b1_width 32\n#define b1_height 32
static unsigned char q1_bits[] = {
   0x00, 0xf8, 0x1f, 0x00, 0x00, 0x07, 0xe0, 0x00, 0xc0, 0x00, 0x00, 0x03,
   0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x10,
   0x04, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
   0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
   0x01, 0x00, 0x00, 0x80, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x04, 0x00, 0x00, 0x20, 0x08, 0x00, 0x00, 0x10, 0x10, 0x00, 0x00, 0x08,
   0x60, 0x00, 0x00, 0x04, 0x80, 0x03, 0x80, 0x03, 0x00, 0x0c, 0x78, 0x00,
   0x00, 0x30, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00,
   0x00, 0x80, 0x04, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x06, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:b2 -foreground white \
-data "#define b2_width 32\n#define b2_height 32
static unsigned char b2_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1f, 0x00, 0x00, 0xff, 0xff, 0x00,
   0xc0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x07, 0xf0, 0xff, 0xff, 0x0f,
   0xf8, 0xff, 0xff, 0x1f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
   0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
   0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
   0xfe, 0xff, 0xff, 0x7f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
   0xf8, 0xff, 0xff, 0x1f, 0xf0, 0xff, 0xff, 0x0f, 0xe0, 0xff, 0xff, 0x07,
   0x80, 0xff, 0xff, 0x03, 0x00, 0xfc, 0x7f, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00,
   0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:q -foreground blue \
-data "#define q_width 32\n#define q_height 32
static unsigned char q_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0x10, 0x0f, 0x00, 0x00, 0x18, 0x1e, 0x00, 0x00, 0x38, 0x1e, 0x00,
   0x00, 0x38, 0x1e, 0x00, 0x00, 0x10, 0x0f, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0xc0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:i -foreground blue \
-data "#define i_width 32\n#define i_height 32
static unsigned char i_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00, 0xf0, 0x03, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xf8, 0x03, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf8, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:w1 -foreground black \
-data "#define w1_width 32\n#define w1_height 32
static unsigned char w1_bits[] = {
   0x00, 0x80, 0x01, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x20, 0x04, 0x00,
   0x00, 0x10, 0x04, 0x00, 0x00, 0x10, 0x08, 0x00, 0x00, 0x08, 0x08, 0x00,
   0x00, 0x08, 0x10, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x04, 0x20, 0x00,
   0x00, 0x02, 0x20, 0x00, 0x00, 0x02, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00,
   0x00, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x01,
   0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00, 0x02,
   0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x08,
   0x08, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x10, 0x04, 0x00, 0x00, 0x10,
   0x04, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x40,
   0x01, 0x00, 0x00, 0x40, 0x01, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x20,
   0xfc, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:w2 -foreground yellow \
-data "#define w2_width 32\n#define w2_height 32
static unsigned char w2_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf0, 0x0f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xf8, 0x1f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfc, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xfe, 0x7f, 0x00, 0x00, 0xff, 0x7f, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x80, 0xff, 0xff, 0x00, 0x80, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x07,
   0xf0, 0xff, 0xff, 0x07, 0xf0, 0xff, 0xff, 0x0f, 0xf8, 0xff, 0xff, 0x0f,
   0xf8, 0xff, 0xff, 0x1f, 0xfc, 0xff, 0xff, 0x1f, 0xfe, 0xff, 0xff, 0x3f,
   0xfe, 0xff, 0xff, 0x3f, 0xfe, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x1f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
image create bitmap tkPriv:w3 -foreground black \
-data "#define w3_width 32\n#define w3_height 32
static unsigned char w3_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0xc0, 0x03, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"

# tkMessageBox --
#
#	Pops up a messagebox with an application-supplied message with
#	an icon and a list of buttons. This procedure will be called
#	by tk_messageBox if the platform does not have native
#	messagebox support, or if the particular type of messagebox is
#	not supported natively.
#
#	This procedure is a private procedure shouldn't be called
#	directly. Call tk_messageBox instead.
#
#	See the user documentation for details on what tk_messageBox does.
#
proc tkMessageBox {args} {
    global tkPriv tcl_platform

    set w tkPrivMsgBox
    upvar #0 $w data

    #
    # The default value of the title is space (" ") not the empty string
    # because for some window managers, a 
    #		wm title .foo ""
    # causes the window title to be "foo" instead of the empty string.
    #
    set specs {
	{-default "" "" ""}
        {-icon "" "" "info"}
        {-message "" "" ""}
        {-parent "" "" .}
        {-title "" "" " "}
        {-type "" "" "ok"}
    }

    tclParseConfigSpec $w $specs "" $args

    if {[lsearch {info warning error question} $data(-icon)] == -1} {
	error "invalid icon \"$data(-icon)\", must be error, info, question or warning"
    }
    if {$tcl_platform(platform) == "macintosh"} {
	if {$data(-icon) == "error"} {
	    set data(-icon) "stop"
	} elseif {$data(-icon) == "warning"} {
	    set data(-icon) "caution"
	} elseif {$data(-icon) == "info"} {
	    set data(-icon) "note"
	}
    }

    if {![winfo exists $data(-parent)]} {
	error "bad window path name \"$data(-parent)\""
    }

    case $data(-type) {
	abortretryignore {
	    set buttons {
		{abort  -width 6 -text Abort -under 0}
		{retry  -width 6 -text Retry -under 0}
		{ignore -width 6 -text Ignore -under 0}
	    }
	}
	ok {
	    if {$data(-icon) == "error"} {
		set buttons {
		    {ok -width 6 -text Bummer -under 0}
		}
	    } else {
		set buttons {
		    {ok -width 6 -text OK -under 0}
		}
	    }
	    if {$data(-default) == ""} {
		set data(-default) "ok"
	    }
	}
	okcancel {
	    set buttons {
		{ok     -width 6 -text OK     -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	retrycancel {
	    set buttons {
		{retry  -width 6 -text Retry  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	yesno {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
	    }
	}
	yesnocancel {
	    set buttons {
		{yes    -width 6 -text Yes -under 0}
		{no     -width 6 -text No  -under 0}
		{cancel -width 6 -text Cancel -under 0}
	    }
	}
	default {
	    error "invalid message box type \"$data(-type)\", must be abortretryignore, ok, okcancel, retrycancel, yesno or yesnocancel"
	}
    }

    if {[string compare $data(-default) ""]} {
	set valid 0
	foreach btn $buttons {
	    if {![string compare [lindex $btn 0] $data(-default)]} {
		set valid 1
		break
	    }
	}
	if {!$valid} {
	    error "invalid default button \"$data(-default)\""
	}
    }

    # 2. Set the dialog to be a child window of $parent
    #
    #
    if {[string compare $data(-parent) .]} {
	set w $data(-parent).__tk__messagebox
    } else {
	set w .__tk__messagebox
    }

    # 3. Create the top-level window and divide it into top
    # and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w $data(-title)
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w $data(-parent)
    if {$tcl_platform(platform) == "macintosh"} {
	unsupported1 style $w dBoxProc
    }

    frame $w.bot
    pack $w.bot -side bottom -fill both
    frame $w.top
    pack $w.top -side top -fill both -expand 1
    if {$tcl_platform(platform) != "macintosh"} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }

    # 4. Fill the top part with bitmap and message (use the option
    # database for -wraplength so that it can be overridden by
    # the caller).

    option add *Dialog.msg.wrapLength 3i widgetDefault
    label $w.msg -justify left -text $data(-message)
    catch {$w.msg configure -font \
		-Adobe-Times-Medium-R-Normal--*-180-*-*-*-*-*-*
    }
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$data(-icon) != ""} {
	if {$tcl_platform(platform)=="macintosh" || [winfo depth $w]<4} {
	    label $w.bitmap -bitmap $data(-icon)
	} else {
	    canvas $w.bitmap -width 32 -height 32 -highlightthickness 0
	    switch $data(-icon) {
		error {
		    $w.bitmap create oval 0 0 31 31 -fill red -outline black
		    $w.bitmap create line 9 9 23 23 -fill white -width 4
		    $w.bitmap create line 9 23 23 9 -fill white -width 4
		}
		info {
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:b1
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:b2
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:i
		}
		question {
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:b1
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:b2
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:q
		}
		default {
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:w1
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:w2
		    $w.bitmap create image 0 0 -anchor nw -image tkPriv:w3
		}
	    }
	}
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 5. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $buttons {
	set name [lindex $but 0]
	set opts [lrange $but 1 end]
	if {![string compare $opts {}]} {
	    # Capitalize the first letter of $name
	    set capName \
		[string toupper \
		    [string index $name 0]][string range $name 1 end]
	    set opts [list -text $capName]
	}

	eval button $w.$name $opts -command [list "set tkPriv(button) $name"]

	if {![string compare $name $data(-default)]} {
	    catch {$w.$name configure -default active}
	}
	pack $w.$name -in $w.bot -side left -expand 1 \
	    -padx 3m -pady 2m

	# create the binding for the key accelerator, based on the underline
	#
	set underIdx [$w.$name cget -under]
	if {$underIdx >= 0} {
	    set key [string index [$w.$name cget -text] $underIdx]
	    bind $w <Alt-[string tolower $key]>  "$w.$name invoke"
	    bind $w <Alt-[string toupper $key]>  "$w.$name invoke"
	}
	incr i
    }

    # 6. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {[string compare $data(-default) ""]} {
	bind $w <Return> "tkButtonInvoke $w.$data(-default)"
    }

    # 7. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set p [winfo parent $w]
    regsub -all {\+\-} [wm geometry $p] {-} geom
    scan $geom %dx%d%d%d pw ph px py
    set x [expr {$px + ($pw - [winfo reqwidth $w])/2}]
    set y [expr {$py + ($ph - [winfo reqheight $w])/2}]
    if {$x<0} {set x 0}
    if {$y<0} {set y 0}
    wm geom $w +$x+$y
    wm deiconify $w

    # 8. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    if {[string compare $data(-default) ""]} {
	focus $w.$data(-default)
    } else {
	focus $w
    }

    # 9. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    destroy $w
    if {$oldGrab != ""} {
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
    return $tkPriv(button)
}
#
##################### End Tk_MessageBox ############################

#################### Begin Console Wdiget ##########################
# A console widget for Tcl/Tk.  Invoke OpenConsole with a window name
# and prompt string to get a new top-level window that allows the
# user to enter tcl commands.  This is mainly useful for testing and
# debugging.
#
# Copyright (C) 1998 D. Richard Hipp
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.
#
# Author contact information:
#   drh@acm.org
#   http://www.hwaci.com/drh/


proc OpenConsole {w prompt} {
  upvar #0 $w.t v
  if {[winfo exists $w]} {destroy $w}
  catch {unset v}
  toplevel $w
  wm title $w {Test And Debug Console}
  wm iconname $w {Console}
  button $w.quit -text Dismiss -command "destroy $w"
  pack $w.quit -side bottom
  scrollbar $w.sb -orient vertical -command "$w.t yview"
  pack $w.sb -side right -fill y
  text $w.t -font fixed -yscrollcommand "$w.sb set"
  pack $w.t -side right -fill both -expand 1
  bindtags $w.t Console
  set v(text) $w.t
  set v(history) 0
  set v(historycnt) 0
  set v(current) -1
  set v(prompt) $prompt
  set v(plength) [string length $v(prompt)]
  $w.t insert end $v(prompt)
  $w.t mark set insert end
  $w.t tag config ok -foreground blue
  $w.t tag config err -foreground red
  after idle "focus $w.t"
}

bind Console <1> {focus %W}
bind Console <KeyPress> {conInsert %W %A}
bind Console <Left> {conLeft %W}
bind Console <Control-b> {conLeft %W}
bind Console <Right> {conRight %W}
bind Console <Control-f> {conRight %W}
bind Console <BackSpace> {conBackspace %W}
bind Console <Control-h> {conBackspace %W}
bind Console <Delete> {conDelete %W}
bind Console <Control-d> {conDelete %W}
bind Console <Home> {conHome %W}
bind Console <Control-a> {conHome %W}
bind Console <End> {conEnd %W}
bind Console <Control-e> {conEnd %W}
bind Console <Return> {conEnter %W}
bind Console <KP_Enter> {conEnter %W}
bind Console <Up> {conPrior %W}
bind Console <Control-p> {conPrior %W}
bind Console <Down> {conNext %W}
bind Console <Control-n> {conNext %W}

# Insert a single character at the insertion cursor
#
proc conInsert {w a} {
  $w insert insert $a
}

# Move the cursor one character to the left
#
proc conLeft {w} {
  upvar #0 $w v
  scan [$w index insert] %d.%d row col
  if {$col>$v(plength)} {
    $w mark set insert "insert -1c"
  }
}

# Erase the character to the left of the cursor
#
proc conBackspace {w} {
  upvar #0 $w v
  scan [$w index insert] %d.%d row col
  if {$col>$v(plength)} {
    $w delete {insert -1c}
  }
}

# Move the cursor one character to the right
#
proc conRight {w} {
  $w mark set insert "insert +1c"
}

# Erase the character to the right of the cursor
#
proc conDelete w {
  $w delete insert
}

# Move the cursor to the beginning of the current line
#
proc conHome w {
  upvar #0 $w v
  scan [$w index insert] %d.%d row col
  $w mark set insert $row.$v(plength)
}

# Move the cursor to the end of the current line
#
proc conEnd w {
  $w mark set insert {insert lineend}
}

# Called when "Enter" is pressed.  Do something with the line
# of text that was entered.
#
proc conEnter w {
  upvar #0 $w v
  scan [$w index insert] %d.%d row col
  set start $row.$v(plength)
  set line [$w get $start "$start lineend"]
  if {$v(historycnt)>0} {
    set last [lindex $v(history) [expr $v(historycnt)-1]]
    if {[string compare $last $line]} {
      lappend v(history) $line
      incr v(historycnt)
    }
  } else {
    set v(history) [list $line]
    set v(historycnt) 1
  }
  set v(current) $v(historycnt)
  $w insert end \n
  if {[catch {uplevel #0 $line} res]} {
    $w insert end $res\n err
  } elseif {[string length $res]>0} {
    $w insert end $res\n ok
  }
  $w insert end $v(prompt)
  $w mark set insert end
  $w yview insert
}

# Change the line to the previous line
#
proc conPrior w {
  upvar #0 $w v
  if {$v(current)<=0} return
  incr v(current) -1
  set line [lindex $v(history) $v(current)]
  conSetLine $w $line
}

# Change the line to the next line
#
proc conNext w {
  upvar #0 $w v
  if {$v(current)>=$v(historycnt)} return
  incr v(current) 1
  set line [lindex $v(history) $v(current)]
  conSetLine $w $line
}

# Change the contents of the entry line
#
proc conSetLine {w line} {
  upvar #0 $w v
  scan [$w index insert] %d.%d row col
  set start $row.$v(plength)
  $w delete $start end
  $w insert end $line
  $w mark set insert end
  $w yview insert
}
########################### End Console Widget ###########################

# A configuration file can be read either by this program (the
# xmktclapp GUI) or by the command-line based mktclapp program.
# But each program reads different parts of the same file.
#
# Mktclapp treats each line that begins with '#' as a comment.
# Xmktclapp reads only those lines that begin with '##'.  Hence,
# each program reads a different part of the same file.

# Write the current configuration out to the file whose name is
# given as an argument.  The current configuration is stored in
# global variables.
#
# If an error occurs (such as we can't open the output file)
# generate an error.
#
proc WriteConfig {filename} {
  if {[catch {open $filename w} f]} {
    error "can't open $filename: $f"
  }
  puts $f "# Configuration file generated by xmktclapp"
  puts $f "# Hand editing is not recommended"
  puts $f "#"
  puts $f "# The \"xmktclapp\" program reads the lines that begin with \"##\"."
  puts $f "# The \"mktclapp\" program reads lines that don't begin with \"#\"."
  puts $f "# Lines beginning with a single \"#\" are comment."
  puts $f "#"

  # Write the part that xmktclapp uses
  #
  global conf
  foreach v [array names conf] {
    puts $f "## $v [list $conf($v)]"
  }

  # Write the part that mktclapp uses
  #
  if {$conf(Mode)!="Tcl/Tk"} {
    puts $f "-notk"
  }
  if {$conf(Autofork)=="Yes"} {
    puts $f "-autofork"
  }
  if {$conf(ReadStdin)=="Yes" && $conf(Autofork)=="No"} {
    puts $f "-read-stdin"
  }
  if {$conf(Shroud)=="Yes"} {
    puts $f "-shroud"
  }
  if {[string length $conf(MainScript)]>0} {
    puts $f "-main-script \"$conf(MainScript)\""
  }
  if {$conf(Standalone)=="Strict"} {
    puts $f "-standalone"
  }
  if {$conf(Standalone)!="No"} {
    set filelist [glob -nocomplain $conf(TclLib)/*.tcl]
    if {[file exists $conf(TclLib)/tclIndex]} {
      lappend filelist $conf(TclLib)/tclIndex
    }
    puts $f "-tcl-library \"$conf(TclLib)\""
    if {$conf(Mode)=="Tcl/Tk"} {
      set l2 [glob -nocomplain $conf(TkLib)/*.tcl]
      set filelist [concat $filelist $l2]
      if {[file exists $conf(TkLib)/tclIndex]} {
        lappend filelist $conf(TkLib)/tclIndex
      }
      puts $f "-tk-library \"$conf(TkLib)\""
    }
    foreach file $filelist {
      puts $f "-strip-tcl \"$file\""
    }
  }
  foreach file [lsort [array names conf CFile:*]] {
    set fn [string range $file 6 end]
    puts $f \"$fn\"
  }
  set tclFileList [lsort [array names conf TclFile:*]]
  foreach file $tclFileList {
    set fn [string range $file 8 end]
    if {$conf(TclFile:$fn)} {
      puts $f "-strip-tcl \"$fn\""
    } else {
      puts $f "-dont-strip-tcl \"$fn\""
    }
  }
  close $f
  Baseline
}

# This routine does the work of the "Save" action.
#
proc DoSave {} {
  global conf
  if {[string length $conf(ConfigFile)]==0} {
    return [DoSaveAs]
  }
  set ext [file extension $conf(ConfigFile)]
  if {[string length $ext]==0} {
    set conf(ConfigFile) $conf(ConfigFile).mta
  }
  return [catch {WriteConfig $conf(ConfigFile)}]
}

# This routine does the work of the "Save As..." action.
#
proc DoSaveAs {} {
  global conf
  set types {
    {{Mktclapp Config Files} {.mta}}
    {{All Files} *}
  }
  set f [tk_getSaveFile -filetypes $types]
  set f [RelativePath $f]
  if {$f!=""} {
    set conf(ConfigFile) $f
    return [DoSave]
  }
  return 0
}

# Read state information from a named file.  Return the number of
# errors encountered.  If the second parameter is not 0, then issue
# an error message for each error found.
#
proc ReadState {fn {quiet 0}} {
  if {[catch {open $fn r} f]} {
    if {!$quiet} {
      tk_messageBox -message "Can't open \"$fn\": $f" -type ok -icon error
    }
    return 1
  }
  set text [read $f]
  close $f
  SetDefaults
  global conf
  foreach line [split $text \n] {
    if {![regexp {^## } $line]} continue
    if {[lindex $line 0]!="##"} continue
    set var [lindex $line 1]
    set value [lindex $line 2]
    set conf($var) $value
  }
  set conf(ConfigFile) $fn
  if {![info exists conf(OutputFile)] || [string length $conf(OutputFile)]==0} {
    set conf(OutputFile) [file root $conf(ConfigFile)].c
  }
  InsertC
  InsertTcl
  return 0
}

# This routine does the work of the Open action.
#
proc DoOpen {} {
  global conf
  set types {
    {{Mktclapp Config Files} {.mta}}
    {{All Files} *}
  }
  set f [tk_getOpenFile -filetypes $types -defaultextension .mta]
  set f [RelativePath $f]
  if {$f==""} return
  set conf(ConfigFile) $f
  ReadState $conf(ConfigFile)
  Baseline
}

# Compare the current configuration with the configuration that
# we read from disk.  Return 1 if we need to save to disk.
#
proc IsDirty {} {
  global conf saved

  foreach v [array names conf] {
    if {![info exists saved($v)] || [string compare $conf($v) $saved($v)]} {
      return 1
    }
  }
  foreach v [array names saved] {
    if {![info exists conf($v)]} {
      return 1
    }
  }
  return 0
}

# We currently are not dirty.  Remember the current state of
# everything so we can compare it later to see if anything has
# changed.
#
proc Baseline {} {
  global conf saved
  catch {unset saved}
  foreach v [array names conf] {
    set saved($v) $conf($v)
  }
}

# Make the current configuration dirty by clearing all the
# "saved()" variables.
#
proc MakeDirty {} {
  catch {unset saved}
}

# Exit the GUI, after first saving the state
#
proc DoExit {} {
  if {[IsDirty]} {
    set r [tk_messageBox -message "Save changes before exiting?" \
              -type yesnocancel]
    if {$r=="cancel"} return
    if {$r=="yes"} {
      if {[DoSave]} return
    }
  }
  exit
}

# Initialize the application to its default state.
#
proc SetDefaults {} {
  global conf tcl_library tk_library
  foreach v [array names conf *File:*] {unset conf($v)}
  set conf(Mode) Tcl/Tk
  set conf(Autofork) No
  set conf(Standalone) No
  set conf(NoSource) No
  set conf(ConfigFile) appinit.mta
  set conf(Shroud) No
  set conf(MainScript) {}
  set conf(TclLib) $tcl_library
  set conf(TkLib) $tk_library
}

# Try to convert a full pathname into a relative pathname.
# But do the convertion only if no ".." are required up front.
#
proc RelativePath {full} {
  if {[file pathtype $full]=="absolute"} {
    set pwd [string trimright [pwd] /]
    set len [string length $pwd]
    set path [string range $full 0 $len]
    if {[string compare $path $pwd/]==0} {
      set full [string range $full [expr $len+1] end]
    }
  }
  return $full
}

# Force a filename to be relative to the current working directory.
# ".." are inserted if needed.
#
proc ForceRelative {name} {
  switch [file pathtype $name] {
    absolute {
      set pwd [file split [pwd]]
      set path [file split $name]
      global tcl_platform
      if {$tcl_platform(platform)=="windows"} {
        set pwd [string tolower $pwd]
        set path [string tolower $path]
      }
      set npwd [llength $pwd]
      set npath [llength $path]
      for {set i 0} {$i<$npwd && $i<$npath} {incr i} {
        if {[string compare [lindex $pwd $i] [lindex $path $i]]} break
      }
      set res {}
      for {set j 0} {$j<$npwd-$i} {incr j} {
        lappend res ..
      }
      set res [concat $res [lrange $path $i end]]
      if {[llength $res]==0} {
        return "."
      }
      return [eval file join $res]
    }
    relative -
    volumerelative {
      return $name
    }
  }
}

# Force a filename to be absolute.
#
proc ForceAbsolute {name} {
  switch [file pathtype $name] {
    relative {
      set path [file split [pwd]/$name]
      set len [llength $path]
      for {set i 1} {$i<$len} {incr i} {
        set dir [lindex $path $i]
        if {$dir=="."} {
          set path [lreplace $path $i $i]
          incr i -1
          incr len -1
          continue
        }
        if {$dir==".."} {
          if {$i==1} {
            set path [lreplace $path $i $i]
            incr i -1
            incr len -1
          } else {
            set path [lreplace $path [expr $i-1] $i]
            incr i -2
            incr len -2
          }
          continue
        }
      }
      return [eval file join $path]
    }
    absolute {
      return $name
    }
    volumerelative {
      return $name
    }
  }
}

# Change a relative path to absolute and an absolute path to relative.
#
proc TogglePath {path} {
  switch [file pathtype $path] {
    absolute {
      return [ForceRelative $path]
    }
    relative {
      return [ForceAbsolute $path]
    }
    volumerelative {
      return $path
    }
  }
}

# This routine is called to when various "Relative Path" buttons
# are pressed.  $w is the button widget.  $var is the name of the
# variable that contains the pathname that needs to be toggled
# between relative and absolute.
#
proc RelAbsPath {w var} {
  upvar #0 $var path
  set path [TogglePath $path]
  ConfigPathButton $w $path
}

# This routine works like RelAbsPath above, but for the special
# case of the Startup Script on the Tcl Scripts page.  In addition
# to toggling the path of the Startup Script (in conf(MainScript))
# we check to see if the script is in the list of Tcl scripts and
# toggle its name there too.
#
proc MainScriptChngPath {w} {
  global conf
  set old $conf(MainScript)
  RelAbsPath $w conf(MainScript)
  set new $conf(MainScript)
  if {[info exists conf(TclFile:$old)]} {
    set conf(TclFile:$new) $conf(TclFile:$old)
    unset conf(TclFile:$old)
    InsertTcl
  }
}

# Given a pathname and a button widget, set the button widget depending
# on the pathname.  As follows:
#
#   1. If the pathname is NULL, disable the button.
#
#   2. If the pathname is a relative path, make the button read
#      "Absolute Path".
#
#   3. If the pathname is absolute, make the button read "Relative Path".
#
proc ConfigPathButton {w path} {
  if {[string length $path]==0} {
    $w config -state disabled -text {Rel/Abs Path}
  } else {
    switch [file pathtype $path] {
      relative {
        $w config -state normal -text {Absolute Path}
      }
      absolute {
        $w config -state normal -text {Relative Path}
      }
      volumerelative {
        $w config -state disabled -text {}
      }
    }
  }
}

# This routine allows ConfigPathButton to be called from a
# variable trace.
#
proc TracePath {w var args} {
  global conf
  ConfigPathButton $w [set $var]
}

# Insert all the files named in the CFile array into the
# listbox on the C/C++ modules page
#
proc InsertC {{sel {}}} {
  global conf
  set w [Notebook:frame .n {C/C++ Modules}]
  $w.c.lb delete 0 end
  set idx 0
  foreach i [lsort [array names conf CFile:*]] {
    set fn [string range $i 6 end]
    $w.c.lb insert end $fn
    if {[string compare $sel $fn]==0} {
      $w.c.lb select clear 0 end
      $w.c.lb select set $idx
    }
    incr idx
  }
  SetCSelect
}

# Insert all the files named in the TclFile array into the
# listbox on the Tcl Scripts page.
#
proc InsertTcl {{sel {}}} {
  global conf
  set w [Notebook:frame .n {Tcl Scripts}]
  $w.c.lb delete 0 end
  set idx 0
  foreach i [lsort [array names conf TclFile:*]] {
    set fn [string range $i 8 end]
    if {$conf($i)} {
      set x "* $fn"
    } else {
      set x "  $fn"
    }
    $w.c.lb insert end $x
    if {[string compare $sel $fn]==0} {
      $w.c.lb select clear 0 end
      $w.c.lb select set $idx
    }
    incr idx
  }
  SetTclSelect
}

# This routine runs when the user presses the "Insert" button
# on the C/C++ Modules page
#
proc DoInsertC {} {
  set types {
    {{C/C++ Source} {.c .cpp .cc .C}}
  }
  set f [tk_getOpenFile -filetypes $types  \
          -title {Select C/C++ source}]
  set f [RelativePath $f]
  if {[string length $f]>0} {
    global conf
    set conf(CFile:$f) 1
    InsertC
  }
}

# This routine runs when the user presses the "Insert" button
# on the Tcl Scripts page
#
proc DoInsertTcl {} {
  set types {
    {{Tcl Scripts} {.tcl}}
    {{All Files} *}
  }
  set f [tk_getOpenFile -filetypes $types -title {Select Tcl Script}]
  set f [RelativePath $f]
  if {[string length $f]>0} {
    global conf
    set conf(TclFile:$f) 1
    InsertTcl
  }
}

# This routine runs when the user presses the "Browse" button on
# the Output C File entry box.
#
proc BrowseForOutputFile {} {
  set types {
    {{C/C++ Source Files} {.c .C}}
    {{All Files} *}
  }
  set f [tk_getSaveFile -filetypes $types -title {Select Output File}]
  set f [RelativePath $f]
  if {[string length $f]>0} {
    global conf
    set conf(OutputFile) $f
  }
}

# This routine runs when the user presses the "Browse" button on
# the Tcl Scripts page
#
proc BrowseForMainScript {} {
  set types {
    {{Tcl Scripts} {.tcl}}
    {{All Files} *}
  }
  set f [tk_getOpenFile -filetypes $types -title {Select Tcl Script}]
  set f [RelativePath $f]
  if {[string length $f]>0} {
    global conf
    set conf(MainScript) $f
  }
}

# This routine runs when the user presses the "Browse" button beside
# The Tcl Library entry box.  We want to select the directory that
# contains the Tcl Script library.
#
proc BrowseForTclLib {} {
  global conf
  set f [tk_getDirectory -initialdir $conf(TclLib) -title "Tcl Script Library"]
  if {[string length $f]>0} {
    set conf(TclLib) $f
  }
}

# This routine runs when the user presses the "Browse" button beside
# The Tk Library entry box.  We want to select the directory that
# contains the Tk Script library.
#
proc BrowseForTkLib {} {
  global conf
  set f [tk_getDirectory -initialdir $conf(TkLib) -title "Tk Script Library"]
  if {[string length $f]>0} {
    set conf(TkLib) $f
  }
}

# After the user clicks in the listbox on the C/C++ Modules page,
# this routine runs to update the screen according to what is
# selected.
#
proc SetCSelect {} {
  set w [Notebook:frame .n {C/C++ Modules}]
  set s [$w.c.lb cursel]
  if {[llength $s]>0} {
    $w.b.del config -state normal
    set fn [$w.c.lb get [lindex $s 0]]
  } else {
    $w.b.del config -state disabled
    set fn {}
  }
  ConfigPathButton $w.b.rp $fn
}

# This routine runs when the user presses the "Delete" button on
# the C/C++ Modules page.
#
proc DoDeleteC {} {
  set w [Notebook:frame .n {C/C++ Modules}]
  set s [$w.c.lb cursel]
  global conf
  foreach i $s {
    set file [$w.c.lb get $i]
    catch {unset conf(CFile:$file)}
    $w.c.lb delete $i
  }
  SetCSelect
}

# This routine runs when the user presses the "Relative Path" or
# "Absolute Path" button associated with the list of C/C++ Modules
#
proc CChngPath {} {
  set w [Notebook:frame .n {C/C++ Modules}]
  set s [$w.c.lb cursel]
  global conf
  set new {}
  foreach i $s {
    set file [$w.c.lb get $i]
    if {[info exists conf(CFile:$file)]} {
      set new [TogglePath $file]
      set conf(CFile:$new) $conf(CFile:$file)
      unset conf(CFile:$file)
    }
  }
  InsertC $new
}


# After the user clicks in the listbox on the Tcl Scripts page,
# this routine runs to update the screen according to what is
# selected.
#
proc SetTclSelect {} {
  set w [Notebook:frame .n {Tcl Scripts}]
  set s [$w.c.lb cursel]
  if {[llength $s]>0} {
    $w.b.del config -state normal
    $w.b.stc config -state normal
    set i [lindex $s 0]
    set f [string range [$w.c.lb get $i] 2 end]
    global conf
    if {$conf(TclFile:$f)} {
      $w.b.stc config -text {Don't Strip Comments} \
          -command "DontStrip [list $f]"
    } else {
      $w.b.stc config -text {Do Strip Comments} \
          -command "DoStrip [list $f]"
    }
  } else {
    $w.b.del config -state disabled
    $w.b.stc config -state disabled
    set f {}
  }
  ConfigPathButton $w.b2.rp $f
}

# This routine runs when the user clicks on the "Don't Strip Comments"
# button on the Tcl Scripts page.
#
proc DontStrip f {
  global conf
  set conf(TclFile:$f) 0
  InsertTcl $f
}

# This routine runs when the user clicks on the "Do Strip Comments"
# button on the Tcl Scripts page.
#
proc DoStrip f {
  global conf
  set conf(TclFile:$f) 1
  InsertTcl $f
}

# This routine runs when the user presses the "Delete" button on
# the Tcl Scripts page.
#
proc DoDeleteTcl {} {
  set w [Notebook:frame .n {Tcl Scripts}]
  set s [$w.c.lb cursel]
  global conf
  foreach i $s {
    set file [string range [$w.c.lb get $i] 2 end]
    catch {unset conf(TclFile:$file)}
    $w.c.lb delete $i
  }
  SetTclSelect
}

# This routine runs when the user presses the "Relative Path" or
# "Absolute Path" button associated with the list of Tcl Scripts.
#
proc TclChngPath {} {
  set w [Notebook:frame .n {Tcl Scripts}]
  set s [$w.c.lb cursel]
  global conf
  set new {}
  foreach i $s {
    set file [string range [$w.c.lb get $i] 2 end]
    if {[info exists conf(TclFile:$file)]} {
      set new [TogglePath $file]
      set conf(TclFile:$new) $conf(TclFile:$file)
      unset conf(TclFile:$file)
      if {[string compare $file $conf(MainScript)]==0} {
        set conf(MainScript) $new
      }
    }
  }
  InsertTcl $new
}

# Check for dubious information in the configuration parameters.
# Report an error and return 1 if found.  Return 0 if everything
# looks ok.
#
proc CheckData {} {
  global conf
  set tclFileList [array names conf TclFile:*]
  set res 0
  if {[llength $tclFileList]>0 && [string length $conf(MainScript)]==0} {
    set msg "No \"Startup\" Tcl Script Specified"
    set res [tk_messageBox -icon warning -message $msg -type okcancel]
    set res [string compare $res ok]
  }
  if {[string length $conf(MainScript)]>0 
      && [lsearch -exact $tclFileList TclFile:$conf(MainScript)]<0} {
    set msg "The \"Startup\" Tcl Script Is Not A Built-In Script!"
    set res [tk_messageBox -icon warning -message $msg -type okcancel]
    set res [string compare $res ok]
  }
  return $res
}

# This routine runs when the "Build" button is pressed on the
# Settings page.
#
proc DoBuild {} {
  if {[CheckData]} return
  DoSave
  set nerr 0
  global conf
  if {![info exists conf(OutputFile)] || [string length $conf(OutputFile)]==0} {
    set conf(OutputFile) [file root $conf(ConfigFile)].c
  }
  set h [file root $conf(OutputFile)].h
  if {[catch {exec mktclapp -header >$h} msg]} {
    tk_messageBox -message "Error in command: $msg" \
      -type ok -icon error -title {Error In Build}
    incr nerr
  }
  if {[catch {exec mktclapp -f $conf(ConfigFile) >$conf(OutputFile)} msg]} {
    tk_messageBox -message "Error in command: $msg" \
      -type ok -icon error -title {Error In Build}
    incr nerr
  }
  if {$nerr==0} {
    set msg "Built \"$conf(OutputFile)\" and \"$h\" with "
    append msg "no errors."
    tk_messageBox -message $msg -type ok -icon info -title {Build Complete}
  }
}

# This routine pops up a help dialog.  The help topic is the
# argument.
#
proc DoHelp subject {
  global Help
  if {[winfo exists .help]} {
    destroy .help
  }
  toplevel .help
  wm title .help {Help}
  wm iconname .help {Help}
  button .help.dismiss -text Dismiss -command {catch {destroy .help}}
  pack .help.dismiss -side bottom
  text .help.t -yscrollcommand ".help.sb set" -wrap word -width 60
  pack .help.t -side left -fill both -expand 1
  scrollbar .help.sb -orient vertical -command ".help.t yview"
  pack .help.sb -side right -fill y
  .help.t tag config heading -justify center \
     -font -adobe-helvetica-bold-r-normal-*-18-180-75-75-p-103-iso8859-1
  .help.t tag config bold -font \
     -adobe-helvetica-bold-r-normal-*-14-140-75-75-p-82-iso8859-1
  .help.t tag config normal -justify left \
     -font -adobe-helvetica-medium-r-normal-*-14-140-75-75-p-77-iso8859-1
  if {![info exists Help($subject)]} {
    set msg $Help(unknown)
  } else {
    set msg $Help($subject)
  }
  .help.t delete 1.0 end
  set cnt 0
  set linestart 0
  set ll [llength $msg]
  for {set i 0} {$i<$ll} {incr i} {
    set cmd [lindex $msg $i]
    switch $cmd {
      heading -
      text -
      bold {
        incr i
        set txt [lindex $msg $i]
        regsub -all "\n *" $txt { } txt
      }
    }
    switch $cmd {
      heading {
        if {$cnt>0} {.help.t insert end \n\n heading}
        .help.t insert end $txt\n\n heading
        set linestart 1
      }
      text {
        if {!$linestart} {.help.t insert end " " normal}
        .help.t insert end $txt normal
        set linestart 0
      }
      bold {
        if {!$linestart} {.help.t insert end " " normal}
        .help.t insert end $txt bold
        set linestart 0
      }
      paragraph {
        .help.t insert end "\n\n" normal
        set linestart 1
      }
    }
    incr cnt
  }
  .help.t config -state disabled
}

# The help screens
#
set Help(About) {
   heading {About XMktclapp}
   text {This is version 2.0 of xmktclapp, released on April 19, 1999.
         XMktclapp itself and the associated mktclapp program are both
         covered by the GNU Public License.  The code that
         xmktclapp generates is in the public domain.}
   paragraph
   text {Report bugs to drh@acm.org.}
   paragraph
   text {If you find this program useful, a note to the
         author would be appreciated.  drh@acm.org.}
}

set Help(What) {
   heading {Introduction}
   text {This program, and a related program "mktclapp", are used to help
         convert a collection of Tcl/Tk and C/C++ source files into
         a single stand-alone executable that will run on machines that
         do not have Tcl/Tk installed.}
   paragraph
   text {Fill in the information on the various notebook pages, then
         choose the File/Build menu option.  That will generate a
         C source code file and an associated header file
         that contain all of your Tcl/Tk code
         embedded in static strings.  The generated C code will also
         contain routines to initialize the Tcl/Tk interpreter.}
   paragraph
   text {Most entry boxes and menus have a help button nearby.  Press
         these help buttons for additional information about the particular
         entry box or menu.}
}
   
set Help(unknown) {
   heading {Unknown Topic}
   text {No help is available at this time for the topic you 
         have specified. Sorry...}
}

set Help(Mode) {
   heading {Application Mode}
   text {The "mktclapp" application generator can produce code that uses
         only Tcl (no GUI) or that uses both Tcl and Tk (with a GUI).}
   paragraph
   text {This option is only useful for Unix compilation. Under Windows,
         it should always be set to "Tcl/Tk".}
}

set Help(Autofork) {
   heading {Fork Into Background?}
   text {If you select "Yes" for the "Fork Into Background" option
         then the generated application will automatically
         run in the background, disconnected from its controlling terminal.
         This is often a useful feature for GUIs.}
   paragraph
   text {Only set the option to "Yes" under Unix.  Under MS-Windows always
         set this option to "No".  The Window C compiler will make the
         necessary arrangements to fork Windows GUIs
         into the background.}
   paragraph
   text {It is hard to use a debugger on an application running in the
         background, so while debugging it is best to leave this option
         turned off.  You can always turn it on before a "real" build if
         it is the behavior that you want.}
}

set Help(Standalone) {
   heading {Standalone}
   text {If Standalone is "Yes",
         then the generated code will run on binary-compatible
         machines that do not have Tcl/Tk installed.  If you choose "No", 
         then Tcl/Tk must be installed on the machine for your application
         to work properly.}
   paragraph
   text {Setting Standalone to "Strict" is like setting it to "Yes" but
         with the following addition: When Standalone is "Strict" the
         "source" command of Tcl is modified so that it can only see
         files that have been compiled into your binary.  In other words,
         when Standalone is "Strict", only files listed on the "Tcl Scripts"
         page and in the Tcl/Tk library directories can be sourced.  The
         strict standalone mode helps detect the common bug of omitting
         one or more Tcl scripts from the "Tcl Scripts" page.}
   paragraph
   text {In order to be truely standalone, you must also link your application
         against the static Tcl/Tk libraries, not the dynamic or shared
         libraries.  How you do this depends on your compiler.  Typically,
         you give the compiler an option like "-static" or "-Bstatic".  Or
         you can specify the static Tcl/Tk library files on the compiler
         command line, like this: "/usr/lib/libtcl8.0.a",  instead of using
         the compiler's -l option like this: "-ltcl8.0".}
}

set Help(ConfigFile) {
  heading {Configuration File}
  text {This entry contains the name of a file that holds the
        configuration information used by both mktclapp and xmktclapp.
        By convention, this file has a ".mta" suffix.}
  paragraph
  text {XMktclapp reads in the first configuration
        file it finds in when it is first invoked. You can read a
        different configuration file using the "Open" button.
        To save the current configuration file to a different filename,
        use the "Save As" button.}
}

set Help(OutputFile) {
  heading {Output C File}
  text {This entry contains the name of the file into which C code
        is written when you press the
        "Build" button or choose the File/Build menu option.  If you
        run mktclapp manually, the generated C code appears on
        standard output.}
  paragraph
  text {Pressing the "Build" button also generates a header file.
        The name of the header file is the same as the name of the C
        file except that the suffix is changed to ".h".}
}

set Help(TclLib) {
  heading {Tcl Library}
  text {The Tcl Library is a directory on your computer that contains
        a bunch of Tcl scripts and an index (named "tclIndex") that are
        needed for many applications.  In a stand-alone executable,
        these scripts must be compiled into the executable because they
        might not exist on the target machine.}
  paragraph
  text {The mktclapp program will automatically add the Tcl Library
        scripts to your executable if you select Standalone mode on
        the Settings page.  But you have to tell mktclapp where to go
        to look for the Tcl scripts.  Enter the name of the directory
        that contains the Tcl scripts you want to use here.}
  paragraph
  text {If you have more than one version of Tcl/Tk installed on your
        machine, there will be more than one Tcl Library directory.
        Make sure you chose a Tcl Library that is compatible with the
        Tcl C Library.}
}

set Help(TkLib) {
  heading {Tk Library}
  text {The Tk Library is a directory on your computer that contains
        a bunch of Tcl scripts and an index (named "tclIndex") that are
        needed for many applications.  In a stand-alone executable,
        these scripts must be compiled into the executable because they
        might not exist on the target machine.}
  paragraph
  text {The mktclapp program will automatically add the Tk Library
        scripts to your executable if you select Standalone mode on
        the Settings page.  But you have to tell mktclapp where to go
        to look for the Tcl scripts by entering a directory name here.}
  paragraph
  text {If you have more than one version of Tcl/Tk installed on your
        machine, there will be more than one Tk Library directory.
        Make sure you chose a Tk Library that is compatible with the
        Tk C Library that you are linking against.}
}

set Help(C/C++) {
  heading {C and C++ Source Files}
  text {This page lists all the C and C++ source files that will be
        used by your application.  (Except, the C source file generated
        by this program should not be listed!)}
  paragraph
  text {The mktclapp application generator scans all of the C source files
        you list looking for function definitions with a name of the form
        "ET_COMMAND_aaaaa(ET_TCLARGS)".  For each such function definition
        found, mktclapp will create a new Tcl command named "aaaaa" that
        is implemented by the C function.}
  paragraph
  text {Mktclapp also extracts some other information it needs by scanning
        source files, so it is important to list all the source files for
        your application here, even if they contain no new Tcl command
        implementations.}
}

set Help(Tcl) {
  heading {Tcl Scripts}
  text {This page lists Tcl Scripts that will be converted into C strings
        and compiled into your application. You can invoke any of these
        scripts by executing the Tcl command}
  bold {source FILENAME}
  text {where "FILENAME" is replaced by the exact same text that appears
        in the window.  Note that the FILENAME on the source command must
        be character-by-character identical to the name that appears on
        this page, or the source command will not work}
  paragraph
  text {Only your own Tcl Scripts should be listed here.  The Tcl/Tk Library
        Tcl Scripts are loaded automatically when you select the Standalone
        option on the Settings page.  See the help on the Standalone option,
        and the help on the Libraries page for more information.}
  paragraph
  text {To save space, mktclapp can attempt to strip comments and 
        excess whitespace from your Tcl
        scripts before compiling them into your application.  But on some
        rare occasions, a Tcl script will not work correctly if its comments
        are removed.  On this page, an asterisk appears to the left of
        every Tcl Script which will have its comments removed.  Use the
        "Don't Strip Comments"
        button to turn this feature off if you need to.}
}

set Help(Shroud) {
  heading {Shroud Tcl Scripts}
  text {Normally, the Tcl Scripts that are compiled into your executable
        can be easily extracted and read using the "strings" command of
        Unix.  But if you select the Shroud options, the compiled-in Tcl
        Scripts are encoded in a way that makes them much more difficult
        to read.  Some users may wish to invoke this option in order to
        "protect" their proprietary code from prying eyes.}
  paragraph
  text {Note that shrouding only makes the code more difficult to read. 
        It is not impossible.  A clever hacker can
        still access your code.  But the same is true of C code, which
        can be de-compiled using commercially available tools.  No
        method of code concealment is perfect.}
  paragraph
  text {Recent trends are for source code to be accessible and readable
        by the end user.  We encouraged you to continue this trend by
        leaving the Shroud option turned off.}
}

set Help(ReadStdin) {
  heading {Reading Tcl Commands From Standard Input}
  text {If you turn this option on, then the resulting executable
        will attempt to read Tcl commands from standard input, just
        like "tclsh" and "wish" do when you invoke them without
        arguments.  In fact, the resulting executable uses the same
        initialization procedure as "tclsh" and "wish", which means
        if you supply a filename on the command-line, your program
        will try to execute that file as if it were a Tcl script.}
  paragraph
  text {This option is useful for testing and debugging during
        program development.  It can also be used to create an enhanced
        version of "tclsh" or "wish" that includes extra commands you
        have implemented in C code.}
  paragraph
  text {This option is forced off if you elect select the "Fork Into
        Background" option.}
}

set Help(MainScript) {
  heading {Startup Script}
  text {A Startup Script is a single Tcl script that is run as soon
        as the interpreter has been initialized.  This is the script
        the draws the main screen of an application, or does other
        one-time setup to get the program going.}
  paragraph
  text {If the Startup entry box is blank, no startup script will be run.
        If a Startup script is specified, but cannot be located, or if
        the Startup script contains an error, no error message is reported
        back to the user.}
  paragraph
  text {The Startup script is not automatically compiled into the
        executable.  If you need the Startup script to be compiled into
        the executable (as most applications do) then you must include
        the script in the list of Tcl Scripts to be compiled in, in addition
        to putting it in the Startup entry box.}
}

SetDefaults
frame .mb -bd 2 -relief raised
pack .mb -side top -fill x
menubutton .mb.file -text File -menu .mb.file.m
pack .mb.file -side left -padx 5
set m [menu .mb.file.m]
$m add command -label "Open..." -underline 0 -command DoOpen
$m add command -label "Save" -underline 0 -command DoSave
$m add command -label "Save As..." -underline 5 -command DoSaveAs
$m add command -label "Build" -underline 0 -command DoBuild
$m add separator
$m add command -label Exit -underline 1 -command DoExit

menubutton .mb.help -text Help -menu .mb.help.m
pack .mb.help -side left -padx 5
set m [menu .mb.help.m]
$m add command -label "About This Program..." -underline 0 \
   -command "DoHelp About"
$m add command -label "Introduction..." -underline 0 -command "DoHelp What"

Notebook:create .n -pages {Settings Libraries {C/C++ Modules} {Tcl Scripts}} \
   -pad 10
pack .n -fill both -expand 1
set w [Notebook:frame .n Settings]

proc Page1Option {w text var choices help} {
  frame $w
  pack $w -side top -fill x -pady 3
  label $w.l -text $text -anchor e -width 28
  eval tk_optionMenu $w.e conf($var) $choices
  $w.e config -width 8
  button $w.h -text Help -command "DoHelp $help"
  pack $w.l $w.e -side left
  pack $w.h -side left -fill y
}

frame $w.spacer -height 5
pack $w.spacer -side top
Page1Option $w.f1 {Application Mode} Mode {{Tcl Only} {Tcl/Tk}} Mode
Page1Option $w.f2 {Fork Into Background?} Autofork {Yes No} Autofork
Page1Option $w.f3 {Read Standard Input?} ReadStdin {Yes No} ReadStdin
Page1Option $w.f4 {Standalone?} Standalone {Strict Yes No} Standalone
Page1Option $w.f5 {Shroud Tcl Scripts?} Shroud {Yes No} Shroud
set f [LabelFrame:create $w.f7 -text "Configuration File" -ipadx 10 -ipady 7 -bd 4]
pack $w.f7 -side top -fill x
entry $f.e -bd 2 -relief sunken -bg white -fg black \
   -textvariable conf(ConfigFile) -width 30 -font fixed
pack $f.e -side top -fill x
button $f.open -text Open -command DoOpen
button $f.save -text Save -command DoSave
button $f.saveas -text {Save As} -command DoSaveAs
button $f.help -text Help -command "DoHelp ConfigFile"
pack $f.help $f.saveas $f.save $f.open -side right -pady 5
set f [LabelFrame:create $w.f8 -text "Output C File" -ipadx 10 -ipady 7 -bd 4]
pack $w.f8 -side top -fill x
entry $f.e -bd 2 -relief sunken -bg white -fg black \
   -textvariable conf(OutputFile) -width 30 -font fixed
pack $f.e -side top -fill x
button $f.br -text Browse -command BrowseForOutputFile
button $f.rp -text {Relative Path} -command "RelAbsPath $f.rp conf(OutputFile)"
trace variable conf(OutputFile) w "TracePath $f.rp conf(OutputFile)"
button $f.bld -text {Build} -command DoBuild
button $f.help -text Help -command "DoHelp OutputFile"
pack $f.help $f.bld $f.rp $f.br -side right -pady 5

# An Easter Egg:  Clicking on the Help button within the Output C File
# box while holding down both Control and Shift causes a debugging console
# to come up.  The debugging console can be used to type Tcl commands
# directly into a running instance of this program.  Very useful on
# Windows, since TkCon doesn't work there.
#
bind $f.help <Control-Shift-1> {
  OpenConsole .con {Debug> }
  break
}

set w [Notebook:frame .n {C/C++ Modules}]
frame $w.c
frame $w.b
pack $w.c -side top -fill both -expand 1 -pady 10 -padx 10
pack $w.b -side top -pady 10
button $w.b.ins -text Insert -command DoInsertC
button $w.b.del -text Delete -command DoDeleteC -state disabled
button $w.b.rp -text {Relative Path} -width 12 -command CChngPath
button $w.b.help -text Help -command "DoHelp C/C++"
pack $w.b.ins $w.b.del $w.b.rp $w.b.help -expand 1 -side left
listbox $w.c.lb -yscrollcommand "$w.c.sb set" -bg white -exportselection 0 \
  -width 50 -font fixed -fg black
bind $w.c.lb <1> {after idle SetCSelect}
pack $w.c.lb -side left -fill both -expand 1
scrollbar $w.c.sb -orient vertical -command "$w.c.lb yview"
pack $w.c.sb -side right -fill y
InsertC

set w [Notebook:frame .n {Tcl Scripts}]
frame $w.c
set f1 [LabelFrame:create $w.m -text "Startup Script" -ipadx 10 -ipady 10 -bd 4]
frame $w.b
frame $w.b2
frame $w.sp -height 8
frame $w.bar -height 4 -relief sunken -bd 2
pack $w.sp $w.c -side top -pady 1
pack $w.b $w.b2 -side top -pady 1
pack $w.bar -side top -fill x -pady 5
pack $w.m -side top -pady 15 -fill x -padx 10
button $w.b.ins -text Insert -command DoInsertTcl
button $w.b.del -text Delete -command DoDeleteTcl -state disabled
button $w.b.stc -text {Do Strip Comments} -width 20 -state disabled
button $w.b2.rp -text {Relative Path} -width 12 -state disabled \
  -command TclChngPath
button $w.b2.help -text Help -command "DoHelp Tcl"
pack $w.b.ins $w.b.del $w.b.stc -side left -expand 1
pack $w.b2.rp $w.b2.help -side left
entry $f1.e -bd 2 -bg white -relief sunken -textvariable conf(MainScript) \
      -width 30 -font fixed
pack $f1.e -side top -fill x
button $f1.s -text {Browse} -command BrowseForMainScript
button $f1.rp -text {Relative Path} -width 12 \
   -command "MainScriptChngPath $f1.rp"
trace variable conf(MainScript) w "TracePath $f1.rp conf(MainScript)"
button $f1.h -text {Help} -command "DoHelp MainScript"
pack $f1.s $f1.rp $f1.h -side left
listbox $w.c.lb -yscrollcommand "$w.c.sb set" -bg white -exportselection 0 \
    -width 50 -font fixed -fg black
bind $w.c.lb <1> {after idle SetTclSelect}
pack $w.c.lb -side left -fill both
scrollbar $w.c.sb -orient vertical -command "$w.c.lb yview"
pack $w.c.sb -side right -fill y
InsertTcl

set w [Notebook:frame .n Libraries]
set f1 [LabelFrame:create $w.f1 -text "Tcl Script Library" -ipadx 10 -ipady 2 -bd 4]
entry $f1.e -bd 2 -relief sunken -bg white -textvariable conf(TclLib) \
     -width 40 -font fixed -fg black
pack $f1.e -side top -pady 10 -fill x
button $f1.b -text Browse -command BrowseForTclLib
button $f1.rp -text {Relative Path} -width 12 \
  -command "RelAbsPath $f1.rp conf(TclLib)"
trace variable conf(TclLib) w "TracePath $f1.rp conf(TclLib)"
button $f1.g -text Guess -command {
  catch {set conf(TclLib) $tcl_library}
}
button $f1.h -text Help -command "DoHelp TclLib"
pack $f1.h $f1.g $f1.rp $f1.b -side right
pack $w.f1 -side top -padx 10 -pady 10 -fill x
set f2 [LabelFrame:create $w.f2 -text "Tk Script Library" -ipadx 10 -ipady 2 -bd 4]
entry $f2.e -bd 2 -relief sunken -bg white -textvariable conf(TkLib) \
     -width 40 -font fixed -fg black
pack $f2.e -side top -pady 10 -fill x
button $f2.b -text Browse -command BrowseForTkLib
button $f2.rp -text {Relative Path} -width 12 \
  -command "RelAbsPath $f2.rp conf(TkLib)"
trace variable conf(TkLib) w "TracePath $f2.rp conf(TkLib)"
button $f2.g -text Guess -command {
  catch {set conf(TkLib) $tk_library}
}
button $f2.h -text Help -command "DoHelp TkLib"
pack $f2.h $f2.g $f2.rp $f2.b -side right
pack $w.f2 -side top -padx 10 -pady 10 -fill x

wm withdraw .
update idletasks
set W 300
set H 200
foreach f {{Settings} {Libraries} {C/C++ Modules} {Tcl Scripts}} {
  set w [Notebook:frame .n $f]
  if {[winfo reqwidth $w]>$W} {
    set W [winfo reqwidth $w]
  }
  if {[winfo reqheight $w]>$H} {
    set H [winfo reqheight $w]
  }
}
Notebook:config .n -width $W -height $H
wm deiconify .
wm protocol . WM_DELETE_WINDOW DoExit
wm protocol . WM_SAVE_YOURSELF DoSave
set filelist [glob -nocomplain *.mta]
if {[llength $filelist]==0} {
  InsertC
  InsertTcl
} else {
  set conf(ConfigFile) [lindex [lsort $filelist] 0]
  ReadState $conf(ConfigFile) 1
  Baseline
}
