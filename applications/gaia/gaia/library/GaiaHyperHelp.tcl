#
# GaiaHyperHelp
# ----------------------------------------------------------------------
# Implements a help facility using html formatted hypertext files.
#
# ----------------------------------------------------------------------
#  AUTHOR: Kris Raney                   EMAIL: kraney@spd.dsccc.com
#  AUTHOR: Peter W. Draper              EMAIL: p.w.draper@durham.ac.uk
#
#  @(#) $Id$
# ----------------------------------------------------------------------
#            Copyright (c) 1996 DSC Technologies Corporation
# ======================================================================
# Permission to use, copy, modify, distribute and license this software
# and its documentation for any purpose, and without fee or written
# agreement with DSC, is hereby granted, provided that the above copyright
# notice appears in all copies and that both the copyright notice and
# warranty disclaimer below appear in supporting documentation, and that
# the names of DSC Technologies Corporation or DSC Communications
# Corporation not be used in advertising or publicity pertaining to the
# software without specific, written prior permission.
#
# DSC DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, AND NON-
# INFRINGEMENT. THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, AND THE
# AUTHORS AND DISTRIBUTORS HAVE NO OBLIGATION TO PROVIDE MAINTENANCE,
# SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS. IN NO EVENT SHALL
# DSC BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
# ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTUOUS ACTION,
# ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
# SOFTWARE.
# ======================================================================

#
# Acknowledgements:
#
# Special thanks go to Sam Shen(SLShen@lbl.gov), as this code is based on his
# help.tcl code from tk inspect.

#
# Default resources.
#
option add *GaiaHyperHelp.width 575 widgetDefault
option add *GaiaHyperHelp.height 450 widgetDefault
option add *GaiaHyperHelp.modality none widgetDefault
option add *GaiaHyperHelp.vscrollMode static widgetDefault
option add *GaiaHyperHelp.hscrollMode static widgetDefault
option add *GaiaHyperHelp.maxHistory 20 widgetDefault

#
# Usual options.
#
itk::usual GaiaHyperHelp {
    keep -activebackground -activerelief -background -borderwidth -cursor \
         -foreground -highlightcolor -highlightthickness \
         -selectbackground -selectborderwidth -selectforeground \
         -textbackground
}
 
# ------------------------------------------------------------------
#                          HYPERHELP
# ------------------------------------------------------------------
class gaia::GaiaHyperHelp {
    inherit iwidgets::Shell

    constructor {args} {}
    destructor {}

    itk_option define -topics topics Topics {}
    itk_option define -helpdir helpdir Directory .
    itk_option define -title title Title "Help"
    itk_option define -closecmd closeCmd CloseCmd {}
    itk_option define -maxhistory maxHistory MaxHistory 20

    public variable beforelink {}
    public variable afterlink {}

    public method showtopic {topic}
    public method followlink {link}
    public method home {}
    public method forward {}
    public method back {}
    public method search {}
    public method updatefeedback {n}

    protected method _readtopic {file {anchorpoint {}}}
    protected method _pageforward {}
    protected method _pageback {}
    protected method _lineforward {}
    protected method _lineback {}
    protected method _fill_go_menu {}

    protected variable _history {}      ;# History list of viewed pages
    protected variable _history_ndx -1  ;# current position in history list
    protected variable _history_len 0   ;# length of history list
    protected variable _histdir -1      ;# direction in history we just came 
                                        ;# from
    protected variable _len 0           ;# length of text to be rendered
    protected variable _file {}         ;# current topic

    private variable _remaining 0       ;# remaining text to be rendered
    private variable _rendering 0       ;# flag - in process of rendering

    private variable _tmpfile {}        ;# search result file
}

#
# Provide a lowercased access method for the Scrolledlistbox class.
#
proc ::gaia::hyperhelp {pathName args} {
    uplevel ::gaia::GaiaHyperHelp $pathName $args
}

# ------------------------------------------------------------------
#                        CONSTRUCTOR
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::constructor {args} {
    itk_option remove iwidgets::Shell::padx iwidgets::Shell::pady

    #
    # Create a pulldown menu
    #
    itk_component add -private menubar {
      frame $itk_interior.menu -relief raised -bd 2
    } {
      keep -background -cursor
    }
    pack $itk_component(menubar) -side top -fill x

    itk_component add -private topicmb {
      menubutton $itk_component(menubar).topicmb -text "File" \
           -menu $itk_component(menubar).topicmb.topicmenu \
           -underline 0 -padx 8 -pady 2
    } {
      keep -background -cursor -font -foreground \
              -activebackground -activeforeground
    }
    pack $itk_component(topicmb) -side left

    itk_component add -private topicmenu {
      menu $itk_component(topicmb).topicmenu -tearoff no
    } {
      keep -background -cursor -font -foreground \
              -activebackground -activeforeground
    }

    itk_component add -private navmb {
      menubutton $itk_component(menubar).navmb -text "Options" \
          -menu $itk_component(menubar).navmb.navmenu \
          -underline 0 -padx 8 -pady 2
    } {
      keep -background -cursor -font -foreground \
             -activebackground -activeforeground
    }
    pack $itk_component(navmb) -side left

    itk_component add -private navmenu {
      menu $itk_component(navmb).navmenu -tearoff no
    } {
      keep -background -cursor -font -foreground \
              -activebackground -activeforeground
    }
    set m $itk_component(navmenu)
    $m add command -label "Forward" -underline 0 -state disabled \
       -command [code $this forward] -accelerator {Alt-f}
    $m add command -label "Back" -underline 0 -state disabled \
       -command [code $this back] -accelerator {Alt-b}
    $m add cascade -label "Go" -underline 0 -menu $m.go

    itk_component add -private navgo {
      menu $itk_component(navmenu).go -postcommand [code $this _fill_go_menu]
    } {
      keep -background -cursor -font -foreground \
              -activebackground -activeforeground
    }

    #
    # Buttons for quick navigation.
    #
    itk_component add navbar {
       ::frame $itk_interior.navbar
    } {
       keep -background -cursor
    }
    pack $itk_component(navbar) -fill x -expand 1 -side top
    
    itk_component add home {
       button $itk_component(navbar).home \
          -bitmap up_arrow \
          -command [code $this home]
    }
    pack $itk_component(home) -side left -pady 2 -padx 2

    itk_component add backward {
       button $itk_component(navbar).backward \
          -bitmap left_arrow \
          -command [code $this back]
    }
    pack $itk_component(backward) -side left -pady 2 -padx 2

    itk_component add forward {
       button $itk_component(navbar).forward \
          -bitmap right_arrow \
          -command [code $this forward]
    }
    pack $itk_component(forward) -side left -pady 2 -padx 2

    #
    # Search for text in the help (all GAIA help).
    #
    itk_component add searchbutton {
       button $itk_component(navbar).sbutton \
          -text "Search" \
          -command [code $this search]
    }
    pack $itk_component(searchbutton) -side right

    itk_component add searchtext {
       entry $itk_component(navbar).stext
    }
    pack $itk_component(searchtext) -side right
    bind $itk_component(searchtext) <Return> [code $this search]

    # Initialize help file search path and name for search results.
    global gaia_help env
    set env(HTX_PATH) [file dirname $gaia_help]
    set _tmpfile "/tmp/GaiaSearch[pid].html"

    # 
    # Add window action buttons for bottom.
    #
    itk_component add actionbar {
       ::frame $itk_interior.actionbar
    } {
       keep -background -cursor
    }
    pack $itk_component(actionbar) -fill x -expand 1 -side bottom

    itk_component add closebutton {
       button $itk_component(actionbar).close \
          -text "Close" \
          -command [code $this deactivate]
    }
    pack $itk_component(closebutton) -fill none \
       -expand 1 -side bottom -pady 2 -padx 2
    
    #
    # Create a scrolledhtml object to display help pages
    #
    itk_component add scrtxt {
      iwidgets::scrolledhtml $itk_interior.scrtxt \
         -linkcommand [code $this followlink] \
         -feedback [code $this updatefeedback]
    } {
        keep \
           -hscrollmode -vscrollmode -background -textbackground \
           -fontname -fontsize -fixedfont -link \
           -linkhighlight -borderwidth -cursor -sbwidth -scrollmargin \
           -width -height -foreground -highlightcolor -visibleitems \
           -highlightthickness -padx -pady -activerelief \
           -relief -selectbackground -selectborderwidth \
           -selectforeground -setgrid -wrap -unknownimage
    }
    pack $itk_component(scrtxt) -fill both -expand yes -side top

    #
    # Bind shortcut keys
    #
    bind $itk_component(hull) <Alt-f> "[code $this forward];break"
    bind $itk_component(hull) <Alt-b> "[code $this back];break"
    bind $itk_component(hull) <Alt-Right> "[code $this forward];break"
    bind $itk_component(hull) <Alt-Left> "[code $this back];break"
    bind $itk_component(hull) <Key-Next> "[code $this _pageforward];break"
    bind $itk_component(hull) <Key-BackSpace> "[code $this _pageback];break"
    bind $itk_component(hull) <Key-Prior> "[code $this _pageback];break"
    bind $itk_component(hull) <Key-Delete> "[code $this _pageback];break"
    bind $itk_component(hull) <Key-Down> "[code $this _lineforward];break"
    bind $itk_component(hull) <Key-Up> "[code $this _lineback];break"

    wm title $itk_component(hull) "Help"

    eval itk_initialize $args
    if {[lsearch -exact $args -closecmd] == -1} {
      configure -closecmd [code $this deactivate]
    }
}

# ------------------------------------------------------------------
#                        DESTRUCTOR
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::destructor {} {
   if { $_tmpfile != {} } {
      file delete $_tmpfile
   }
}

# ------------------------------------------------------------------
#                             OPTIONS
# ------------------------------------------------------------------
 
# ------------------------------------------------------------------
# OPTION: -topics
#
# Specifies the topics to display on the menu. For each topic, there should
# be a file named <helpdir>/<topic>.html
# ------------------------------------------------------------------
configbody gaia::GaiaHyperHelp::topics {
    set m $itk_component(topicmenu)
    $m delete 0 last
    foreach topic $itk_option(-topics) {
      if {[lindex $topic 1] == {} } {
        $m add radiobutton -variable topic \
          -value $topic \
          -label $topic \
          -command [list $this showtopic $topic]
      } else {
        if {[string index [file dirname [lindex $topic 1]] 0] != "/" && \
            [string index [file dirname [lindex $topic 1]] 0] != "~"} {
          set link $itk_option(-helpdir)/[lindex $topic 1]
        } else {
          set link [lindex $topic 1]
        }
        $m add radiobutton -variable topic \
          -value [lindex $topic 0] \
          -label [lindex $topic 0] \
          -command [list $this followlink $link]
      }
    }
    $m add separator
    $m add command -label "Close Window" -underline 0 \
      -command $itk_option(-closecmd)
}

# ------------------------------------------------------------------
# OPTION: -title
#
# Specify the window title.
# ------------------------------------------------------------------
configbody gaia::GaiaHyperHelp::title {
    wm title $itk_component(hull) $itk_option(-title)
}

# ------------------------------------------------------------------
# OPTION: -helpdir
#
# Set location of help files
# ------------------------------------------------------------------
configbody gaia::GaiaHyperHelp::helpdir {
    if {[file pathtype $itk_option(-helpdir)] == "relative"} {
      configure -helpdir [file join [pwd] $itk_option(-helpdir)]
    } else {
      set _history {}
      set _history_len 0
      set _history_ndx -1
      $itk_component(navmenu) entryconfig 0 -state disabled
      $itk_component(navmenu) entryconfig 1 -state disabled
      configure -topics $itk_option(-topics)
   }
}

# ------------------------------------------------------------------
# OPTION: -closecmd
#
# Specify the command to execute when close is selected from the menu
# ------------------------------------------------------------------
configbody gaia::GaiaHyperHelp::closecmd {
  $itk_component(topicmenu) entryconfigure last -command $itk_option(-closecmd) 
}

# ------------------------------------------------------------------
#                            METHODS
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# METHOD: showtopic topic
#
# render text of help topic <topic>. The text is expected to be found in
# <helpdir>/<topic>.html
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::showtopic {topic} {
  if ![regexp {(.*)#(.*)} $topic dummy topicname anchorpart] {
    set topicname $topic
    set anchorpart {}
  }
  if {$topicname == ""} {
    set topicname $_file
    set filepath $_file
  } else {
    set filepath $itk_option(-helpdir)/$topicname.html
  }
  if {[incr _history_ndx] < $itk_option(-maxhistory)} {
    set _history [lrange $_history 0 [expr $_history_ndx - 1]]
    set _history_len [expr $_history_ndx + 1]
  } else {
    incr _history_ndx -1
    set _history [lrange $_history 1 $_history_ndx]
    set _history_len [expr $_history_ndx + 1]
  }
  lappend _history [list $topicname $filepath $anchorpart]
  _readtopic $filepath $anchorpart
}

# ------------------------------------------------------------------
# METHOD: followlink link
#
# Callback for click on a link. Shows new topic.
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::followlink {link} {
   if {[string compare $beforelink ""] != 0} {
      eval $beforelink $link
   }
   if ![regexp {(.*)\#(.*)} $link dummy filepart anchorpart] {
      set filepart $link
      set anchorpart {}
   }

   if {$filepart != "" && [string index [file dirname $filepart] 0] != "/" && \
          [string index [file dirname $filepart] 0] != "~"} {
      set filepart [$itk_component(scrtxt) pwd]/$filepart
      set hfile $filepart
   } else {
      set hfile $_file
   }

   incr _history_ndx
   set _history [lrange $_history 0 [expr $_history_ndx - 1]]
   set _history_len [expr $_history_ndx + 1]
   lappend _history [list [file rootname [file tail $hfile]] $hfile $anchorpart]
   set ret [_readtopic $filepart $anchorpart]
   if {[string compare $afterlink ""] != 0} {
      eval $afterlink $link
   }
   return $ret
}

# ------------------------------------------------------------------
# METHOD: home
#
# Show the "home" topic (i.e. first on history).
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::home {} {
   if {$_rendering} return
   eval _readtopic [lrange [lindex $_history 0] 1 end]
}

# ------------------------------------------------------------------
# METHOD: forward
#
# Show topic one forward in history list
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::forward {} {
    if {$_rendering || ($_history_ndx+1) >= $_history_len} return
    incr _history_ndx
    eval _readtopic [lrange [lindex $_history $_history_ndx] 1 end]
}

# ------------------------------------------------------------------
# METHOD: back
#
# Show topic one back in history list
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::back {} {
    if {$_rendering || $_history_ndx <= 0} return
    incr _history_ndx -1
    set _histdir 1
    eval _readtopic [lrange [lindex $_history $_history_ndx] 1 end]
}

# ------------------------------------------------------------------
# METHOD: search
#
# Search the complete help system for a word. Display the results
# as an index. Assumes the Starlink "findme" command is available.
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::search {} {
   file delete $_tmpfile
   catch {
      exec findme -q -warn -html -f -m -s \
         [$itk_component(searchtext) get] > $_tmpfile
   }
   incr _history_ndx
   set _history [lrange $_history 0 [expr $_history_ndx - 1]]
   set _history_len [expr $_history_ndx + 1]
   lappend _history [list [file rootname [file tail $_tmpfile]] $_tmpfile {}]
   set _file {}
   set ret [_readtopic $_tmpfile {}]
}

# ------------------------------------------------------------------
# METHOD: updatefeedback remaining
#
# Callback from text to update feedback widget
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::updatefeedback {n} {
    if {($_remaining - $n) > .1*$_len} {
      [$itk_interior.feedbackshell childsite].helpfeedback step [expr $_remaining - $n]
      update idletasks
      set _remaining $n
    }
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _readtopic 
#
# Read in file, render it in text area, and jump to anchorpoint
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_readtopic {file {anchorpoint {}}} {
  if {$file != ""} {
    if {[string compare $file $_file] != 0} {
      if {[catch {set f [open $file r]} err]} {
        incr _history_ndx $_histdir
        set _history_len [expr $_history_ndx + 1]
        set _histdir -1
        set m $itk_component(navmenu)
        if {($_history_ndx+1) < $_history_len} {
          $m entryconfig 0 -state normal
        } else {
          $m entryconfig 0 -state disabled
        }
        if {$_history_ndx > 0} {
          $m entryconfig 1 -state normal
        } else {
          $m entryconfig 1 -state disabled
        }
        error $err
      }
      set _file $file
      set txt [read $f]
      iwidgets::shell $itk_interior.feedbackshell -title "Rendering HTML" -padx 1 -pady 1
      iwidgets::Feedback [$itk_interior.feedbackshell childsite].helpfeedback \
          -steps [set _len [string length $txt]] \
          -labeltext "Rendering HTML" -labelpos n
      pack [$itk_interior.feedbackshell childsite].helpfeedback
      $itk_interior.feedbackshell center $itk_interior
      $itk_interior.feedbackshell activate
      set _remaining $_len
      set _rendering 1
      if [catch {$itk_component(scrtxt) render $txt [file dirname $file]} err] {
          if [regexp "</pre>" $err] {
            $itk_component(scrtxt) render "<tt>$err</tt>"
          } else {
            $itk_component(scrtxt) render "<pre>$err</pre>"
          }
      }
      wm title $itk_component(hull) "Help: $file"
      delete object [$itk_interior.feedbackshell childsite].helpfeedback
      delete object $itk_interior.feedbackshell
      set _rendering 0
    }
  }
  set m $itk_component(navmenu)
  if {($_history_ndx+1) < $_history_len} {
    $m entryconfig 0 -state normal
  } else {
    $m entryconfig 0 -state disabled
  }
  if {$_history_ndx > 0} {
    $m entryconfig 1 -state normal
  } else {
    $m entryconfig 1 -state disabled
  }
  if {$anchorpoint != "{}"} {
    $itk_component(scrtxt) import -link #$anchorpoint
  } else {
    $itk_component(scrtxt) import -link #
  }
  set _histdir -1
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _fill_go_menu
#
# update go submenu with current history
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_fill_go_menu {} {
    set m $itk_component(navgo)
    catch {$m delete 0 last}
    for {set i [expr $_history_len - 1]} {$i >= 0} {incr i -1} {
      set topic [lindex [lindex $_history $i] 0]
      set filepath [lindex [lindex $_history $i] 1]
      set anchor [lindex [lindex $_history $i] 2]
      $m add command -label $topic \
         -command [list $this followlink $filepath#$anchor]
    }
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _pageforward
#
# Callback for page forward shortcut key
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_pageforward {} {
    $itk_component(scrtxt) yview scroll 1 pages
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _pageback
#
# Callback for page back shortcut key
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_pageback {} {
    $itk_component(scrtxt) yview scroll -1 pages
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _lineforward
#
# Callback for line forward shortcut key
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_lineforward {} { 
    $itk_component(scrtxt) yview scroll 1 units 
}

# ------------------------------------------------------------------
# PRIVATE METHOD: _lineback
#
# Callback for line back shortcut key
# ------------------------------------------------------------------
body gaia::GaiaHyperHelp::_lineback {} { 
    $itk_component(scrtxt) yview scroll -1 units 
}
