#+
#  Name:
#     ScrollHTML

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     HTML rendering widget with scrollbars.

#  Description:
#     This class uses Richard Hipps TkHTML widget and adds scrollbars
#     to it. It also provides a default set of methods for rendering a
#     given page and provides a history mechanism for returning to
#     prior and later pages that have been visited.

#  Invocations:
#
#        ScrollHTML window [-option value]...
#
#     This command create an instance of a scrolltext and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option definitions below


#  Methods:
#     See method definitions below.

#  Inheritance:
#     FrameWidget

#  Copyright:
#     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter W. Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     02-MAY-2001 (PWD):
#     	 Original version.
#     {enter_further_changes_here}

#-

itk::usual ScrollHTML {}

itcl::class gaia::ScrollHTML {

   #  Inheritances:
   inherit util::FrameWidget

   #  Constructor.
   #  ------------
   constructor {args} {

      #  Create the label.
      itk_component add label {
         label $w_.label
      }

      #  Create the scrollbars.
      itk_component add vscroll {
         scrollbar $w_.vscroll -orient vertical
      }

      #  Horizontal fits into frame that has a corner padding element.
      itk_component add hframe {
         frame $w_.hframe -borderwidth 0
      }
      set rwidth [winfo reqwidth $itk_component(vscroll)]

      itk_component add corner {
         frame $itk_component(hframe).corner -width $rwidth
      }

      itk_component add hscroll {
         scrollbar $itk_component(hframe).hscroll -orient horizontal
      }

      #  Create the html widget adding all the necessary callbacks for
      #  a simple rendering system.
      itk_component add html {
         ::html $w_.html \
            -yscrollcommand "$itk_component(vscroll) set" \
            -xscrollcommand "$itk_component(hscroll) set" \
            -formcommand [code $this formcommand_] \
            -imagecommand [code $this imagecommand_]\
            -scriptcommand [code $this scriptcommand_] \
            -appletcommand [code $this appletcommand_] \
            -hyperlinkcommand [code $this hyperlinkcommand_] \
            -underlinehyperlinks 0 \
            -tablerelief raised \
            -unvisitedcolor blue \
            -takefocus 1
      } {
         keep \
            -width -height -exportselection -foreground -background \
            -padx -pady -tablerelief -underlinehyperlinks
      }
      $itk_component(hscroll) configure -command "$itk_component(html) xview"
      $itk_component(vscroll) configure -command "$itk_component(html) yview"

      #  This widget also needs a couple of bindings to work. One to
      #  pick up hyperlink events (XXX I thought -hyperlinkcommand
      #  should do this) and one to change the cursor as hyperlinks
      #  are passed over.
      bind HtmlClip <1> [code $this hyperlinkcommand_ %x %y]
      catch {focus $itk_component(html).x}
      bind HtmlClip <Motion> {
         set parent [winfo parent %W]
         set url [$parent href %x %y]
         if {[string length $url] > 0} {
            $parent configure -cursor hand2
         } else {
            $parent configure -cursor {}
         }
      }

      #  Add bindings for keyboard movements.
      bind HtmlClip <Key-Next> [code $this pageforward_]
      bind HtmlClip <Key-BackSpace> [code $this pageback_]
      bind HtmlClip <Key-Prior> [code $this pageback_]
      bind HtmlClip <Key-Delete> [code $this pageback_]
      bind HtmlClip <Key-Down> [code $this lineforward_]
      bind HtmlClip <Key-Up> [code $this lineback_]

      #  Now pack everything into place.
      pack $itk_component(label) -side top -fill x
      pack $itk_component(hframe) -side bottom -fill x
      pack $itk_component(corner) -side right -fill y
      pack $itk_component(hscroll) -side bottom -fill x
      pack $itk_component(vscroll) -side right  -fill y
      pack $itk_component(html) -expand true -fill both

      #  Do initialisations.
      eval itk_initialize $args
   }

   #  Methods.
   #  --------

   #  This method is called for each form element. These are just set
   #  to the not available image for types that require a window.
   protected method formcommand_ {n cmd args} {
      switch $cmd {
         select -
         textarea -
         input {
            set w [lindex $args 0]
            label $w -image [scope nogifsm]
         }
      }
   }

   #  This method is called for every <IMG> markup. The arguments are
   #  the value of the "src" parameter (in a resolved form), followed
   #  by the other parameters (width, height, everything else).
   protected method imagecommand_ {args} {

      #  Get the image source.
      set imgsrc [lindex $args 0]

      #  If we have this image already then re-use it.
      if { [info exists images_($imgsrc)] } {
         return $images_($imgsrc)
      }

      #  Otherwise create the image. Failures (i.e. not available)
      #  return a small gray image.
      if {[catch {image create photo -file $imgsrc} img]} {
         return smgray
      }
      set images_($imgsrc) $img
      return $img
   }

   #  This method is called for every <SCRIPT> markup. Does nothing.
   protected method scriptcommand_ {args} {
   }

   #  This method is called for every <APPLET> markup. Just shows a
   #  label with the applet name.
   protected method appletcommand_ {w arglist} {
      label $w -text "The Applet $w" -bd 2 -relief raised
   }

   #  This method is called when the user clicks on a hyperlink. There
   #  are really two cases to deal with here, when a new page is to be
   #  loaded and when we need to move to an anchor in the current
   #  page. The args are either the name of the hyperlink, or the X
   #  and Y coordinates of a potential hyperlink.
   protected method hyperlinkcommand_ { args } {
      catch {focus $itk_component(html).x}
      if { [llength $args] > 1 } {
         set url [$itk_component(html) href [lindex $args 0] [lindex $args 1]]
         if { $url == {} } return
      } else {
         set url [lindex $args 0]
      }

      #  If the file is same as last time look for an anchor.
      set pattern "$current_\#"
      set len [string length $pattern]
      incr len -1
      if { [string range $url 0 $len] == $pattern } {
         incr len
         $itk_component(html) yview [string range $new $len end]
      } else {
         loadfile $url
      }
   }

   #  Clear the widget. Clear all images that have been created.
   public method clear {} {
      $itk_component(html) clear
      if { [info exists images_] } {
         foreach imgsrc [array names images_] {
            image delete $images_($imgsrc)
         }
         catch {unset images_}
      }
   }

   #  Clear the history.
   public method clear_history {} {
      catch {unset history_}
      set nhist_ 0
   }

   #  Read a file, returning the contents as the results.
   protected method readfile_ {url} {
      if { ![regexp {(.*)\#(.*)} $url dummy filename anchor] } {
         set filename $url
         set anchor {}
      }
      if {[catch {open $filename r} fp]} {
         error $fp
         return {}
      } else {
         fconfigure $fp -translation binary
         set r [read $fp [file size $filename]]
         close $fp
         return $r
      }
   }

   #  Load a file into the HTML widget. Update history if needed.
   public method loadfile {url {update 1} } {
      set html [readfile_ $url]
      if { $html == "" } return
      clear
      if { $update } {happend_ $url}
      set current_ $url
      $itk_component(html) configure -base $url
      $itk_component(html) parse $html
   }

   #  Just show some HTML in the widget.
   public method parse {text {clear 1}} {
      if { $clear } clear
      $itk_component(html) parse $text
   }

   #  Refresh the current file.
   public method refresh {} {
      if { $current_ == {} } return
      loadfile $current_
   }

   #  Append a file to the history record. Do not append if this is
   #  the same as the current file.
   protected method happend_ {name} {
      set history_($nhist_) $name
      incr nhist_
   }

   #  Return the file associated with a history position.
   public method history {n} {
      if { [info exists history_($n)] } {
         return history_($n)
      }
   }

   #  Display the "previous" file.
   public method previous {} {
      incr nhist_ -2
      if { [info exists history_($nhist_)] } {
         loadfile $history_($nhist_) 0
      } else {
         incr nhist_ 2
      }
   }

   #  Display the "next" file.
   public method next {} {
      if { [info exists history_($nhist_)] } {
         loadfile $history_($nhist_) 0
         incr nhist_
      }
   }

   #  Callback for page forward shortcut key.
   protected method pageforward_ {} {
      $itk_component(html) yview scroll 1 pages
   }

   #  Callback for page back shortcut key.
   protected method pageback_ {} {
      $itk_component(html) yview scroll -1 pages
   }

   #  Callback for line forward shortcut key
   protected method lineforward_ {} {
      $itk_component(html) yview scroll 1 units
   }

   #  Callback for line back shortcut key
   protected method lineback_ {} {
      $itk_component(html) yview scroll -1 units
   }

   #  Protected variables:
   #  --------------------

   #  Array of all the loaded images. The index if the file name.
   protected variable images_

   #  Name of the file that we're looking at.
   protected variable current_ {}

   #  The names of last 20 URLs that we've visited.
   protected variable history_

   #  The current insertion position in the history.
   protected variable nhist_ 0

   #  Configuration options:
   #  ----------------------

   #  Set text of label.
   itk_option define -label scrolltextlabel ScrolltextLabel {} {
      if { [info exists itk_component(label)] && $itk_option(-label) != {}} {
         $itk_component(label) configure -text "$itk_option(-label)"
      } else {
         pack unpack $itk_component(label)
      }
   }


   #  Shared images
   #  -------------
   #  These images are used in place of GIFs or of form elements

   common smgray {}
   image create photo smgray -data {
      R0lGODdhOAAYAPAAALi4uAAAACwAAAAAOAAYAAACI4SPqcvtD
      6OctNqLs968+w+G4kiW5omm6sq27gvH8kzX9m0VADv/
   }

   common nogifsm {}
   image create photo nogifsm -data {
      R0lGODdhEAAQAPEAAACQkADQ0PgAAAAAACwAAAAAEAAQAAACN
      ISPacHtD4IQz80QJ60as25d3idKZdR0IIOm2ta0Lhw/Lz2S1J
      qvK8ozbTKlEIVYceWSjwIAO///
   }
}
