#+
#  Name:
#     GaiaHyperHelp

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Implements a help facility using html formatted hypertext files.

#  Description:

#  Invocations:
#
#        GaiaHyperHelp object_name [configuration options]
#
#     This creates an instance of a GaiaHyperHelp object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     05-MAY-2001 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaHyperHelp {}

itcl::class gaia::GaiaHyperHelp {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set top-level window title.
      wm title $w_ "GAIA: Global help"

      #  Add the File and Options menu.
      add_menubar
      set filemenu_ [add_menubutton "File"]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add Forward and Backward to options menu.
      $Options add command -label {Forward} -command [code $this forward] \
         -accelerator {Alt-f}
      $Options add command -label {Back} -command [code $this back] \
         -accelerator {Alt-b}

      #  Add buttons for quick navigation to the "toolbar"
      itk_component add toolbar {
         ::frame $w_.toolbar
      }

      itk_component add home {
         button $itk_component(toolbar).home \
            -bitmap up_arrow \
            -command [code $this home]
      }

      itk_component add backward {
         button $itk_component(toolbar).backward \
            -bitmap left_arrow \
            -command [code $this back]
      }

      itk_component add forward {
         button $itk_component(toolbar).forward \
            -bitmap right_arrow \
            -command [code $this forward]
      }

      #  Search for text in the help (all GAIA help).
      itk_component add searchbutton {
         button $itk_component(toolbar).sbutton \
            -text "Search" \
            -command [code $this search]
      }

      itk_component add searchtext {
         entry $itk_component(toolbar).stext
      }

      #  <Return> in search entry starts search.
      bind $itk_component(searchtext) <Return> [code $this search]

      #  Initialize help file search path and name for search results.
      global gaia_help env
      set env(HTX_PATH) [file dirname $gaia_help]
      set tmpfile_ "/tmp/GaiaSearch[pid].html"

      #  Set up action bar for bottom of window.
      itk_component add actionbar {
         ::frame $itk_interior.actionbar
      }

      itk_component add closebutton {
         button $itk_component(actionbar).close \
            -text {Close} \
            -command "wm withdraw $w_"
      }

      #  Create a ScrollHTML object to display the HTML help pages
      itk_component add html {
         gaia::ScrollHTML $w_.html
      }

      #  Add bindings for shortcut keys that move through the history.
      bind $w_ <Alt-f> "[code $this forward];break"
      bind $w_ <Alt-b> "[code $this back];break"
      bind $w_ <Alt-Right> "[code $this forward];break"
      bind $w_ <Alt-Left> "[code $this back];break"

      #  Pack all components.
      pack $itk_component(toolbar) -fill x -expand 0 -side top
      pack $itk_component(home) -side left -pady 2 -padx 2
      pack $itk_component(backward) -side left -pady 2 -padx 2
      pack $itk_component(forward) -side left -pady 2 -padx 2
      pack $itk_component(searchbutton) -side right -pady 2 -padx 2
      pack $itk_component(searchtext) -side right -pady 2 -padx 2

      pack $itk_component(actionbar) -fill x -expand 0 -side bottom
      pack $itk_component(closebutton) -fill none \
         -expand 1 -side bottom -pady 2 -padx 2

      pack $itk_component(html) -fill both -expand true -side top

      #  Evaluate any options (sets topics and help directory).
      eval itk_initialize $args

      #  Load first topic
      home
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $tmpfile_ != {} } {
         file delete $tmpfile_
      }
   }

   #  Methods:
   #  --------

   #  METHOD: init
   #
   #  Called after construction is complete.
   public method init {} {
   }

   #  METHOD: showtopic topic
   #
   #  Render text of help topic <topic>. The text is expected to be
   #  found in <helpdir>/<topic>.html
   public method showtopic {topic} {
      if { $topic != ""} {
         if ![regexp {(.*)\#(.*)} $topic dummy topicname anchorpart] {
            set topicname $topic
            set anchorpart {}
         }
         set url $itk_option(-helpdir)/${topic}.html${anchorpart}
         readtopic_ $url
      }
   }

   # METHOD: home
   #
   # Show the "home" topic, i.e. first in menu.
   public method home {} {
      if { $rendering_ || $filemenu_ == {} } return
      $itk_component(html) clear_history
      $filemenu_ invoke 0
   }

   # METHOD: hometopic
   #
   # Show a menu topic, clearing history.
   public method hometopic { topic } {
      $itk_component(html) clear_history
      showtopic $topic
   }

   # METHOD: forward
   #
   # Show topic one forward in history list
   public method forward {} {
      $itk_component(html) next
   }

   # METHOD: back
   #
   # Show topic one back in history list
   public method back {} {
      $itk_component(html) previous
   }

   # METHOD: search
   #
   # Search the complete help system for a word. Display the results
   # as an index. Assumes the Starlink "findme" command is available.
   public method search {} {
      file delete $tmpfile_
      catch {
#         23rd Jan 2003 "-s" option currently broken under Linux.
#         exec findme -q -warn -html -f -m -s \
#            [$itk_component(searchtext) get] > $tmpfile_
         exec findme -q -warn -html -f -m \
            [$itk_component(searchtext) get] > $tmpfile_
      }
      set ret [readtopic_ $tmpfile_]
   }

   #  METHOD: readtopic_
   #
   #  Read a URL and render it.
   protected method readtopic_ {url} {
      if { $url != "" } {
         set rendering_ 1
         if [catch {$itk_component(html) loadfile $url} err] {
            if [regexp "</pre>" $err] {
               $itk_component(html) parse "<tt>$err</tt>"
            } else {
               $itk_component(html) parse "<pre>$err</pre>"
            }
         }
         wm title $w_ "Help: $url"
         set rendering_ 0
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  OPTION -topics {{topic file} {topic file}...}
   #
   #  Specifies the main topics to display on the File menu. For each
   #  topic, there should be a file named <helpdir>/<file>.html, or
   #  the full file name should be given.
   itk_option define -topics topics Topics {} {
      if { $itk_option(-topics) == {} || $filemenu_ == {} } return

      #  Remove previous topics and other menu items.
      set m $filemenu_
      $filemenu_ delete 0 last

      #  And add new ones.
      foreach topic $itk_option(-topics) {
         $filemenu_ add command \
            -label [lindex $topic 0] \
            -command [code $this hometopic [lindex $topic 1]]
      }

      #  Add final menu items.
      $filemenu_ add separator
      $filemenu_ add command -label {Close Window} -command "wm withdraw $w_"
   }

   # OPTION: -helpdir {directory}
   #
   # Set location of help files
   # ------------------------------------------------------------------
   itk_option define -helpdir helpdir Directory . {
      if {[file pathtype $itk_option(-helpdir)] == "relative"} {
         configure -helpdir [file join [pwd] $itk_option(-helpdir)]
      } else {
         configure -topics $itk_option(-topics)
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Flag - in process of rendering
   protected variable rendering_ 0

   #  Search result file
   protected variable tmpfile_ {}

   #  The File menu.
   protected variable filemenu_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  End of class definition.
}
