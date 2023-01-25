#+
#  Name:
#     TabbedGaia

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Provide a tabbed view/model of GAIA clones.

#  Description:
#     Saves on having many windows open at once and provides the
#     ability to quickly switch between them. Also provides the
#     ability to load all the images in an HDS container file or FITS
#     MEF, as well as controlling the child windows of any GAIA instance.

#  Invocations:
#
#        TabbedGaia object_name [configuration options]
#
#     This creates an instance of a TabbedGaia object. The return is
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

#  Copyright:
#     Copyright (C) 2003-2005 Central Laboratory of the Research Councils.
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
#     14-JUL-2003 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual TabbedGaia {}

itcl::class gaia::TabbedGaia {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  And stop geometry restoration from working.
      configure -center 0

      #  Evaluate any options.
      eval itk_initialize $args

      #  Arrange same X defaults as main application.
      setXdefaults

      #  Create the UI. This consists of a toplevel window, plus menu
      #  for basic control and a tabbedpane for viewing the GAIA
      #  instances.

      #  Set the top-level window title.
      wm title $w_ "GAIA: main tabbed view"

      #  Closing the main window exits the application.
      wm protocol $w_ WM_DELETE_WINDOW [code $this quit]

      #  Add a File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add window help.
      add_help_button tabbedusage "On Window..."

      #  Add option to create a new instance of GAIA in a tab.
      $File add command -label {New tab} \
         -command [code $this new_tab] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this new_tab]
      $short_help_win_ add_menu_short_help $File \
         {New tab} {Create a new tab pane with a GAIA instance}

      #  Add option to load a MEF type file into the tabs, replacing
      #  any images already loaded.
      $File add command -label {Load file...} \
         -command [code $this find_file 0] \
         -accelerator {Control-l}
      bind $w_ <Control-l> [code $this find_file 0]
      $short_help_win_ add_menu_short_help $File \
         {Load file...} \
         {Open a file and load images into tabs replacing any existing images}

      #  Add option to load a MEF type file into several tabs appended
      #  to those that already exist.
      $File add command -label {Append file...} \
         -command [code $this find_file 1] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this find_file 1]
      $short_help_win_ add_menu_short_help $File \
         {Load file...} \
         {Open a file and load images into new tabs}

      #  Exit the application.
      $File add command -label {Exit} \
         -command [code $this quit] \
         -accelerator {Control-q}
      bind $w_ <Control-q> [code $this quit]
      $short_help_win_ add_menu_short_help $File \
         {Exit} {Exit the application}

      #  Options menu.
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Toggle control of child windows.
      $Options add checkbutton \
         -label {Control popup windows} \
         -variable [scope itk_option(-control_popups)] \
         -onvalue 1 \
         -offvalue 0

      #  Create the tabbed pane.
      itk_component add tabbedpane {
         iwidgets::tabnotebook $w_.tab -angle 0 -tabpos w
      }
      pack $itk_component(tabbedpane) -side top -fill both -expand 1

      #  Add the first tab.
      new_tab
   }

   #  Destructor:
   #  -----------
   destructor  {
      save_toplevel_geometry_
   }

   #  Methods:
   #  --------

   #  Quit the application.
   public method quit {} {
      #  XXX need -quiet_exit option.
      delete object $this
      after idle exit
   }

   #  Called when constructor is completed.
   public method init {} {
      load_toplevel_geometry_
   }

   #  Create an instance of GAIA and put it in a new tab. Returns the
   #  name of the clone.
   public method new_tab {} {
      set pane [$itk_component(tabbedpane) add \
                   -command [code $this switched_pane_]]

      #  Create another frame so that we can pass on geometry requests
      #  (need to apply -container 1 at creation).
      itk_component add container$containercount_ {
         frame $pane.container -container 1
      }
      pack $itk_component(container$containercount_) -fill both -expand 1

      #  Get the frame id, this is needed to host the GAIA
      #  instance. The gaiauseid variable is picked by Toplevel.itk.
      global ::gaiauseid
      set gaiauseid [winfo id $itk_component(container$containercount_)]
      set index $tabcount_
      incr containercount_
      incr tabcount_

      #  Need to stop TopLevelWidget blocking...
      rename ::tkwait ::real_tkwait
      rename ::tabbedgaia_tkwait ::tkwait

      if { ! $started_ } {

         #  Cannot use a clone first time, must create a GAIA "application".
         set started_ 1
         set main_windows_(.keeper) 1; # Stop application exiting...
                                       # by TopLevelWidget mechanisms.
         gaia::Gaia::startGaia
         set names_($tabcount_) ".gaia1"

      } else {

         #  It's hard to get syncronisation right here. If we view the
         #  tab too quickly GAIA seems to be fixed at geometry 1x1 and
         #  doesn't map correctly. Cannot use Gaia::noblock_clone as
         #  this waits for a visibility event which isn't seen if we
         #  don't do "show_tab_", which in turn is too early for
         #  GAIA... Do this the hard way by stopping the block used by
         #  the normal Gaia::clone method, quite like Gaia::startGaia.
         set names_($tabcount_) [[get_first_instance_] clone]
      }

      #  Need to update so that GAIA instances are available?
      update

      #  Doesn't work without these before update either (block
      #  happens anyway?).
      rename ::tkwait ::tabbedgaia_tkwait
      rename ::real_tkwait ::tkwait

      #  Arrange for page to be revealed when all work is completed.
      after idle [code $this show_tab_ $index]
      $names_($tabcount_) configure \
         -on_close_cmd [code $this tab_closed_]

      #  Tab has GAIA clone label.
      $itk_component(tabbedpane) pageconfigure $index \
         -label [$names_($tabcount_) cget -number]

      #  Return GAIA instance.
      return $names_($tabcount_)
   }

   #  Get a tab by number and return the GAIA instance displayed.
   #  If index is greater than the current tabcount_ limit then new
   #  instances up to that limit are created.
   public method get_tab { index } {
      if { $index < $tabcount_ } {
         return $names_([expr $index+1])
      }
      for { set i [expr $tabcount_-1] } { $i < $index } { incr i } {
         set gaia [new_tab]
      }
      return $gaia
   }

   #  Show a tab from the notebook.
   protected method show_tab_ { i } {
      $itk_component(tabbedpane) select $i
   }

   #  Invoked when the current instance of GAIA is closed. Use current
   #  tab if index is -1.
   protected method tab_closed_ { {index -1} } {
      if { $index == -1 } {
         set index [$itk_component(tabbedpane) view]
      }
      if { $index == 0 && $tabcount_ == 1 } {
         #  All gone. The interface cannot work without at least one
         #  GAIA instance.
         quit
      }
      $itk_component(tabbedpane) delete $index

      #  All the remaining tabs are re-labelled so that we don't have
      #  any gaps.
      set j 0
      for { set i 0 } { $i < $tabcount_} { incr i } {
         if { $i != $index } {
            $itk_component(tabbedpane) pageconfigure $j -label \
               [$names_([expr $i+1]) cget -number]
            set names_([expr $j+1]) $names_([expr $i+1])
            incr j
         }
      }
      set tabcount_ $j
      unset names_([expr $tabcount_+1])

      if { $index < $tabcount_ } {
         show_tab_ $index
      } else {
         show_tab_ [expr max(0,$tabcount_-1)]
      }

      #  This must be the last tab visited and is now gone..
      set lasttab_ -1
   }

   #  Arrange for Xdefaults to work like the GAIA application.
   public method setXdefaults {} {
      util::setXdefaults
      rtd::setXdefaults
      cat::setXdefaults
      skycat::setXdefaults
      gaia::setXdefaults
   }

   #  Pane has switched.
   protected method switched_pane_ {} {
      set index [$itk_component(tabbedpane) view]

      #  Withdraw toplevel windows that are children of the window we
      #  have just deselected. This should be "lasttab_"
      if { $itk_option(-control_popups) &&
           $lasttab_ != -1 &&
           $lasttab_ < $tabcount_ } {

         set lastgaia $names_([expr $lasttab_+1])
         if { $lastgaia != {} && [winfo exists $lastgaia] } {
            hide_children $lastgaia
         }

      }
      #  Always restore any toplevels associated with this tab.
      if { [info exists names_([expr $index+1])] } {
         set thisgaia $names_([expr $index+1])
         if { $thisgaia != {} && [winfo exists $thisgaia] } {
            reveal_children $thisgaia $itk_option(-control_popups)
         }
      }
      set lasttab_ $index
   }

   #  Withdraw any children of a GAIA instance.
   public method hide_children {thisgaia} {
      set children [$thisgaia list_windows $thisgaia]
      set child_windows_($thisgaia) {}
      foreach w $children {
         if { [winfo ismapped $w] } {
            lappend child_windows_($thisgaia) $w
            catch {wm withdraw $w}
         }
      }
   }

   #  Deiconify any children of a GAIA instance. If raise is true then
   #  windows that are not withdrawn are also raised.
   public method reveal_children {thisgaia raise} {
      if { [info exists child_windows_($thisgaia)] } {
         foreach w $child_windows_($thisgaia) {
            if {[winfo exists $w]} {
               if { $raise || ! [winfo ismapped $w] } {
                  catch {wm deiconify $w}
               }
            }
         }
      }
   }

   #  Save the position of the top level window so we can reload it
   #  the next time.
   protected method save_toplevel_geometry_ {} {
      set s [winfo geometry $w_]

      #  Check for case where window was not initialized...
      if { "$s" != "1x1+0+0" } {
         if { [catch { set fd [::open $toplevel_geometry_ w] }] } {
            return
         }
         puts $fd $s
         ::close $fd
      }
   }

   #  Restore the position of the top level window from the previous session.
   #  Apply a default if not found.
   protected method load_toplevel_geometry_ {} {
      if { [catch { set fd [::open $toplevel_geometry_ r] } ] } {
         wm geometry $w_ 850x800+0+0
         return
      }
      catch {
         set geom [gets $fd]
         wm geometry $w_ $geom
      }
      ::close $fd
   }

   #  Use a file dialog to obtain a name and then load it.
   public method find_file {append {dir "."} {pattern "*.*"}} {

      #  First time ask GAIA what file types are supported.
      if { $itk_option(-file_types) == {} } {
         configure -file_types [[get_first_instance_] cget -file_types]
      }
      set file [get_file_ $dir $pattern $itk_option(-file_types)]
      if {"$file" != ""} {
         load_file $append $file
      }
   }

   #  Get filename using fileselection dialog. This is created once and
   #  retains the current name and filters when repeatably accessed.
   protected method get_file_ {dir pattern types} {
      if { ! [winfo exists $fileselect_] } {
         set fileselect_ [util::FileSelect $w_.select -dir $dir -filter $pattern \
                             -transient 1 -withdraw 1 -filter_types $types]
         wm transient $fileselect_ [winfo toplevel $w_]
      } else {

         #  Now a transient of this window, not one that created it.
         wm transient $fileselect_ [winfo toplevel $w_]

         #  Also deiconfy and raise in case previous parent is iconised.
         wm deiconify $fileselect_
         raise $fileselect_
      }
      if {[$fileselect_ activate]} {
         return [$fileselect_ get]
      }
   }

   #  Open a new file and load its contents into a series of
   #  tabbedgaia instances. If append is false then these replace any
   #  existing images
   public method load_file {append filename} {

      if { $append } {
         set index $tabcount_
      } else {
         set index 0
      }

      #  Create a new tab and get it to load the file. We then ask
      #  it to list all the other images that may need displaying.
      set basegaia [get_tab $index]
      incr index
      $basegaia open $filename
      update

      set image [[$basegaia get_image] get_image]
      set isfits [$image isfits]
      set hdu_list [$image hdu list]
      set headings [$image hdu listheadings]

      set n 0
      foreach hdu $hdu_list {
         if { $n > 0 } {
            eval lassign [list $hdu] hdu type
            if { $type == "image" || ! $isfits } {
               set gaia [get_tab $index]
               incr index
               $gaia open $filename
               update
               set image [[$gaia get_image] get_image]
               if { $isfits } {
                  $image hdu $hdu
               } else {
                  $image hdu $hdu "data"
               }
            } else {
               #  Tables do not count.
               incr n -1
            }
         }
         incr n
      }

      # When appending we can have extra tabs left. These should be
      # removed.
      if { ! $append } {
         incr n
         if { $n <= $tabcount_ } {
            set limit $tabcount_
            for { set i $limit } { $i >= $n } { incr i -1 } {
               destroy object $names_($i)
               tab_closed_ [expr $i-1]
            }
         }
      }
   }

   #  Locate the first available instance of GAIA.
   protected method get_first_instance_ {} {
      for { set i 0 } { $i < $tabcount_ } {incr i } {
         set name $names_([expr $i+1])
         if { [winfo exists $name] } {
            return $name
         }
      }
      error_dialog "Failed to locate an instance of GAIA!"
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The possible file types for loading.
   itk_option define -file_types file_types File_Types {}

   # Whether to control popup windows.
   itk_option define -control_popups control_popups Control_Popups 0

   #  A font used for labels.
   itk_option define -labelfont labelfont LabelFont TkDefaultFont {
      set ::gaia_fonts(labelfont) $itk_option(-labelfont)
   }

   #  A font used for fixed width text.
   itk_option define -textfont textfont TextFont TkFixedFont {
      set ::gaia_fonts(textfont) $itk_option(-textfont)
   }

   #  A font used for labels that require symbols (alpha & delta).
   itk_option define -wcsfont wcsfont WcsFont TkDefaultFont {
      set ::gaia_fonts(wcsfont) $itk_option(-wcsfont)
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Number of tabs current open.
   protected variable tabcount_ 0

   #  Whether the first instance of GAIA has been created.
   protected variable started_ 0

   #  The last tab selected (-1 for not).
   protected variable lasttab_ -1

   #  List of windows associated with a GAIA instance.
   protected variable child_windows_

   #  The names of the GAIA instances.
   protected variable names_

   #  Overall number of tabs created (unique).
   protected variable containercount_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of the file used to save the position of the main window.
   global ::env
   protected common toplevel_geometry_ $env(HOME)/.skycat/tabbedgeometry

   #  Name of fileselection dialog window. Shared between all instances.
   common fileselect_ .tabbedgaiafs

#  End of class definition.
}

#  Dummy do nothing procedure for replace tkwait temporarily.
proc tabbedgaia_tkwait {args} {
}
