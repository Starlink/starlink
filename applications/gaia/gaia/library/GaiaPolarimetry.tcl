#+
#  Name:
#     GaiaPolarimetry

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for manipulating and displaying a catalogue
#     of vectors produced by POLPACK representing a polarization map.

#  Description:
#     This class creates a top-levelwidget for ...

#  Invocations:
#
#        GaiaPolarimetry object_name [configuration options]
#
#     This creates an instance of a GaiaPolarimetry object. The
#     return is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See the "itk_option define" declarations below.

#  Methods:
#     See the method declarations below.

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils
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
#     DSB: David S. Berry  (STARLINK)
#     {enter_new_authors_here}

#  History:
#     15-JUN-2000 (DSB):
#        Original version.
#     15-JAN-2016 (DSB):
#        If a bad selection expression is entered, leave the bad expression
#        visible so that it can be corrected. Previously, the bad expression
#        was cleared, meaning the user had to start entering it again from
#        scratch, with potential new mistakes.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolarimetry {}

itcl::class gaia::GaiaPolarimetry {

#  Inheritances:
#  -------------
   inherit util::TopLevelWidget

#  Constructor:
#  ------------
   constructor {args} {
      global ::env

#  Add the name of this toolbox to the list of created toolboxes.
      lappend these_ $this

#  Evaluate any options.
      eval itk_initialize $args

#  Set the top-level window title.
      wm title $w_ "GAIA: Polarimetry "

#  Ensure that resources are released if the window manager closes the
#  window.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close_win]

#  Set up the name of the directory in which the values used for polarimetry
#  toolbox options will be stored.
      set_optdir

#  Create a GaiaPolList to hold the list of undo-able actions. Set its
#  maximum length to 20
      set actionlist_ [::gaia::GaiaPolList actlist#auto]
      $actionlist_ setMax 20

#  Indicate that there is currently nothing to undo or redo.
      set canundo_ 0
      set canredo_ 0

#  Set defaults for options
      set values_($this,usetab) 1
      set values_($this,saveopt) 1
      set values_($this,page) 0

#  Create GaiaPolLists to hold the displayed catalogues, and the
#  styles used to display each catalogue. Set their maximum lengths to 20.
      set catlist_ [::gaia::GaiaPolList catlist#auto]
      set stylelist_ [::gaia::GaiaPolList stylist#auto]
      $catlist_ setMax 20
      $stylelist_ setMax 20

#  No catalogue to display as yet.
      set newcat_ ""

#  Add short help window.
      make_short_help

#  Add the menu bar.
      add_menubar

#  Add the File menu to the menu bar.
      set File_ [add_menubutton "File" "File menu..."]
      configure_menubutton File -underline 0

#  Add option to create a new window.
      $File_ add command -label {New window} \
         -command [code $this clone_me] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me]
      $short_help_win_ add_menu_short_help $File_ \
         {New window} {Create a new polarimetry toolbox}

#  Add the File menu item to open a new catalogue.
      $File_ add command -label {Open} \
         -command [code $this open_cat] \
         -accelerator "Control-o"
      bind $w_ <Control-o> [code $this open_cat]
      $short_help_win_ add_menu_short_help $File_ \
         {Open} {Open a new polarimetry catalogue}

#  Add the File menu item to save the current catalogue.
      $File_ add command -label {Save as} \
         -command [code $this save_cat] \
         -accelerator "Control-s"
      bind $w_ <Control-s> [code $this save_cat]
      $short_help_win_ add_menu_short_help $File_ \
         {Save as} {Save the current catalogue to a new file}

#  Add the File menu "Close window" item.
      $File_ add command -label "Close window" \
         -command [code $this close_win] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close_win]
      $short_help_win_ add_menu_short_help $File_ \
         {Close window} {Close this window}

#  Add a separator to the File menu.
      $File_ add separator

#  Add the Edit menu to the menu bar.
      set Edit_ [add_menubutton "Edit" "Edit menu..."]
      configure_menubutton File -underline 0

#  Add the Edit menu item to undo the last operation.
      $Edit_ add command -label {Un-do} \
         -command [code $this undo] \
         -accelerator {Control-u}
      bind $w_ <Control-u> [code $this undo]
      $short_help_win_ add_menu_short_help $Edit_ \
         {Un-do} {Un-do the effect of the previous operation}

#  Add the Edit menu item to redo the last undone operation.
      $Edit_ add command -label {Re-do} \
         -command [code $this redo] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this redo]
      $short_help_win_ add_menu_short_help $Edit_ \
         {Re-do} {Re-do the previous undone operation}

#  Add the Edit menu item to cut the currently selected vectors.
      $Edit_ add command -label {Cut} \
         -command [code $this cut] \
         -accelerator {Control-x} \
         -state disabled
      bind $w_ <Control-x> [code $this cut]
      $short_help_win_ add_menu_short_help $Edit_ \
         {Cut} {Remove the currently selected vectors}

#  Add the Options menu to the menu bar.
      set Options [add_menubutton "Options" "Options menu..."]
      configure_menubutton Options -underline 0

#  Add a menu item to the Options menu to empty the list of recently accessed
#  catalogues shown int he File menu.
      $Options add command -label {Forget Recent} \
                           -command "::gaia::GaiaPolarimetry::resetAllRecent"
      $short_help_win_ add_menu_short_help $Options \
         {Forget Recent} {Reset the list of recently accessed catalogues in the File menu}

#  Add a menu item to the Options menu to restore start-up values for all
#  notebook pages.
      $Options add command -label {Restore} \
                           -command [code $this setControls 1]
      $short_help_win_ add_menu_short_help $Options \
         {Restore} {Restore the current start-up settings for all controls}

#  Add a menu item to the Options menu to reset default values for all
#  notebook pages.
      $Options add command -label {Reset} \
                           -command [code $this setControls 0]
      $short_help_win_ add_menu_short_help $Options \
         {Reset} {Reset built-in default settings for all controls}

#  Add a menu item to the Options menu to allow/prevent the saving of
#  current control settings when the toolbox is deleted.
      $Options add checkbutton -label {Save Settings} \
                           -variable [scope values_($this,saveopt)] \
                           -onvalue 1 \
                           -offvalue 0 \
                           -command [code $this saveOpt]
      $short_help_win_ add_menu_short_help $Options \
         {Save Settings} {Save any changes made to control settings upon exit?}

#  Add a menu item to the Options menu to allow/prevent the display of data
#  in the table.
      $Options add checkbutton -label {Enable Table} \
                           -variable [scope values_($this,usetab)] \
                           -onvalue 1 \
                           -offvalue 0 \
                           -command [code $this useTab]
      $short_help_win_ add_menu_short_help $Options \
         {Enable Table} {Data will be displayed in the table only if this button is checked}

#  Add the Table menu to the menu bar.
      set Table [add_menubutton "Table" "Table control menu..."]
      configure_menubutton Table -underline 0 -state disabled

#  Add window help.
      add_help_button polarusage "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get help on the use of this window}

#  Create a progress bar.
      set pbar_ $w_.progress
      itk_component add progress {ProgressBar $pbar_}

#  Create the tab notebook for containing each page of options.
      itk_component add notebook {
         ::iwidgets::tabnotebook $w_.notebook -tabpos w -background \
                                 yellow -width 800 -height 400
      }
      pack $itk_component(notebook) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m

#  Add a page for the rendering options. A GaiaPolUStyle object is a
#  FrameWidget, but is also a GaiaPolStyle. The FrameWidget contains
#  controls for setting and getting style parameters in the GaiaPolStyle.
      itk_component add newsty {
          ::gaia::GaiaPolUStyle $w_.newsty -changecmd "[code $this redraw 1 0]" \
                                -optdir $optdir_
      }
      $itk_component(notebook) add -label Rendering \
                               -command [code $this selectPage newsty]
      pack $itk_component(newsty) -in [$itk_component(notebook) childsite 0] \
                                  -anchor nw -expand 1 -fill x

#  Add a page to control the highlighting of vectors under the pointer.
      itk_component add highlight {
          ::gaia::GaiaPolUHigh $w_.highlight -changecmd "[code $this newHigh]" \
                                             -optdir $optdir_ \
                                             -actioncmd "[code $this addAction]"
      }
      $itk_component(notebook) add -label Highlighting \
                               -command [code $this selectPage highlight]
      pack $itk_component(highlight) -in [$itk_component(notebook) childsite 1] \
                                     -anchor nw -expand 1 -fill x

#  Add a page to control the appearance of the key.
      itk_component add key {
          ::gaia::GaiaPolUKey $w_.key -changecmd "[code $this newKey]" \
                                      -optdir $optdir_ \
                                      -actioncmd "[code $this addAction]"
      }
      $itk_component(notebook) add -label Key \
                               -command [code $this selectPage key]
      pack $itk_component(key) -in [$itk_component(notebook) childsite 2] \
                               -anchor nw  -expand 1 -fill x


#  Add a page for the selection options.
      itk_component add sel {
          ::gaia::GaiaPolUSelOpt $w_.sel -changecmd "[code $this newSelOpt]" \
                                         -optdir $optdir_ \
                                         -actioncmd "[code $this addAction]"
      }
      $itk_component(notebook) add -label Selecting \
                                   -command [code $this selectPage sel]
      pack $itk_component(sel) -in [$itk_component(notebook) childsite 3] \
                               -anchor nw -expand 1 -fill x

#  Add a page to calculate and display statistics.
      itk_component add stats {
          ::gaia::GaiaPolUStats $w_.stats -optdir $optdir_ \
                                          -pbar [code $pbar_] \
                                          -actioncmd "[code $this addAction]"
      }
      $itk_component(notebook) add -label "Statistics" \
                                   -command [code $this selectPage stats]
      pack $itk_component(stats) -in [$itk_component(notebook) childsite 4] \
                                 -anchor nw -expand 1 -fill x

#  Add a page to allow selection of I, Q, U, V (etc) columns.
      itk_component add cols {
          ::gaia::GaiaPolUCols $w_.cols -optdir $optdir_ \
                                        -actioncmd "[code $this addAction]" \
                                        -changecmd "[code $this newCols]"
      }
      $itk_component(notebook) add -label "Column Names" \
                                   -command [code $this selectPage cols]
      pack $itk_component(cols) -in [$itk_component(notebook) childsite 5] \
                                -anchor nw -expand 1 -fill x

#  Add a page to allow binning of I, Q, U, V (etc).
      itk_component add bin {
          ::gaia::GaiaPolUBin $w_.bin -changecmd "[code $this newBin]" \
                                      -optdir $optdir_ \
                                      -actioncmd "[code $this addAction]"
      }
      $itk_component(notebook) add -label "Binning" \
                                   -command [code $this selectPage bin]
      pack $itk_component(bin) -in [$itk_component(notebook) childsite 6] \
                                -anchor nw -expand 1 -fill x

#  Add a page to allow display of aperture integrated I, Q, U, V (etc) values.
      itk_component add integ {
          ::gaia::GaiaPolUInteg $w_.integ -optdir $optdir_ \
                                        -pbar [code $pbar_] \
                                        -actioncmd "[code $this addAction]" \
                                        -changecmd "[code $this newInteg]"
      }
      $itk_component(notebook) add -label "Integrate" \
                                   -command [code $this selectPage integ]
      pack $itk_component(integ) -in [$itk_component(notebook) childsite 7] \
                                -anchor nw -expand 1 -fill x

#  Add a page to control display of spectral information.
      itk_component add spec {
          ::gaia::GaiaPolUSpec $w_.spec -optdir $optdir_ \
                                        -pbar [code $pbar_] \
                                        -changecmd "[code $this newSpec]"
      }
      $itk_component(notebook) add -label "SpecPol" \
                                   -command [code $this selectPage spec]
      pack $itk_component(spec) -in [$itk_component(notebook) childsite 8] \
                                 -anchor nw -expand 1 -fill x

#  Create a TableList to display the numerical catalogue contents.
      set tablelist_ $w_.cattab
      itk_component add cattab {
         ::util::TableList $tablelist_ -hscroll 1 -height 8 \
                                    -selectmode extended \
                                    -exportselection 0
      }
      add_short_help $tablelist_ \
            {Catalogue values: {bitmap b1} = select row, \
                               {bitmap dragb2} = scroll list}

#  Add the Table menu item to print the table.
      $Table add command -label "Print..." \
                -command [code $tablelist_ print_dialog]
      $short_help_win_ add_menu_short_help $Table {Print...} \
                  {Print selected columns from the table}

#  Add the Table menu item to sort the table.
      $Table add command -label "Sort..." \
                -command [code $tablelist_ sort_dialog]
      $short_help_win_ add_menu_short_help $Table {Sort...} \
                  {Sort the table}

#  Add the Table menu item to show or hide the table.
      $Table add command -label "Show/Hide..." \
                -command [code $tablelist_ layout_dialog]
      $short_help_win_ add_menu_short_help $Table {Show/Hide...} \
                  {Select the columns which are to be visible}

#  Create the button bar
      itk_component add buttonbar {frame $w_.action}

#  Add a button to close window.
      itk_component add close {
         button $itk_component(buttonbar).close -text "Close" \
            -command [code $this close_win]
      }
      add_short_help $itk_component(close) {Close this window}

#  Add a button to redisplay the catalogue.
      itk_component add redraw {
         button $itk_component(buttonbar).redraw -text "Redraw" \
            -command [code $this redraw 0 0]
      }
      add_short_help $itk_component(redraw) {Re-draw the vectors if required (double click to force a re-draw)}
      bind $itk_component(redraw) <Double-ButtonPress-1> [code $this redraw 0 1]

#  Add a button to zoom the display to the selected vectors.
      itk_component add zoom {
         button $itk_component(buttonbar).zoom -text "Zoom" \
            -command [code $this zoom 0]
      }
      bind $itk_component(zoom) <Double-ButtonPress-1> [code $this zoom 1]
      add_short_help $itk_component(zoom) {Zoom the image to the selected vectors (double click to view all vectors)}

#  Pack all the components into place.
      pack $itk_component(progress) -side bottom -fill both
      pack $itk_component(buttonbar) -side bottom -fill both -pady 3 -padx 3
      pack $itk_component(notebook) -side top -fill both -expand 1 -pady 3 -padx 3
      pack $itk_component(cattab) -side bottom -fill both -expand 1
      pack $itk_component(close) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(redraw) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(zoom) -side right -expand 1 -pady 1 -padx 1

#  Create a GaiaPolDisp object to handle the display of vectors on the canvas.
      set display_ [::gaia::GaiaPolDisp disp#auto $w_ \
                               $itk_option(-image) \
                               $itk_option(-rtdimage) \
                               $itk_option(-canvas) \
                               [code $pbar_] \
                               "$this newsel" \
			       [$itk_component(highlight) getFont] \
			       [$itk_component(key) getLFont] \
                               "$this dispAction" ]

#  Copy defaults from other control panels to the display.
      $display_ setKEnabled [$itk_component(key) getEnabled]
      $display_ setKlCol [$itk_component(key) getLColour]
      $display_ setKlFmt [$itk_component(key) getLFormat]
      $display_ setKvCol [$itk_component(key) getVColour]
      $display_ setKvWid [$itk_component(key) getVWidth]
      $display_ setKbgCol [$itk_component(key) getBgColour]
      $display_ setKbdCol [$itk_component(key) getBdColour]
      $display_ setKbdWid [$itk_component(key) getBdWidth]
      $display_ setKPad [$itk_component(key) getBgPad]
      $display_ setKvVal [$itk_component(key) getVValue]

      $display_ setHgEnabled [$itk_component(highlight) getEnabled]
      $display_ setHgColour [$itk_component(highlight) getColour]
      $display_ setHgFormat [$itk_component(highlight) getFormat]

      $display_ setSelShape [$itk_component(sel) getShape]

#  Create a GaiaPolTable object to handle the display of vectors in the table.
      set table_ [::gaia::GaiaPolTable tab#auto $w_ \
                               $itk_option(-rtdimage) \
                               [code $tablelist_] \
                               [code $pbar_] \
                               "$this newsel" ]

#  Initalize the state and shorthelp for the undo and redo entries
#  in the Edit menu.
      setUndoRedo

#  Add binding for the table.
      bind $tablelist_.listbox <ButtonRelease-1> +[code $table_ tabSel]

#  Override these values with values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$optdir_/GaiaPolarimetry.opt"
      if { [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
            if { $recent_ == "" } {
               if { [info exists values_($this,recent)] } {
                  set recent_ $values_($this,recent)
                  unset values_($this,recent)
               }
            }
         }
      }

#  Add items to the File menu for recently accessed files.
      set recentFile_ ""
      ::gaia::GaiaPolarimetry::updateAllRecent

#  Select the required options page in the notebook.
      $itk_component(notebook) select $values_($this,page)

#  Enable or disable the table
      useTab

#  Check we have a usable version of polpack.
      checkPolpack 3.0

   }

#  Destructor:
#  -----------
   destructor  {

#  Remove $this from the list of polarimetry toolboxes.
      set i [lsearch -exact $these_ $this]
      if { $i != -1 } { set these_ [lreplace $these_ $i $i] }

#  Close the catalogue.
      catch {close_cat}

#  Delete all currently active PolObjects created by this toolbox.
      ::gaia::GaiaPolObject::annullAll $this

#  If no polarimetry toolboxes are currently in existence, ensure the
#  directory used to store temporary objects is deleted.
      if { $these_ == "" } {
         ::gaia::GaiaPolObject::rmDir
      }
   }

#  Public Methods:
#  ===============

#  Called to update the list of recently accessed files in the File menu
#  of this toolbox.
#  ----------------------------------------------------------------------
   public method updateRecent {} {

#  Remove any commands for recently accessed files from the File menu.
      if { $recentFile_ != "" } {
         $File_ delete $recentFile_ end
      }

#  If there are any recently acessed files to add...
      if { [llength $recent_] > 0 } {

#  Store the index within the File menu for the first one.
         set recentFile_ [expr [$File_ index end] + 1]

#  Add an item to the File menu for each recently accessed file.
         foreach desc $recent_ {
            set label [lindex $desc 0]
            set file [lindex $desc 1]

            $File_ add command -label "  $label" \
               -command [code $this opener $file]
            $short_help_win_ add_menu_short_help $File_ \
               "  $label" "Open $file"

         }
      }
   }

#  Called to cut the selected vectors out of the catalogue.
#  --------------------------------------------------------
   public method cut {} {

#  If no catalogue is displayed, warn the user.
      if { $newcat_ == "" } {
         error_dialog "No catalogue is currently displayed"

#  If there are no selected vectors, warn the user.
      } elseif { [$newcat_ noSel] } {
         error_dialog "No vectors are currently selected"

#  Otherwise...
      } else {

#  Mark all selected vectors as deleted.
         $newcat_ cut

#  Make sure the display reflects the new catalogue.
         redraw 1 0
      }
   }

#  Called when the File menu Open item is selected.
#  -----------------------------------------------
   public method opener {file} {
      set ret 0

#  Indicate what is happening.
      setHold "Opening $file"

#  Check a file name was supplied.
      if { [string trim "$file"] == "" } {

#  Check that the file exists.
      } elseif { ![file isfile $file] } {
         error_dialog "There is no file named '$file'"

#  Otherwise...
      } else {

#  Close any currently open Catalogue. This saves any changes to disk,
#  resets the Undo/Redo action list, the style list and catalogue
#  list, and clears the display.
         if { [close_cat] } {

#  Create a new Catalogue object describing the catalogue, and its
#  selection history.
            set newcat_ [code [::gaia::GaiaPolCat cat#auto $file $itk_option(-rtdimage) $w_ [code $pbar_] ] ]

#  If unsuccesfull, reset newcat_
            if { [catch {$newcat_ info class} ] } {
               set newcat_ ""

#  Otherwise...
            } else {

#  Indicate what is happening.
               setHold "Configuring control panels..."

#  Notify the control pages that a new catalogue has been opened.
               $itk_component(cols) newCat $newcat_
               $itk_component(spec) newCat $newcat_
               $itk_component(newsty) newCat $newcat_

#  Reset the key properties used by the display so that default values
#  will be used when the next key is created.
               $display_ resetKey

#  Display the catalogue.
               redraw 0 0

#  Indicate what is happening.
               setHold "Almost there..."

#  Push copies of the current catalogue and style on to the catalogue and
#  style lists.
               set prevsty_ [$itk_component(newsty) copy]
               $stylelist_ add $prevsty_

               set prevcat_ [$newcat_ copy]
               $catlist_ add $prevcat_

#  Reset the Undo and redo buttons.
               set canundo_ 0
               set canredo_ 0
               setUndoRedo

#  Enable the table menu.
               useTab

#  Set the top-level window title.
               wm title $w_ "GAIA: Polarimetry ($file)"
               set ret 1

#  Add it to the list of recently accessed files.
               ::gaia::GaiaPolarimetry::addAllRecent $file

            }
         }
      }

      resetHold
      return $ret
   }

#  Save the current options values to the options file, over-writing any
#  existing options file. Include the list of recently accessed catalogues.
#  If required, unset the variables so that other toolboxes wont pick them
#  up.
#  ------------------------------------------------------------------------
   public method saveOpts {remove} {
      set values_($this,recent) $recent_
      if { [file isdirectory $optdir_] } {
         set optfile "$optdir_/GaiaPolarimetry.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox : $mess"
         } else {
            foreach name [array names values_] {
               if { [regexp {([^,]+),(.*)} $name match obj elem] } {
                  if { $obj == $this } {
                     puts $fd "set option($elem) \{$values_($name)\}"
                     if { $remove } { unset values_($name) }
                  }
               }
            }
            close $fd
         }
      }
      if { !$remove } { unset values_($this,recent) }
   }

#  Close this window, kill it if needed, otherwise clear the displayed
#  information and withdraw.
#  -------------------------------------------------------------------
   public method close_win {} {

#  Now close the window.
      if { [close_cat] } {
         if { $itk_option(-really_die) } {
            saveOpts 1
            delete object $this
         } else {
            saveOpts 0
            wm withdraw $w_
         }
      }
   }

#  Called when the currently displayed spectral channel is changed
#  in the SpecPol control panel. It tells the currently displayed
#  catalogue to hide all but the specified spectral channel. It then does
#  a redraw to display the new channel.
#  ---------------------------------------------------------------
   public method newSpec {} {
      set oldval [$newcat_ getZvals]
      set newval [$itk_component(spec) getZvals]
      if { $oldval != $newval } {
         $newcat_ setZvals $newval
         redraw 1 0
      }
   }

#  Executed when a GaiaApp "runwiths" completes.
#  ----------------------------------------
   public method completed {} {
      set done_ 1
   }

#  Executed when a GaiaApp "getparam" completes. Remove enclosing single
#  quotes which are added by startcl.
#  ---------------------------------------------------------------------
   public method gotparam {param val} {
      if { ![regexp {^ *'(.*)' *$} $val match parval_] } {
         set parval_ $val
      }
      set done_ 1
   }

#  Called when the canvas display is updated. $type is either "cat" or
#  "style", $obj is the name of an object of the specified type
#  (GaiaPolCat or GaiaPolStyle), and $desc is a description of the action
#  which resulted in the display being updated.
#  ----------------------------------------------------------------------
   public method dispAction { type obj desc } {

#  Add an action to the action list.
      addAction $type $desc

#  If the style has changed, save a copy of the new style on the style
#  list.
      if { $type == "style" } {
         set sty [$obj copy]
         $stylelist_ add $sty
         $sty annull

#  If the catalogue has changed, save a copy of the new catalogue on the
#  catalogue list.
      } else {
         set cat [$obj copy]
         $catlist_ add $cat
         $cat annull
      }
   }

#  Called when the current catalogue is to be binned.
#  -------------------------------------------------
   public method newBin {} {

#  If no catalogue is displayed, warn the user.
      if { $newcat_ == "" } {
         error_dialog "No catalogue is currently displayed"

#  Otherwise...
      } else {

#  If there are any unknown columns in the catalogue, ask the user to
#  check the column names.
         if { [$itk_component(cols) colsOK] } {

#  Get the binning parameters from the binning control panel.
            set box [$itk_component(bin) getBox]
            set method [$itk_component(bin) getMethod]
            set debias [$itk_component(bin) getDebias]
            set minval [$itk_component(bin) getMinVal]
            set sigmas [$itk_component(bin) getSigmas]

#  Create a new PolCat which is a binned copy of the currently displayed
#  PolCat.
            set cat [$newcat_ bin $box $method $debias $minval $sigmas]

#  If succsful, replace the original with the new, and display the new
#  catalogue.
            if { $cat != "" } {
               $newcat_ annull
               set newcat_ $cat
               redraw 1 0
            }
         }
      }
   }

#  Called when a selection of vectors is made. $reset indicates if the
#  current selection is to be reset (to either all clear or all set) before
#  selecting (or deselecting) the given rows. $origin is set to:
#     0 if the choice was made using the canvas
#     1 if the choice was made by using table.
#     2 if the choice was made by using the "Select" control panel.
#
#  $type indicates how the selection is specified:
#     "rows" - an explicit list of row indices have been chosen (indices
#              in $data)
#     "circle" - choose all vectors within a circle (bounding box in $data)
#     "box"  - choose all vectors within a rectangle (bounding box in $data)
#     "expr" - choose all vectors satisfying an algebraic expression (in
#              $data).
#     "all" - choose all vectors.
#     "invert" - invert the selection

#  -------------------------------------------------------------------
   public method newsel { reset origin type data } {
      set ret 1

#  Return if no catalogue is displayed.
      if { $newcat_ == "" } { return 0 }

#  If the choice was made using the canvas, return if selection using the
#  canvas is currently disabled.
      if { $origin == 0 } {
         if { [$itk_component(sel) getFreeze] } { return 0 }
      }

#  Indicate what is happening.
      setHold "Updating vector selection"

#  Unless, the vectors were chosen within the table, see if they are
#  to be selected, or deselected. If the choice was made within the table
#  the vectors are always selected.
      if { $origin != 1 } {
         set select_rows [$itk_component(sel) getSelect]
      } else {
         set select_rows 1
      }

#  If all vectors are to be chosen, do it.
      if { $type == "all" } {
         if { !$reset } {
            $newcat_ selectAll
         } else {
            $newcat_ deselectAll
         }

#  If the selection is to be inverted, do it.
      } elseif { $type == "invert" } {
         $newcat_ invert

#  Otherwise...
      } else {

#  If required, reset the selection first. $select_rows is true if the
#  chosen rows are to be selected, otherwise the chosen rows are
#  deselected.
         if { $reset } {
            if { $select_rows } {
               $newcat_ deselectAll
            } else {
               $newcat_ selectAll
            }
         }

#  Now modify the selection by selecting or deselecting the chosen rows.
         if { $select_rows } {
            set ret [$newcat_ select $type $data]
         } else {
            set ret [$newcat_ deselect $type $data]
         }
      }

#  Make sure the display reflects the new selection.
      if { $ret } {
         redraw 1 0
      }

      resetHold
      return $ret

   }

#  If readopt is 0, set all control settings back to their hard-wired
#  defaults. If readopt is 1, set all current settings back to the current
#  start-up values.
#  ---------------------------------------------------------------------
   public method setControls {readopt} {
      foreach cs [$itk_component(notebook) childsite] {
         foreach page [pack slaves $cs] {
            $page reset $readopt
         }
      }
   }

#  Called when the "Save options" item in the Options menu is changed.
#  It passes the current saveopt setting on to all the pages in the
#  notebook.
#  ------------------------------------------------------------------
   public method saveOpt {} {
      foreach cs [$itk_component(notebook) childsite] {
         foreach page [pack slaves $cs] {
            $page setSaveOpt $values_($this,saveopt)
         }
      }
   }

#  Called when the "Enable Tables" item in the Options menu is changed.
#  ------------------------------------------------------------------
   public method useTab {} {
      if { !$values_($this,usetab) } {
         if { $usetable_ } {
            set usetable_ 0
            $table_ clear
         }
         configure_menubutton Table -state disabled
      } else {
         if { !$usetable_ } {
            set usetable_ 1
            $table_ tabulate $newcat_ $prevcat_ 1
         }
         configure_menubutton Table -state normal
      }
   }

#  Select a page of the notebook.
#  ------------------------------
   public method selectPage {name} {
      $itk_component($name) create
      set values_($this,page) [$itk_component(notebook) index select]
      if { $name == "stats" } {
         $itk_component(stats) calc
      } elseif { $name == "integ" } {
         $itk_component(integ) calc
      }
   }

#  Called to update the list of recently accessed files in the File menus
#  of all polarimetry toolboxes.
#  ----------------------------------------------------------------------
   public proc updateAllRecent {} {
      foreach toolbox $these_ {
         $toolbox updateRecent
      }
   }

#  Ensure a file is on the recently accessed list in the File menus of
#  all polarimetry toolboxes.
#  -----------------------------------------------------------------
   public proc addAllRecent {file} {
      foreach toolbox $these_ {
         $toolbox addRecent $file
      }
   }

#  Reset the list of recently accessed catalogues in the File menus of
#  all polarimetry toolboxes.
#  -----------------------------------------------------------------
   public proc resetAllRecent {} {
      foreach toolbox $these_ {
         $toolbox resetRecent
      }
   }

#  Protected Methods:
#  =================

#  Indicate what is going on.
#  --------------------------
   protected method setHold {text} {
      blt::busy hold $w_ -cursor "watch"
      $pbar_ reset
      $pbar_ config -text $text
      update idletasks
   }

#  Clear the progress bar etc.
#  --------------------------
   protected method resetHold {} {
      blt::busy release $w_
      $pbar_ reset
      update idletasks
   }

#  Zoom the image so that the selected vectors fill the screen.
#  ------------------------------------------------------------
   protected method zoom { double } {
      if { $double } {
         inhibit_single 1
         $display_ zoom_to_image
         after 1000 "[code $this inhibit_single 0]"

      } else {
         after 250 {set a2 1}
         tkwait variable a2
         if { ![inhibit_single] } {
            $display_ zoom_to_selection
         }
      }
   }

#  Control the inhibiting of single clicks.
#  ----------------------------------------
   protected method inhibit_single { {state ""} } {
      if { $state == "" } {
         return $inhibit_single_
      } else {
         set inhibit_single_ $state
      }
   }

#  Change the highlighting settings in the GaiaPolDisp.
#  ----------------------------------------------------
   protected method newHigh {} {
      if { $display_ != "" } {
         $display_ setHgEnabled [$itk_component(highlight) getEnabled]
         $display_ setHgColour [$itk_component(highlight) getColour]
         $display_ setHgFormat [$itk_component(highlight) getFormat]
      }
   }

#  Called when a new integration is about to be performed using the
#  "Integrate" panel.
#  --------------------------------------------------------------------
   protected method newInteg {} {

#  If there are any unknown columns in the catalogue, ask the user to
#  check the column names.
      return [$itk_component(cols) colsOK]

   }

#  Called when new column names are chosen. Get the name of the changed
#  column and set its value in the GaiaPolCat.
#  --------------------------------------------------------------------
   protected method newCols {q} {
      if { $newcat_ != "" } {
         $newcat_ setColNam $q [$itk_component(cols) getCol $q]
         $itk_component(integ) newStats $newcat_
      }
   }

#  Called when new selction options are chosen. $reset indicates if the
#  current selection is to be reset (to either all clear or all set) before
#  selecting (or deselecting) the rows specified by the selection
#  expression. Item indicates which control was changed; "sexp" means a
#  new selection expression has been given.
#  -----------------------------------------------------------------------
   protected method newSelOpt {reset item} {

#  Ensure the cursor selection shape used by the GaiaPolDisp is the value
#  selected by the user.
      set shape [$itk_component(sel) getShape]
      $display_ setSelShape $shape

#  Ensure that the same selection shape is used by all other polarimetry
#  toolboxes.
      foreach toolbox $these_ {
         if { [ [$toolbox component sel] getShape] != $shape } {
            [$toolbox component sel] setShape $shape
         }
      }

#  If all vectors are to be selected.
      if { $item == "all" } {
         newsel $reset 2 "all" ""

#  If the current selection is to be inverted.
      } elseif { $item == "invert" } {
         newsel $reset 2 "invert" ""

#  If a new selection expression has been given.
      } elseif { $item == "sexp" } {

#  Get the current selection expression.
         set sexp [$itk_component(sel) getSexp]

#  Initialise a flag to indicate that the expression is not usable.
         set isgood 0

#  If not blank...
         if { $sexp != "" } {

#  If no catalogue is displayed, warn the user and reset the expression.
            if { $newcat_ == "" } {
               error_dialog "No catalogue is currently displayed"

#  Otherwise, find the corresponding vectors and select them. If
#  succesful, indicate the expression is usable.
            } else {
               if { [newsel $reset 2 "expr" $sexp] } {
                  set isgood 1
               }
            }
         }

#  Return the new expression to the GaiaPolUSelOpt GUI. This also adds the
#  expression to the list of recently used expressions (so long as the
#  expression is not already in the list and so long as it is good).
         $itk_component(sel) setSexp $sexp $isgood
      }
   }

#  Change the key settings in the GaiaPolDisp.
#  -------------------------------------------
   protected method newKey {} {
      if { $display_ != "" } {
         $display_ setKEnabled [$itk_component(key) getEnabled]
         $display_ setKlCol [$itk_component(key) getLColour]
         $display_ setKlFmt [$itk_component(key) getLFormat]
         $display_ setKvCol [$itk_component(key) getVColour]
         $display_ setKvWid [$itk_component(key) getVWidth]
         $display_ setKbgCol [$itk_component(key) getBgColour]
         $display_ setKbdCol [$itk_component(key) getBdColour]
         $display_ setKbdWid [$itk_component(key) getBdWidth]
         $display_ setKPad [$itk_component(key) getBgPad]

         set vval [$itk_component(key) getVValue]
         $display_ setKvVal $vval

#  Display the key.
	 $display_ Key

      }
   }

#  Display the current catalogue using the current style. $addact indicates
#  if a new action should be added to the action list as a result of the
#  update. $force indicates if the display should be updated even if it
#  seems that the update would produce no changes.
#  -------------------------------------------------------------------
   protected method redraw { addact force } {

#  Check we have a catalogue.
      if { $newcat_ == "" } {
         error_dialog "No catalogue is currently displayed"

#  If so, update the canvas and the table to display it.
      } else {
         if { $usetable_ } {
            $table_ tabulate $newcat_ $prevcat_ $force
         }
         $display_ draw $newcat_ $prevcat_ $itk_component(newsty) $prevsty_ $addact $force

#  If the new GaiaPolCat refers to a different catalogue from the
#  previous one, ensure that the necessary control panels are notified
#  about the change.
         if { $prevcat_ != "" } {
            if { [lindex [$prevcat_ changes $newcat_] 1] == "redraw" } {
               $itk_component(spec) newCat $newcat_
            }
         }

#  Save references for the catalogue and style just displayed. They will
#  have been added to the catalogue and style lists if they differ from the
#  current entries in the lists.
         if { $prevsty_ != "" } { $prevsty_ annull }
         set prevsty_ [$stylelist_ get]
         if { $prevcat_ != "" } { $prevcat_ annull }
         set prevcat_ [$catlist_ get]

#  Indicate that new values should be calculated by the statistics
#  and aperature integration pages in the notebook.
         $itk_component(stats) newStats $newcat_
         $itk_component(integ) newStats $newcat_

#  Ensure the current selection expression in the "Selecting" panel
#  corresponds to the expression stored with the catalogue (if any).
         $itk_component(sel) setSexp [$newcat_ getSexp] 1

#  Enable or disable the Cut item in the Edit menu depending on whether
#  there are currently any selected vectors.
         if { [$newcat_ noSel] } {
            $Edit_ entryconfigure "Cut" -state disabled
         } else {
            $Edit_ entryconfigure "Cut" -state normal
         }

#  Give the focus back to this window since it seems to go to the display
#  window after a display update.
         focus $w_
      }
   }

#  Update the display to undo or redo a "style" action.
#  ----------------------------------------------------
   protected method doStyle { undo } {

#  Decrement or increment the current entry in the stylelist. Return
#  without action if the no styles remain to be undone or redone.
      if { $undo } {
         if { ![$stylelist_ prev] } { return }
      } else {
         if { ![$stylelist_ next] } { return }
      }

#  Get a reference to the required new style.
      set sty [$stylelist_ get]

#  Update the $itk_component(newsty) controls to reflect the new style.
      $itk_component(newsty) setFrom $sty

#  Annull the style references
      $sty annull

#  Update the canvas and table without adding a new action.
      redraw 0 0

   }

#  Update the display to undo or redo a "cat" action.
#  ----------------------------------------------------
   protected method doCat { undo } {

#  Decrement or increment the current entry in the catlist. Return
#  without action if the no catalogues remain to be undone or redone.
      if { $undo } {
         if { ![$catlist_ prev] } { return }
      } else {
         if { ![$catlist_ next] } { return }
      }

#  Get a reference to the required new catalogue
      set cat [$catlist_ get]

#  Store a copy of the new catalogue as the current catalogue.
      $newcat_ annull
      set newcat_ [$cat copy]

#  Annull the catalogue reference
      $cat annull

#  Update the canvas and table without adding a new action.
      redraw 0 0
   }

#  Create a new instance of this object.
#  -----------------------------------
   protected method clone_me {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

#  Close a catalogue.
#  ------------------
   protected method close_cat {} {

#  Indicate what is happening.
      setHold "Closing current catalogue"

#  Check that any changes have been saved to disk.
      if { $newcat_ != "" } {
         if { [$newcat_ getChanged] } {
            if { ![confirm_dialog "Current catalogue not yet saved. Continue?"] } {
               resetHold
               return 0
            }
         }
         set newcat_ [$newcat_ annull]
      }

#  Store blank column headings in all the options pages which use column
#  names.
      $itk_component(cols) newCat ""
      $itk_component(stats) clear
      $itk_component(integ) clear

#  Empty the action, style and catalogue lists
      if { $prevcat_ != "" } { set prevcat_ [$prevcat_ annull] }
      if { $prevsty_ != "" } { set prevsty_ [$prevsty_ annull] }
      $actionlist_ reset
      $stylelist_ reset
      $catlist_ reset
      set canundo_ 0
      set canredo_ 0

#  Disable the Table menu.
      useTab

#  Clear the displayed data
      $display_ clear
      $table_ clear

#  Reset the vector scale so that a new default value appropriate to the
#  new catalogue will be found and used.
      catch {$itk_component(newsty) setMag ""}

#  Reset the spectral channel.
      catch {$itk_component(spec) setZvals "" }

#  Reset the column headings in the relevant options panels.
      catch {$itk_component(cols) newCat ""}
      catch {$itk_component(stats) setHeadings ""}
      catch {$itk_component(integ) setHeadings ""}

#  Set the top-level window title.
      wm title $w_ "GAIA: Polarimetry "

      resetHold
      return 1
   }

#  Get filename using fileselection dialog. This is created once and
#  retains the current name and filters when repeatably accessed.
#  -----------------------------------------------------------------
   protected method get_file {dir pattern types} {
       if { ! [winfo exists $fileselect_] } {
           set fileselect_ [FileSelect $w_.select -dir $dir -filter $pattern \
                             -transient 1 -withdraw 1 -filter_types $types]
           wm transient $fileselect_ [winfo toplevel $w_]
       } else {

#  It's a transient so shouldn't need to do this... but CDE
#  needs it.
          wm deiconify $fileselect_
          raise $fileselect_
       }
       if {[$fileselect_ activate]} {
           return [$fileselect_ get]
       }
   }

#  Open a new catalogue specified by the user
#  ------------------------------------------
   protected method open_cat {} {

#  Get the name of the file to open.
      set file [get_file [pwd] {*.FIT} $types_]

#  Attempt to open it.
      opener $file
   }

#  Reset the list of recently accessed catalogues in the File menu.
#  -----------------------------------------------------------------
   protected method resetRecent {} {
      set recent_ ""
      updateRecent
   }

#  Ensure a file is on the recently accessed list in the File menu.
#  -----------------------------------------------------------------
   protected method addRecent {file} {

#  Get the file basename. This is used as a short label for the file.
   set label [lindex [file split $file] end]

#  See if the supplied file is already on the recently accessed list.
#  If so, use its label instead of the basename found above, and remove
#  the existing entry for this file.
   set i -1
   foreach desc $recent_ {
      incr i
      if { [lindex $desc 1] == $file } {
         set label [lindex $desc 0]
         set recent_ [lreplace $recent_ $i $i]
         break
      }
   }

#  Check that the label is unique. Unique labels are created by adding a
#  sequence number in parenthesise to the end of the file basename.
   foreach desc $recent_ {
      set oldlabel [lindex $desc 0]
      if { $oldlabel == $label } {
         set used(1) 1
      } else {
         if { [regexp "^$label \\((.+)\\)\$" $oldlabel match oldseq] } {
            set used($oldseq) 1
         }
      }
   }

   set seq 1
   while { [info exists used($seq)] } {incr seq}
   if { $seq > 1 } { set label "$label ($seq)" }

#  Construct a description of the file holding the file label and the
#  full path.
      set desc [list $label $file]

#  Add the new description to the start of the list.
      set recent_ [lreplace $recent_ 0 -1 $desc]

#  If the list is now longer than 10, retain only the first ten items.
      if { [llength $recent_] > 10 } {
         set recent_ [lrange $recent_ 0 9]
      }

#  Update the list of recently accessed files in the File menu.
      updateRecent

   }

#  Save the current catalogue to a new file.
#  -----------------------------------------
   protected method save_cat {} {

# Check we have a catalogue.
      if { $newcat_ != "" } {

#  Get a new file name in which to store the current catalogue.
         set file [get_file [pwd] {*.FIT} $types_]

#  Do nothing if no file was supplied.
         if { $file != "" } {

#  If no file type was included, use ".FIT".
            if { [file extension $file] == "" } {
               set file "$file.FIT"
            }

#  Check that the supplied file name can be used.
            if { [file isfile $file] } {
               if {![confirm_dialog "File: `[file tail $file]' exists \
                                     - Do you want to overwrite it ?" $w_]} {
                  return
               }
               if {[file isdir $file]} {
                   error_dialog "File: `[file tail $file]' is a directory" $w_
                   return
               }
            }

#  Save the catalogue with this name. If succesful, add the file name to
#  the list of recently accessed files in the File menu.
            if { [$newcat_ save $file] } {
               ::gaia::GaiaPolarimetry::addAllRecent $file
            }
         }
      }
   }

#  Undo the previous operation.
#  -----------------------------
   protected method undo {} {
      if { $canundo_ } {
         set action [$actionlist_ get]
         if { $action != "" } {
            if { ![$actionlist_ first] } {
               $actionlist_ prev
            } else {
               set canundo_ 0
            }
            set canredo_ 1
            setUndoRedo
            set type [$action getType]

            if { $type == "cat" } {
               doCat 1

            } elseif { $type == "style" } {
               doStyle 1

            } else {
               $action undo
            }

            $action annull

         }
      }
   }

#  Redo the previous undone operation.
#  -----------------------------------
   protected method redo {} {
      if { $canredo_ } {
         if { $canundo_ } {
            $actionlist_ next
         }
         set action [$actionlist_ get]
         if { $action != "" } {
            if { [$actionlist_ last] } {
               set canredo_ 0
            }
            set canundo_ 1
            setUndoRedo
            set type [$action getType]

            if { $type == "cat" } {
               doCat 0

            } elseif { $type == "style" } {
               doStyle 0

            } else {
               $action redo
            }

            $action annull

         }
      }
   }

#  Add a new operation (action) to the list of undo-able operations.
#  -----------------------------------------------------------------
   protected method addAction {type desc {obj ""} {ucmd ""} {rcmd ""} } {
      set action [::gaia::GaiaPolAction action#auto $type $desc $obj $ucmd $rcmd]
      if { !$canundo_ } { $actionlist_ reset }
      $actionlist_ add [code $action]
      set canundo_ 1
      set canredo_ 0
      setUndoRedo
      $action annull
   }

#  Establish the state of the undo and redo menu ites, and set their
#  short help.
#  -----------------------------------------------------------------
   protected method setUndoRedo {} {

#  If there is nothing to re-do, disable the Re-do button.
      if { !$canredo_ } {
         $Edit_ entryconfigure "Re-do" -state disabled
         $short_help_win_ add_menu_short_help $Edit_ \
            {Re-do} {There are no operations to Re-do}

# Otherwise, enable the Re-do button, and set the short help to
# indicate what action will be re-done by the next redo operation.
      } else {
         $Edit_ entryconfigure "Re-do" -state normal

         if { $canundo_ } { $actionlist_ next }
         set action [$actionlist_ get]
         set help [$action getDesc]
         $action annull
         if { $canundo_ } { $actionlist_ prev }
         $short_help_win_ add_menu_short_help $Edit_ \
            {Re-do} "Re-do operation ($help)"
      }

#  If there is nothing to undo, disable the Un-do button.
      if { !$canundo_ } {
         $Edit_ entryconfigure "Un-do" -state disabled
         $short_help_win_ add_menu_short_help $Edit_ \
            {Un-do} {There are no operations to Un-do}

#  Otherwise, enable the Un-do button, and set the short help to
#  indicate what action will be un-done by the next undo operation.
      } else {
         $Edit_ entryconfigure "Un-do" -state normal
         set action [$actionlist_ get]
         set help [$action getDesc]
         $action annull
         $short_help_win_ add_menu_short_help $Edit_ \
            {Un-do} "Un-do operation ($help)"
      }
   }

#  Establish the name of the options directory, creating it if it does
#  not already exists.
#  ------------------------------------------------------------------------
   protected method set_optdir {} {
      global ::env
      set optdir_ $env(HOME)/.skycat/GaiaPolarimetry
      if {! [file isdirectory $optdir_]} {
         if { [catch {file mkdir $optdir_} mess] } {
            puts "Error making $optdir_: $mess"
         }
      }
   }

#  Check we have a usable version of polpack
#  -----------------------------------------
   private method checkPolpack {v} {
      global ::env
      set ok 0

#  Get a GaiaApp which provides access to the Polpack "polversion" command.
      if { [info exists env(POLPACK_DIR)] } {
         set polversion [::gaia::GaiaApp \#auto  \
                   -application $env(POLPACK_DIR)/polversion ]

      } else {
         if { [file isdirectory "/star/bin/polpack"] }  {
            set polversion [::gaia::GaiaApp \#auto \
                   -application /star/bin/polpack/polversion ]
         } else {
            error_dialog "Cannot locate the POLPACK directory. \
            Define the environment variable POLPACK_DIR and restart."
         }
      }

#  Check the above worked OK.
      if { $polversion != "" } {

#  Configure the GaiaApp object.
         $polversion configure -notify [code $this completed]
         $polversion configure -parnotify [code $this gotparam]

#  Run polversion comparing the installed version of polpack
#  against the supplied version number.
         set done_ 0
         $polversion runwiths compare=$v
         while { $done_ == 0 } {
            after 500 {set a 1}
            update idletasks
            tkwait variable a
         }

#  Get the result.
         set done_ 0
         $polversion getparam result
         while { $done_ == 0 } {
            after 500 {set a 1}
            update idletasks
            tkwait variable a
         }

         if { $parval_ == 0 || $parval_ == -1 } {
            set ok 1
         } else {
            error_dialog "This toolbox needs POLPACK version $v or later."
         }

# Delete the GaiaApp.
         $polversion delete_sometime

      }

#  Close the window if polpack is not available.
      if { !$ok } {
         close_win
      }

   }

#  Configuration options: (public variables)
#  ----------------------
#  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

#  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

#  Name of RtdImageCtrl widget or a derived class.
   itk_option define -image image Image {} {}

#  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

#  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

#  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

#  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

#  Protected variables: (available to instance)
#  --------------------
   protected {

#  The current catalogue
      variable newcat_ ""

#  A list of undo-able actions.
      variable actionlist_ ""

#  The Edit menu button
      variable Edit_

#  The File menu button
      variable File_

#  The index of the first recent file entry in the File menu.
      variable recentFile_ ""

#  A list of displayed catalogues.
      variable catlist_ ""

#  A list of the used display styles
      variable stylelist_ ""

#  The object which handles display of vectors on the canvas.
      variable display_ ""

#  The object which handles display of vectors in the table.
      variable table_ ""

#  Name of tablelist widget.
      variable tablelist_ ""

#  Name of progress bar widget.
      variable pbar_ ""

#  The previously displayed GaiaPolCat.
      variable prevcat_ ""

#  The previously displayed GaiaPolStyle.
      variable prevsty_ ""

#  Is there anything to undo?
      variable canundo_ 0

#  Is there anything to redo?
      variable canredo_ 0

#  Should single clicks be ignored?
      variable inhibit_single_ 0

#  Directory for storing option values for the polarimetry toolbox.
      variable optdir_

#  Should the table be used to display rows of data?
      variable usetable_ 1

#  Set to 1 when a polpack command completes.
      variable done_ 0

#  Set to the value of the most recently "got" parameter value
      variable parval_ ""

   }

#  Common variables: (shared by all instances)
#  -----------------

#  Name of fileselection dialog window.
   common fileselect_ .polarimetryfs

#  File types recognized by CURSA.
   common types_ { {FITS(.FIT) *.FIT} {FITS(.FITS) *.FITS}
                   {FITS(.GSC) *.GSC} {FITS(.fit) *.fit} {FITS(.fits)
		   *.fits} {FITS(.gsc) *.gsc} {STL(.TXT) *.TXT}
		   {STL(.txt) *.txt} {TST(.TAB) *.TAB} {TST(.tab) *.tab} {Any *} }

#  Array for passing around at global level. Indexed by ($this,param).
   common values_

#  List of recently accessed files (the same list is used by all toolboxes)
   common recent_ ""

#  List of all currently existing polarimetry toolbox objects
   common these_ ""

#  End of class definition.
}
