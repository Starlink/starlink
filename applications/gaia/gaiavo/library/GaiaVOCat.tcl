#+
#  Name:
#     GaiaVOCat

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a VO service and display the result.

#  Description:
#     This class defines a standard basic UI for containing the query
#     elements specific to a particular VO service (like SIAP). It
#     controls the query and displays the resultant VOTable, which can
#     be edited. On acceptance the various servers will be added to the
#     general catalogue configuration, so that detailed queries for
#     images and catalogues (same as any other Skycat server).
#
#     This is the VO equivalent of the cat::AstroCat class.

#  Invocations:
#
#        GaiaVOCat object_name [configuration options]
#
#     This creates an instance of a GaiavoVolume object. The return is
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
#     Copyright (C) 2008 Science and Technology Facilities Council
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     31-JUL-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCat {}

itcl::class gaiavo::GaiaVOCat {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args

      #  Add a short help window.
      make_short_help

      #  UI elements.
      open_catalog

      #  Add menubar.
      add_menubar

      #  File menu.
      set m [add_menubutton File]

      add_menuitem $m command "Close" \
         {Close this window} \
         -command [code $this close]

      #  Edit menu.
      set m [add_menubutton Edit]

      add_menuitem $m command "Remove selected" \
         {Remove selected rows from the results} \
         -command [code $this remove_selected]

      add_menuitem $m command "Add new row..." \
         {Enter the data for a new row of results} \
         -command [code $this enter_new_object]

      add_menuitem $m command "Edit selected row..." \
         {Edit the data for the selected row} \
         -command [code $this edit_selected_object]

      #  Options menu.
      set m [add_menubutton Options]

      add_menuitem $m command "Set Sort Columns..." \
         {Set options for sorting the query results} \
         -command [code $this sort_dialog]

      add_menuitem $m command "Hide/Show Columns..." \
         {Set options for displaying columns of the query results} \
         -command [code $this select_columns]

      add_menuitem $m command "Proxies..."  \
         "Define an HTTP proxy server for use with a firewall." \
         -command [code cat::AstroCat::proxies]

      #  Do the initial check for proxies.
      check_proxies

      #  Add the registry query component.
      itk_component add registry {
         gaiavo::GaiaVORegistrySearch $w_.registry \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done]
      }
      pack $itk_component(registry) -side top -fill both -expand 1
      add_short_help $itk_component(registry) {Controls to set registry query}

      #  Add the table for displaying the query results.
      itk_component add results {
         QueryResult $w_.results \
            -astrocat [code $w_.cat] \
            -title "Search Results" \
            -hscroll 1 \
            -height 12 \
            -sortcommand [code $this set_sort_cols] \
            -layoutcommand [code $this set_show_cols] \
            -selectmode extended \
            -exportselection 0
      } {
      }
      pack $itk_component(results) -side top -fill both -expand 1
      bind $itk_component(results).listbox <ButtonRelease-1> \
         [code $this select_result_row_]

      add_short_help $itk_component(results) \
         {Query results: {bitmap b1} = select object, \
             double click {bitmap b1} = label object, \
             {bitmap dragb2} = scroll list}

      #  Add the dialog button frame and the action buttons.
      itk_component add buttons {
         frame $w_.buttons -borderwidth 2 -relief raised
      }
      pack  $itk_component(buttons) -side top -fill x

      itk_component add search {
         button $itk_component(buttons).search \
            -text "Search" \
            -command [code $this search]
      }
      pack $itk_component(search) -side left -expand 1 -pady 2m
      add_short_help $itk_component(search) \
         {{bitmap b1} = start catalogue search}

      itk_component add stop {
         button $itk_component(buttons).stop \
            -text "Stop" \
            -state disabled \
            -command [code $this interrupt]
      }
      pack $itk_component(stop) -side left -expand 1 -pady 2m
      add_short_help $itk_component(stop) \
         {{bitmap b1} = interrupt the current operation}

      itk_component add close {
         button $itk_component(buttons).close \
            -text "Close" \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -pady 2m
      add_short_help $itk_component(close) {{bitmap b1} = close this window}

      #  Add the ProgressBar, looks busy during query.
      itk_component add progressbar {
         ProgressBar $w_.progress
      }
      pack $itk_component(progressbar) -side top -fill x
      add_short_help itk_component(progressbar) \
         {Progress bar: displays status of work in progress}
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  Close this window.
   public method close {} {
      wm withdraw $w_
   }

   #  Pop up a dialog to sort the list.
   public method sort_dialog {} {
      $itk_component(results) sort_dialog
   }

   #  Called when the user has selected columns to sort the results by.
   #  The first arg is the sort columns, the second arg is the order
   #  (increasing, decreasing).
   public method set_sort_cols {sort_cols sort_order} {
      if { "[$w_.cat sortcols]" != "$sort_cols" \
              || "[$w_.cat sortorder]" != "$sort_order"} {
         $w_.cat sortcols $sort_cols
         $w_.cat sortorder $sort_order
         cat::CatalogInfo::save {} $w_ 0
         $itk_component(results) config -sort_cols $sort_cols \
            -sort_order $sort_order
         search
      }
   }

   #  Pop up a dialog to select table columns to display.
   public method select_columns {} {
      $itk_component(results) select_columns
   }

   #  Called when the user has selected columns to show.
   public method set_show_cols {cols} {
      set show [$w_.cat showcols]
      if { "$show" == {} } {
         set show [$itk_component(results) get_headings]
      }
      if { "$show" != "$cols" } {
         $w_.cat showcols $cols
         cat::CatalogInfo::save {} $w_ 0
      }
   }

   #  Clear the table listing.
   public method clear {} {
      catch {$itk_component(results) clear}
   }

   #  Remove the currently selected rows.
   public method remove_selected {} {
      $itk_component(results) remove_selected

      # update the display
      clear
      search
   }

   #  Pop up a dialog to enter the data for a new row.
   public method enter_new_object {} {
      $itk_component(results) enter_new_object [code $this search]
   }


   #  Pop up a window so that the user can edit the selected object(s).
   public method edit_selected_object {} {
      $itk_component(results) edit_selected_object [code $this search]
   }

   #  Open the catalogue for this window.
   public method open_catalog {} {
      astrocat $w_.cat

      #  Normally -catalog should be specified when creating this widget
      #  if not, choose a default...
      if { $itk_option(-catalog) == {} } {
         return
      }

      #  Open the catalogue.
      set name $itk_option(-catalog)
      if { [catch {$w_.cat open $name $itk_option(-catalogdir)} msg] } {
         error_dialog $msg $w_
         return -code error
      }

      #  Display catalogue name in header and icon.
      wm title $w_ "[$w_.cat longname $name $itk_option(-catalogdir)] ($itk_option(-number))"
      wm iconname $w_ [$w_.cat shortname $name $itk_option(-catalogdir)]
   }

   #  Interrupt the current search.
   public method interrupt {} {
      $itk_component(registry) interrupt
      set_feedback off
      catch {$itk_component(results) config -title "Search Results"}
      set_state normal
   }

   #  Set/reset widget states while busy.
   public method set_state {state} {
      if { $state == "normal" } {
         $itk_component(search) config -state normal
         $itk_component(stop) config -state disabled
      } else {
         $itk_component(search) config -state disabled
         $itk_component(stop) config -state normal
      }
      update idletasks
      $itk_component(progressbar) reset
   }

   # This method is called when the background query is done.
   public method query_done {result} {
      set_state normal

      if { ! [file exists $result] } {
         error_dialog $result $w_
         after 0 [list $itk_component(progressbar) config -text $result]
      } else {

         busy {
            $w_.cat open $result

            #  Need to update QueryResult with content?
            set prev_headings $headings_
            set headings_ [$w_.cat headings]
            set info_ [$w_.cat content]

            #  Update table.
            $itk_component(results) config -headings $headings_

            #  Initial show columns.
            if { $itk_option(-show_cols) != {} } {
               $itk_component(results) set_options $headings_ Show 0
               $itk_component(results) set_options $itk_option(-show_cols) Show 1
            }

            if { "$prev_headings" != "$headings_" } {
               $itk_component(results) update_options
            }

            $itk_component(results) config -info $info_

            $itk_component(results) config \
               -title "Returned [$itk_component(results) total_rows] rows"

         }
      }
   }

   #  Start the catalogue search based on the current search options and
   #  display the results in the table.
   public method search {args} {
      #  Start the query in the background.
      catch {
         $itk_component(results) config -title "Searching..."
         $itk_component(results) clear
      }

      set_state disabled
      $itk_component(progressbar) config -text "Searching registry..."
      $itk_component(progressbar) look_busy

      $itk_component(registry) query
   }

   #  Row is selected.
   protected method select_result_row_ {} {
      puts "row selected, do something..."
   }


   #  Respond to feedback about query progress (stop and start).
   protected method set_feedback {onoff} {
      if { $onoff == "off" } {
         $itk_component(progressbar) reset
      }
   }

   #  Check for a file ~/.skycat/proxies, once each session, and use it to
   #  initialize environment variables for a proxy server.
   public proc check_proxies {} {
      if { $checked_proxies_ } {
         return
      }
      cat::ProxyDialog::check_proxies $::env(HOME)/.skycat/proxies
      set checked_proxies_ 1
   }

   #  Pop up a dialog to set or change the HTTP proxy server.
   public proc proxies {} {
      utilReUseWidget ProxyDialog .proxy \
         -configfile $::env(HOME)/.skycat/proxies
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of catalogue.
   itk_option define -catalog catalog Catalog {}

   #  Name of catalogue directory, or a tcl list forming the path to it (empty
   #  means root).
   itk_option define -catalogdir catalogDir CatalogDir {}

   #  Columns to show. Defined once during initialisation.
   itk_option define -show_cols show_cols Show_Cols {}

   #  Command to execute when a query is completed.

   #  Protected variables: (available to instance)
   #  --------------------

   #  List of catalogue column headings (from results of most recent query).
   protected variable headings_ {}

   #  Result from most recent query (list of rows).
   protected variable info_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Flag: set to 1 after we checked for a proxy server.
   protected common checked_proxies_ 0
}
