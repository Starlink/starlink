#+
#  Name:
#     PlasticSearch

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Displays a table that originated from a PLASTIC request.

#  Description:
#     This class extends GaiaSearch to provide methods required for 
#     subsequent PLASTIC control of a catalogue which has been acquired
#     through the PLASTIC system.

#  Inheritance:
#     gaia::GaiaSearch

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor (Bristol University)
#     {enter_new_authors_here}

#  History:
#     10-AUG-2006 (MBT):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual PlasticSearch {}

itcl::class gaia::PlasticSearch {
   inherit gaia::GaiaSearch

   #  Constructor.
   constructor {args} {
      eval itk_initialize $args
   }

   #  Initialise widget.
   public method init {} {
      GaiaSearch::init

      set plastic_app_ [gaia::Gaia::get_plastic_app]
      if {$plastic_app_ != ""} {

         #  Add a new menu for PLASTIC-related activities.
         set interopmenu_ [add_menubutton Interop]
         set m $interopmenu_
         configure_menubutton Interop -underline 6
         add_short_help $itk_component(menubar).interop \
            {Interop menu: catalogue-related tool interoperability using PLASTIC}

         $m add checkbutton -label "Selection broadcasts row index" \
            -variable [scope send_rows] 
         $m add checkbutton -label "Selection broadcasts row position" \
            -variable [scope send_radecs]

         $m add separator

         set send_info_menu [menu $m.send_info]
         add_menuitem $m command "Broadcast current search" \
            {Send all visible rows to PLASTIC applications} \
            -command [code $this transmit_info {}]
         add_menuitem $m cascade "Send current search" \
            {Send all visible rows to a selected PLASTIC application} \
            -menu $send_info_menu
 
         set send_selection_menu [menu $m.send_selection]
         add_menuitem $m command "Broadcast current selection" \
            {Send current row selection to PLASTIC applications} \
            -command [code $this transmit_selection {}]
         add_menuitem $m cascade "Send current selection" \
            {Send current row selection to a selected PLASTIC application} \
            -menu $send_selection_menu

         #  Arrange for the menu to be kept up to date with the current
         #  state of the PLASTIC connection.
         $plastic_app_ plastic_reg_command [code $this plastic_reg_changed_]
         plastic_reg_changed_
         set tracker [$plastic_app_ cget -app_tracker]
         $tracker plastic_apps_command [code $this plastic_apps_changed_]
         plastic_apps_changed_
      }
   }

   #  Selects the rows identified by a given list of integer values.
   public method select_indices {idx_list} {
      set info {}
      set all [all_rows_]
      foreach idx $idx_list {
         lappend info [lindex $all $idx]
      }
      set_info $headings_ $info
   }

   #  Highlights one of the rows in the table represented by this object.
   #  The given idx value is the index of the row in the full table as
   #  originally loaded.
   public method highlight_index {idx} {

      #  Work out the index into the current search result.  This isn't
      #  very efficient (search through all rows for an identical match)
      #  but I don't see how else it can be done using the existing data
      #  structures available to this class.
      set row [lindex [all_rows_] $idx]
      set info_idx [lsearch $info_ $row]
      if {$row >= 0} {

         #  Highlight the chosen row in the table viewer widget.
         $results_ select_row $info_idx

         #  Highlight the associated symbol on the image widget.
         set id_col [$w_.cat id_col]
         if {$id_col >= 0} {
            set id [lindex $row $id_col]
            if {$id != ""} {
               select_symbol cat$id 0 $info_idx
            }
         }
      }
   }

   #  Override select_result_row to perform additional PLASTIC-related
   #  row highlighting activities.
   public method select_result_row {} {
      skycat::SkySearch::select_result_row
      activate_selected_row
   }

   #  Override select_symbol to perform additional PLASTIC-related
   #  row highlighting activities.
   public method select_symbol {id toggle {rownum -1}} {
      SkySearch::select_symbol $id $toggle $rownum
      activate_selected_row
   }

   #  Transmits the current selection to other PLASTIC apps.
   public method transmit_selection {recipients} {
      transmit_rows [$results_ get_selected] $recipients
   }

   #  Transmits the current search result to other PLASTIC apps.
   public method transmit_info {recipients} {
      transmit_rows $info_ $recipients
   }

   #  Sends the current row selection as a list of objects to be 
   #  highlighted in other PLASTIC applications.
   public method transmit_rows {rows recipients} {
      if {[catch {
         set sender [gaia::Gaia::get_plastic_sender]
         if {$sender != "" && $table_id != ""} {
            set all_rows [all_rows_]
            set idx_list {}

            #  This list is shockingly inefficient, being an O(N*M) full-text
            #  match of the M selected rows against the N rows in the 
            #  whole list.  Don't know how else it can be done though.
            foreach row $rows {
               set idx [lsearch $all_rows $row]
               if {$idx > -1} {
                  lappend idx_list $idx
               }
            }
            $sender send_selection $table_id $idx_list $recipients 
         }
      } msg]} {
         puts "selection send error: $msg"
      }
   }

   #  Invoked when the row selection may have changed.  According to the
   #  current state of this object, it may transmit messages to the
   #  PLASTIC system concerning the new row selection.
   public method activate_selected_row {} {

      #  Send the row index to PLASTIC if required.
      if {$send_rows} {
         if {[catch {
            set sender [gaia::Gaia::get_plastic_sender]
            if {$sender != "" && $table_id != ""} {
               set rows [$results_ get_selected]
               if {[llength $rows] == 1} {
                  set row [lindex $rows 0]
                  set base_idx [lsearch [all_rows_] $row]
                  if {$base_idx >= 0} {
                     $sender send_row $table_id $base_idx {}
                  }
               }
            }
         } msg]} {
            puts "row activation error: $msg"
         }
      }

      #  Send the row sky position to PLASTIC if required.
      if {$send_radecs} {
         if {[catch {
            set sender [gaia::Gaia::get_plastic_sender]
            set ra_col [$w_.cat ra_col]
            set dec_col [$w_.cat dec_col]
            if {$sender != "" && $table_id != "" &&
                $ra_col >= 0 && $dec_col >= 0} {
                set rows [$results_ get_selected]
                if {[llength $rows] == 1} {
                   set row [lindex $rows 0]
                   set ra  [lindex $row $ra_col]]
                   set dec [lindex $row $dec_col]
                   $sender send_radec $ra $dec {}
                }
            }
         } msg]} {
            puts "row activation error: $msg"
         }
      }
   }

   #  Returns a list of all the rows in the table loaded into this widget.
   #  This is a superset of the rows currently visible.
   protected method all_rows_ {} {
      return [$w_.cat query]
   }

   #  Called when the PLASTIC connection goes up or down.
   protected method plastic_reg_changed_ {} {

      #  Configure the menu items all enabled/disabled according to whether
      #  there is a PLASTIC connection.
      if {$plastic_app_ != ""} {
         set is_reg [$plastic_app_ is_registered]
      } else {
         set is_reg 0
      }
      set when_reg [expr {$is_reg ? "normal" : "disabled"}]
      set nitem [$interopmenu_ index last]
      for {set item 0} {$item <= $nitem} {incr item} {
         catch {$interopmenu_ entryconfigure $item -state $when_reg}
      }
   }

   #  Called when external applications register or unregister with PLASTIC.
   protected method plastic_apps_changed_ {} {

      #  Configure some of the cascade menus so that they have one entry
      #  for each of the currently registered PLASTIC applications
      #  capable of receiving the relevant messages.
      set selection_menu $interopmenu_.send_selection
      set info_menu $interopmenu_.send_info
      $info_menu delete 0 last
      $selection_menu delete 0 last
      if {[$plastic_app_ is_registered]} {
         set tracker [$plastic_app_ cget -app_tracker]
         set msg_id "ivo://votech.org/votable/showObjects"
         set msg_apps [$tracker get_supporting_apps $msg_id]
         foreach app $msg_apps {
            set appname [$app cget -name]
            set appid [$app cget -id]
            add_menuitem $info_menu command "Send to $appname" \
               "Send all visible rows to $appname" \
               -command "[code $this transmit_info $appid]"
            add_menuitem $selection_menu command "Send to $appname" \
               "Send current row selection to $appname" \
               -command "[code $this transmit_selection $appid]"
         }
      }
   }

   #  ID of the table loaded into this widget.  This is the ID used by 
   #  PLASTIC messages to identify the table.
   public variable table_id {}

   #  Whether to broadcast each row index when it is highlighted.
   public variable send_rows 1

   #  Whether to broadcast each row sky position when it is highlighted.
   public variable send_radecs 0

   protected variable plastic_app_
   protected variable interopmenu_
}
