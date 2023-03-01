#+
#  Name:
#     GaiaFITSHeader

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Display the FITS headers of a dataset.

#  Description:
#     This class displays a dialog that contains the FITS headers
#     of a dataset that is associated with a GaiaNDAccess instance.

#  Invocations:
#
#        GaiaFITSHeader object_name [configuration options]
#
#     This creates an instance of a GaiaFITSHeader object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
#     PWD: Peter Draper (JAC - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-FEB-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaFITSHeader {}

itcl::class gaia::GaiaFITSHeader {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      set lwidth 18
      set vwidth 10

      #  Set window properties.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "FITS Headers ($itk_option(-number))"
      wm geometry $w_ 100x20

      #  Add menubuttons and menus
      add_menubar
      set m [add_menubutton File]

      add_menuitem $m command "Print..." {Print table} \
         -command [code $this print]

      add_menuitem $m command "Close" {Close window} \
         -command [code $this close]

      set m [add_menubutton Header]

      add_menuitem $m cascade "Sort..." {Sort FITS header} \
         -menu [menu $m.sort]

      $m.sort add radiobutton -label "Keyword increasing" \
         -command [code $this sort Keyword "increasing"] \
         -variable [scope sort_] \
         -value "increasing"
      $m.sort add radiobutton -label "Keyword decreasing" \
         -command [code $this sort Keyword "decreasing"] \
         -variable [scope sort_] \
         -value "decreasing"
      $m.sort add radiobutton -label "Natural order" \
         -command [code $this sort "" "natural"] \
         -variable [scope sort_] \
         -value "natural"

      add_menuitem $m cascade "Show / Hide" \
         {Configure layout of table} \
         -menu [menu $m.show]

      $m.show add checkbutton -label "Keyword" \
         -command [code $this show Keyword] \
         -variable [scope show_keyword_] \
         -onvalue 1 -offvalue 0

      $m.show add checkbutton -label "Value" \
         -command [code $this show Value] \
         -variable [scope show_value_] \
         -onvalue 1 -offvalue 0

      $m.show add checkbutton -label "Comment" \
         -command [code $this show Comment] \
         -variable [scope show_comment_] \
         -onvalue 1 -offvalue 0

      itk_component add table {
         util::TableList $w_.table \
            -title "FITS Headers" \
            -hscroll 1 -vscroll 1 \
            -headings "Keyword Value Comment"
      }
      $itk_component(table) set_option Keyword Show $show_keyword_
      $itk_component(table) set_option Value Show $show_value_
      $itk_component(table) set_option Comment Show $show_comment_

      itk_component add buttons {
         frame $w_.buttons
      }
      itk_component add close {
         button $itk_component(buttons).close \
            -text Close -width 8 \
            -command [code $this close]
      }

      #  Search headers for a string.
      itk_component add search {
         util::LabelEntry $itk_component(buttons).search \
            -text Search -valuewidth 10 \
            -command [code $this search]
      }

      pack $itk_component(search) -side left -fill x -expand 1
      pack $itk_component(close)  -side left -fill none -expand 0
      pack $itk_component(buttons) -side bottom -fill both -expand 0
      pack $itk_component(table) -fill both -expand 1 -side top
   }

   #  Destructor:
   #  -----------
   destructor  {

   }

   #  Methods:
   #  --------

   #  Close the window. Just withdraws.
   public method close {} {
      wm withdraw $w_
   }

   #  Sort FITS header
   protected method sort {sort_col sort_order} {
      $itk_component(table) config \
         -sort_cols $sort_col -sort_order $sort_order
      $itk_component(table) new_info
   }

   #  Show / Hide table column
   protected method show {label} {
      set value ""
      switch $label {
         "Keyword" {
            set value $show_keyword_
         }
         "Value" {
            set value $show_value_
         }
         "Comment" {
            set value $show_comment_
         }
      }
      $itk_component(table) set_option $label Show $value
      $itk_component(table) new_info
   }

   #  Print headers.
   protected method print {} {
      busy {
         utilReUseWidget util::TableListPrint $w_.print \
            -table $itk_component(table) \
            -printcmd [$itk_component(table) cget -printcmd]
      }
   }

   #  Search and highlight pattern
   protected method search { string } {
      $itk_component(search) select

      set tbl $itk_component(table)
      set listbox [$tbl component listbox]

      set string [string tolower $string]

      set start_index $search_index_
      set search_index_ 0
      if { "$string" != "$search_string_" } {
         set start_index 0
         set search_string_ $string
      }

      set end_index [$listbox index end]
      if { $start_index >= $end_index } {
         set start_index 0
      }

      set length [string length [$listbox get 0]]

      for { set n $start_index } { $n < $end_index } { incr n } {
         set row [string tolower [$listbox get $n]]
         if {[catch {regexp -indices $string $row indices} index] } {
            continue
         }
         if { $index > 0 } {
            $tbl select_row $n
            lassign $indices i1 i2
            set i1 [expr {double($i1) + ($i2 - $i1) / 2.}]

            lassign [$listbox bbox $n] x0 y0 w h
            set offs [expr {double($length) / $w * [winfo width $w_] / 2.0}]
            set i1 [expr {$i1 - $offs}]
            $listbox xview moveto [expr {(1.0  - (double($length - $i1) / $length))}]
            set search_index_ [incr n]
            break
         }
      }
   }

   #  Activate this widget, updates the FITS headers.
   public method activate {} {
      if { $itk_option(-accessor) != {} } {
         update_headers_
      }
   }

   #  Show or update header info
   protected method update_headers_ {} {
      set search_index_ 0
      set w $itk_component(table)

      set headers [$itk_option(-accessor) fitsheaders]

      #  TableList needs formatting...
      set lastblank 0
      foreach line [split $headers "\n"] {
         set l [string trim $line]
         if {"$l" == "END"} {
            lappend info [list END {} {}]
            break
         }
         if { [lempty $l] } {
            if { ! $lastblank } {
               lappend info [list {} {} {}]
            }
            set lastblank 1
            continue
         }
         set lastblank 0

         set triple [gaia::GaiaFITSHeader::get_kvc $line]
         if { [lempty $triple] } {
            set triple [list INVALID {} $line]
         }
         lappend info $triple
      }
      if { [info exists info] && ! [lempty $info] } {
         $w config -info $info
      }
      [$w component listbox] xview moveto 0.0
   }

   #  Return a tcl list with keyword, value and comment (kvc).
   #  Public proc for re-usability.
   public proc get_kvc { line } {
      #  Use utility parser to handle quoted strings.
      lassign [fits::parsecard $line] key val com

      #  For these keywords make the comment the value, it's tidier.
      if { "$key" == ""  || "$key" == "COMMENT" || "$key" == "HISTORY" } {
         set val $com
         set com ""
      }
      return [list $key $val $com]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaNDAccess instance holding the dataset.
   itk_option define -accessor accessor Accessor {}


   #  Protected variables: (available to instance)
   #  --------------------

   #  Keyword sort setting, increasing, decreasing or natural.
   protected variable sort_ "natural"

   #  Whether to show keywords.
   protected variable show_keyword_ 1

   #  Whether to show value.
   protected variable show_value_ 1

   #  Whether to show comments.
   protected variable show_comment_ 1

   #  Current search index.
   protected variable search_index_ 0

   #  Current search string.
   protected variable search_string_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
