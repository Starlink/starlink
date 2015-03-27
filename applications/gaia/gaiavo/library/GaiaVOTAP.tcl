#+
#  Name:
#     GaiaVOTAP

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a TAP service.

#  Description:
#     Extends the GaiaVOCat class to query a given TAP service.
#     Will plot any RA/Dec coordinates as positions over the displayed
#     image, and attempt to download and display any images that are
#     present in a selected row.
#     Also supports STC-S region display for columns with the correct xtype.

#  Invocations:
#
#        GaiaVOTAP object_name [configuration options]
#
#     This creates an instance of a GaiaVOTAP object. The return is
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
#     Copyright (C) 2014 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     28-MAR-2014 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOTAP {}

itcl::class gaiavo::GaiaVOTAP {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCat

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  Add additional menu items.
   public method init {} {
      GaiaVOCat::init

      #  Override title.
      wm title $w_ "Query TAP service"

      set m [add_menubutton Options "Options menu"]

      #  Change the plot symbols.
      add_menuitem $m command "Set plot symbols..." \
         {Set the symbol (color, size, etc.) to use to plot objects} \
         -command [code $this set_plot_symbols]

      #  Add menus for selecting simple plot symbols.
      add_plot_menus_

      #  Plot symbols button.
      itk_component add plot {
         button $itk_component(buttons).plot \
            -text "Plot" \
            -command [code $this plot]
      }
      pack $itk_component(plot) -side left -before $itk_component(open) -expand 1 -pady 2m
      add_short_help $itk_component(plot) {Plot positions over image}

      #  The "Open" button is now generic and means download an image (or
      #  cube), if this is present as part of the reponse.
      $itk_component(open) configure -text "Display"
      add_short_help $itk_component(open) \
         {Download and handle any displayables}

      #  Display into new window.
      itk_component add displaynew {
         button $itk_component(buttons).displaynew \
            -text "Display in new" \
            -command [code $this display_in_new]
      }
      pack $itk_component(displaynew) -before $itk_component(close) \
         -side left -expand 1 -pady 2m
      add_short_help $itk_component(displaynew) \
         {Download and handle any displayables in new window}

      #  Set names for the canvas tags used for all symbols. This defined
      #  in the imgplot command.
      set tag_ $w_.cat
      set object_tag_ $tag_.objects
      set label_tag_ $tag_.labels

      #  Add bindings for symbols.
      $canvas_ bind $object_tag_  <1> [code $this select_symbol current 0]
      $canvas_ bind $object_tag_  <Shift-1> \
                                  [code $this select_symbol current 1]
      $canvas_ bind $object_tag_  <Control-1> \
                                  [code $this select_symbol current 1]
      $canvas_ bind $object_tag_  <Any-Enter> "$canvas_ config -cursor tcross"
      $canvas_ bind $object_tag_  <Any-Leave> "$draw_ reset_cursor"

      #  Button release selects symbols for selected rows.
      bind $itk_component(results).listbox <ButtonRelease-1> \
         [code $this select_result_row]
   }

   #  Add the component that will control the query.
   protected method add_query_component_ {} {

      itk_component add tap {
         gaiavo::GaiaVOTAPQuery $w_.tap \
            -accessURL $itk_option(-accessURL) \
            -shortname $itk_option(-shortname) \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done] \
            -gaiactrl [$itk_option(-gaia) get_image]
      }
      pack $itk_component(tap) -side top -fill x -expand 1
      add_short_help $itk_component(tap) \
         {Controls for querying the TAP service}

      set query_component_ $itk_component(tap)
   }

   #  New catalogue, set default plot symbol.
   public method query_done {status result} {
      GaiaVOCat::query_done $status $result
      if { $status } {
         set_default_plot_symbol_
      }
   }

   #  Display the selected row in a new window.
   public method display_in_new {} {
      set new_window_ 1
      foreach row [$itk_component(results) get_selected] {
         open_service_ $row
      }
      set new_window_ 0
   }

   #  Open a service, "args" is a list of values from a row of the current
   #  table. Assumes that this means, display an image... XXX Could probably
   #  check for other "displayables", tables and cubes as well..
   protected method open_service_ {args} {
      #  Need to locate a field to get the URL for downloading any images...
      set ucds [$w_.cat ucd]
      set n 0
      set accessref {}
      foreach ucd $ucds {
         if { [string match -nocase "*accessref*" $ucd] } {
            set accessref [eval lindex $args $n]
            break
         }
         incr n
      }

      if { $itk_option(-gaia) != {} && $accessref != {} } {
         if { $urlget_ == {} } {
            set urlget_ [gaia::GaiaUrlGet .\#auto \
                            -notify_cmd \
                            [code $this display_image_ $new_window_]]
         }
         blt::busy hold $w_
         $urlget_ get $accessref
      }
   }

   #  Display an image, if it exists.
   protected method display_image_ {new_window filename type} {
      blt::busy release $w_
      if { [::file exists $filename] } {
         if { $new_window } {
            $itk_option(-gaia) newimage_clone $filename
         } else {
            $itk_option(-gaia) open $filename
         }
      }
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
      }
      set urlget_ {}
   }

   #  Plot the RA and Dec positions on the image using the defined symbols.
   public method plot {} {

      #  Clear any existing positions.
      clear_plot

      #  Create an AST FrameSet to describe the coordinates.
      set att {}
      set system [$w_.cat system]
      if { $system != {} } {
         append att "System=$system,"
      }
      set epoch [$w_.cat epoch]
      if { $epoch != {} } {
         append att "Epoch=$epoch,"
      }
      set equinox [$w_.cat equinox]
      if { $equinox != {} } {
         append att "Equinox=$equinox,"
      }

      #  Fallback to standard.
      if { $equinox == {} && $att == {} } {
         append att "System=FK5,Equinox=$equinox_"
      }

      #  Create the AST FrameSet.
      set astref [gaiautils::astskyframeset $att]

      #  See if we have an STC region column, if so make that available.
      set xtypes [$w_.cat xtype]
      set n 0
      foreach xtype $xtypes {
         if { [string match -nocase "adql:region" $xtype] } {
            #  STC column.
            $w_.cat entry update [list "stc_col $n"] $last_result_
            break
         }
         incr n
      }

      #  Do the plot.
      if {[catch {$w_.cat imgplot $rtdimage_ $info_ $astref $headings_} msg]} {
         error_dialog $msg
      }
   }

   #  Clear the plot of any existing symbols.
   public method clear_plot {} {
      catch {$canvas_ delete $tag_}
   }

   #  Set the catalogue plotting symbol to the default.
   protected method set_default_plot_symbol_ {} {
      if { $type_ == {} } {
         set type_ circle
      }
      if { $colour_ == {} } {
         set colour_ red
      }
      if { $size_ == {} } {
         set size_ 3
      }
      set_simple_symbol_
   }

   #  Set the catalogue plotting symbol to that of the markers menu.
   protected method set_simple_symbol_ {} {
      set symbol [list {} [list $type_ $colour_ {} {} {} {}] [list $size_ {}]]
      $w_.cat symbol $symbol
      plot
   }

   #  Pop up a dialog to set the plot symbols to use for this catalogue.
   public method set_plot_symbols {} {
      set columns $headings_
      if {[llength $columns] == 0} {
         info_dialog \
            "Please make a query first so that the column names are known" $w_
         return
      }
      utilReUseWidget gaia::GaiaSymbolConfig $w_.symconf \
         -catalog $itk_option(-catalog) \
         -astrocat [code $w_.cat] \
         -columns $columns \
         -command [code $this plot]
   }

   #  Add the menu items for selecting simple plot symbols.
   protected method add_plot_menus_ {} {
      set m [add_menubutton Markers "Plot symbol markers"]

      #  Add the menus
      foreach {label name} {Type type Size size Colour colour} {
         $m add cascade -label $label -menu [menu $m.$name]
      }

      #  Add short help texts for menu items
      add_menu_short_help $m Type {Set the plot symbol shape}
      add_menu_short_help $m Size {Set the plot symbol size (pixels)}
      add_menu_short_help $m Colour {Set the plot symbol colour}

      #  Add the known types.
      foreach {name} $symbol_types_ {
         $m.type add radiobutton \
            -value $name \
            -bitmap $name \
            -variable [scope type_] \
            -command [code $this set_simple_symbol_]
      }

      #  Size menu
      foreach i {3 5 7 9 11 15 21 31} {
         $m.size add radiobutton \
            -value $i \
            -label $i \
            -variable [scope size_] \
            -command [code $this set_simple_symbol_]
      }

      #  Colour menu
      foreach i $colours_ {
         $m.colour add radiobutton \
            -value $i \
            -command [code $this set_simple_symbol_] \
            -variable [scope colour_] \
            -background $i
      }
   }

   #  Select a symbol, given the canvas id and optional row number
   #  in the table listing. If $toggle is 0, deselect all other symbols
   #  first, otherwise toggle the selection of the items given by $id.
   public method select_symbol {id toggle {rownum -1}} {
      set tag [lindex [$canvas_ gettags $id] 0]

      if {$rownum < 0} {
         set rownum [get_table_row $id]
         if {$rownum < 0} {
            return
         }
      }

      if {$toggle} {
         if {[$draw_ item_has_tag $tag $w_.selected]} {
            deselect_symbol $tag
            $itk_component(results) deselect_row $rownum
            return
         }
      } else {
         deselect_symbol $w_.selected
      }

      if {"$rownum" >= 0} {
         $itk_component(results) select_row $rownum [expr !$toggle]
         $itk_component(results) select_result_row
      }

      foreach i [$canvas_ find withtag $tag] {
         set width [$canvas_ itemcget $i -width]
         $canvas_ itemconfig $i -width [expr $width+2]
      }
      $canvas_ addtag $w_.selected withtag $tag
      $canvas_ raise $tag $rtdimage_
   }

   #  Return the table row index corresponding the given symbol canvas id.
   #  Note: The imgplot subcommand adds a canvas tag "row#$rownum" that we can
   #  use here.  Also: cat$id is first tag in the tag list for each object.
   public method get_table_row {id} {

      set tags [$canvas_ gettags $id]
      #  Look for row#tag (but only if not sorted!)
      if {[llength [$w_.cat sortcols]] == 0} {
         foreach tag $tags {
            if {[scan $tag "row#%d" rownum] == 1} {
               return $rownum
            }
         }
      }

      #  Search for $id in query results (slow way).
      set tag [lindex $tags 0]
      set rownum -1
      foreach row [$itk_component(results) get_contents] {
         incr rownum
         set id [lindex $row [$w_.cat id_col]]
         if { "cat$id" == "$tag" } {
            return $rownum
         }
      }

      #  Not found.
      return -1
   }

   #  Deselect the given symbol, given its canvas tag or id.
   public method deselect_symbol {tag} {
      foreach i [$canvas_ find withtag $tag] {
         set width [$canvas_ itemcget $i -width]
         $canvas_ itemconfig $i -width [expr $width-2]
      }
      $canvas_ dtag $tag $w_.selected
   }

   #  Called when a row in the table is selected.
   protected method select_result_row {} {
      $itk_component(results) select_result_row

      #  Clear symbol selection
      deselect_symbol $w_.selected

      #  Select symbols matching selected rows
      foreach row [$itk_component(results) get_selected_with_rownum] {
         lassign $row rownum row
         set id [lindex $row [$w_.cat id_col]]
         if {"$id" == ""} {
            continue
         }
         select_symbol cat$id 1 $rownum
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Instance of GAIA to display the catalogue.
   itk_option define -gaia gaia Gaia {} {
      set rtdctrl_ [$itk_option(-gaia) get_image]
      set rtdimage_ [$rtdctrl_ get_image]
      set canvas_ [$rtdctrl_ get_canvas]
      set draw_ [$rtdctrl_ component draw]
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Various elements from the GAIA instance.
   protected variable rtdctrl_ {}
   protected variable rtdimage_ {}
   protected variable canvas_ {}
   protected variable draw_ {}

   #  Equinox for VO catalogues. Really ICRS but J2000 (=FK5/J2000).
   protected variable equinox_ "J2000"

   #  Various tags associated with the positions when plotted. These
   #  are defined by imgplot method.
   protected variable tag_ {}
   protected variable object_tag_ {}
   protected variable label_tag_ {}

   #  Simple symbol values.
   protected variable type_ {}
   protected variable colour_ {}
   protected variable size_ {}

   #  Handler for downloading images.
   protected variable urlget_ {}

   #  Whether to display into new window.
   protected variable new_window_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   protected common symbol_types_ \
      {circle square cross triangle diamond stcshape}

   protected common colours_ {white grey90 grey80 grey70 grey60 grey50
      grey40 grey30 grey20 grey10 black red green blue cyan magenta yellow}

}
