#+
#  Name:
#     GaiaSearch

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for extending the abilities of SkySearch

#  Description:
#     This class extends SkySearch adding the facilities required for
#     GAIA. Check the methods below for what is available.

#  Invocations:
#
#        GaiaSearch object_name [configuration options]
#
#     This creates an instance of a GaiaSearch object. The
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
#     skycat::SkySearch

#  Copyright:
#     Copyright (C) 1998-2001 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     14-SEP-1998 (PWD):
#        Original version.
#     09-AUG-2001 (PWD):
#        Added the "Extract selected..." item to the Options menu.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSearch {}

#  This class extends the SkySearch catalog widget browser class (see
#  SkySearch (n) and AstroCat(n) to add support for plotting objects
#  and displaying images.

itcl::class gaia::GaiaSearch {
   inherit skycat::SkySearch

   #  Constructor.
   #  ============
   constructor {args} {
      set tag_ {};               #  Not sure why this is needed.
      eval itk_initialize $args
   }

   #  Destructor.
   #  ===========
   destructor {
      #  Release AST frameset.
      if { $astref_ != 0 } {
         catch {gaiautils::astannul $astref_}
      }
   }

   #  Init method, called after the options have been evaluated.
   public method init {} {

      #  The catalogue may be accessed during construction (and converted
      #  to native format), catch any errors and dispose of this object.
      if { [catch {skycat::SkySearch::init} msg] } {
         catch {::delete object $this}
         error $msg
      }

      #  Remove the "Save with image" menu option. This is not
      #  available.
      set m [get_menu File]
      if { $iscat_ } {
         $m delete "Save with image"
      }

      #  Use our Open dialog so we can browse for HDUs.
      if { $iscat_ } {
         $m entryconfigure "Open" -command \
            [code GaiaSearch::get_local_catalog $itk_option(-id) $w_]
      }

      #  Remove <Enter> binding as this slows down the zoom window a lot.
      $canvas_ bind $object_tag_  <Any-Enter> {}
      $canvas_ bind $object_tag_  <Any-Leave> {}

      #  Add menu options that we want. Only add these options for
      #  catalogue windows, not image servers.
      if { $iscat_ } {

         #  Add our local options.
         set m [get_menu Options]
         $m add separator

         #  Add the ability to extract the selected rows to a new
         #  catalogue and display in a window.
         add_menuitem $m command "Extract selected" \
            {Extract any selected rows and display as a new catalog} \
            -command [code $this extract_selected_]

         #  Add centre on selected object option.
         add_menuitem $m command "Center on selected row" \
            {Centre main image on selected object (also bound to {bitmap b2} in table)} \
            -command [code $this centre_selected_object_]

         #  Clear all objects. Much faster than graphics clear.
         add_menuitem $m command "Clear all objects" \
            {Clear all objects displayed on image} \
            -command [code $this clear_all_objects]

         #  Add labels to all objects.
         add_menuitem $m command "Label all objects" \
            {Label all objects displayed on image (same as double clicking on all rows)} \
            -command [code $this label_all_objects]

         #  Select a font for labelling objects.
         add_menuitem $m cascade "Label Font" \
            {Select a font for labelling objects} \
            -menu [menu $m.font]
         foreach i $itk_option(-fonts) {
            $m.font add radiobutton \
               -value $i \
               -label {abc} \
               -command [code $this configure -canvasfont $i] \
               -variable [scope itk_option(-canvasfont)] \
               -font $i
         }

         #  Select colour for labelling objects.
         add_menuitem $m cascade "Label Colour" \
            {Select a colour for labelling objects} \
            -menu [menu $m.labelcolour]
         foreach i $itk_option(-colors) {
            $m.labelcolour add radiobutton \
               -value $i \
               -command [code $this configure -labelcolour $i] \
               -variable [scope itk_option(-labelcolour)] \
               -background $i
         }

         #  Add interpret X and Y coordinates as pixel coordinate option.
         $m add checkbutton  -label {Use NDF origins} \
            -variable [scope itk_option(-use_origin)] \
            -onvalue 1 \
            -offvalue 0 \
            -command [code $this set_origin]
         $short_help_win_ add_menu_short_help $m \
            {Use NDF origins} \
            {X and Y are NDF pixel coordinates and need correcting for NDF origins}
         set_origin

         #  Add option for a some help on this window (not image servers).
         set m [add_help_button catalogues "Catalogues Overview..." \
                   {General information about catalogues}   ]
         set m [add_help_button query "On Window..." \
                   {Display help on using this window}   ]
      }

      #  Check that the catalogue is opened.
      if { [$w_.cat url] == {} } {
         after idle [code delete object $this]
      }
   }

   #  Add the table for displaying the query results. Redefined from
   #  parent class SkySeach to use GaiaQueryResult and to add a
   #  binding to centre on the selected object.
   method add_result_table {} {

      #  Table to display results as text
      itk_component add results {
         set results_ [gaia::GaiaQueryResult $w_.results \
                          -astrocat [code $w_.cat] \
                          -skycat $skycat_ \
                          -title "Search Results" \
                          -hscroll 1 \
                          -height 12 \
                          -sortcommand [code $this set_sort_cols] \
                          -layoutcommand [code $this set_show_cols] \
                          -selectmode extended \
                          -exportselection 0]

      } {
      }
      pack $itk_component(results) -side top -fill both -expand 1
      bind $results_.listbox <ButtonRelease-1> [code $this select_result_row]
      $results_ set_options {MORE PREVIEW more preview} Show 0
      bind $results_.listbox <Double-ButtonPress-1> [code $this label_selected_object]
      bind $results_.listbox <ButtonPress-2> [code $this centre_selected_object_]
   }

   #  Open a catalog. Override the AstroCat version so that we can apply
   #  info changes before they are used, at which stage many can not
   #  be undone, like the WCS columns. The changes are applied using a
   #  given command which will be called immediately after the catalogue
   #  is opened.
   public method open_catalog {} {
      cat::AstroCat::open_catalog
      if { $itk_option(-open_cmd) != {} } {
         eval $itk_option(-open_cmd)
      }
   }

   #  Method to set the symbol used when plotting. This is the same
   #  format as a line in a local catalogue.
   public method set_symbol {args} {
      $w_.cat symbol $args
   }

   #  Method to get the symbol used when plotting.
   public method get_symbol {} {
      return [$w_.cat symbol]
   }

   #  Method to set the symbol used when plotting. This is the same
   #  format as a line in a local catalogue. This version only sets
   #  symbol, if not already set.
   public method maybe_set_symbol {args} {
      if { [$w_.cat symbol] == {} } {
         $w_.cat symbol $args
      }
   }

   #  Set the columns in which the various parameters occur.
   public method set_dec_col {col} {
      $w_.cat entry update [list "dec_col $col"]
   }
   public method set_ra_col {col} {
      $w_.cat entry update [list "ra_col $col"]
   }
   public method set_x_col {col} {
      $w_.cat entry update [list "x_col $col"]
   }
   public method set_y_col {col} {
      $w_.cat entry update [list "y_col $col"]
   }

   #  Override method that creates SkyQuery, so that we can we
   #  GaiaQuery instead.
   public method add_search_options {} {
      itk_component add searchopts {
         set searchopts_ [gaia::GaiaQuery $w_.searchopts \
                             -relief groove \
                             -borderwidth 2 \
                             -astrocat [code $w_.cat] \
                             -skycat $skycat_ \
                             -searchcommand [code $this search] \
                             -command [code $this query_done]]
      }
      pack $itk_component(searchopts) -side top -fill x
   }

   #  Set the maximum number of objects. Returns 1 if succeeded, -1 if
   #  feature not available (no searches) and 0 if failed (usually sign
   #  that interface is busy creating itself).
   public method set_maxobjs {value} {
      if { [info exists searchopts_] && [winfo exists $searchopts_] } {
         return [$searchopts_ set_maxobjs $value]
      }
      return 0
   }

   #  Redefine search method. Do this so we can inhibit automatic
   #  searching on local catalogues (we need to change the default
   #  behaviour, before doing a search). Also set the origin so that
   #  this works for the first invocation (otherwise region may be
   #  still be image based at this point).
   public method search {args} {
      set_origin
      if { $allow_searches_ } {
         set_wcs_type
         cat::AstroCat::search $args
         restore_equinox_
      }

   }

   #  Restore an old equinox catalogue setting, if needed. This is usually
   #  required when using a catalogue with an explicit system, equinox and
   #  epoch coordinate system. If the equinox is not temporarily set to
   #  J2000 an internal transformation of the catalogue positions will be
   #  made if the equinox is not J2000.
   protected method restore_equinox_ {} {
      if { $oldequinox_ != {} } {
         $w_.cat setequinox $oldequinox_
         set oldequinox_ {}
      }
   }

   #  Set the display area from the image that is displayed.
   public method set_from_image {} {
      if { [info exists searchopts_] && [winfo exists $searchopts_] } {
         $searchopts_ set_from_image
      }
   }

   #  Return the iswcs value of the current catalogue.
   public method iswcs {} {
      return [$w_.cat iswcs]
   }

   #  Return the ispix value of the current catalogue.
   public method ispix {} {
      return [$w_.cat ispix]
   }

   #  Remove the catalogue from the info entry lists.
   public method remove_catalog {} {
      $w_.cat entry remove $itk_option(-catalog)
   }

   #  Procedure to allow the allow_searches variable to be toggled.
   proc allow_searches {flag} {
      set allow_searches_ $flag
   }

   #  Centre the main window on the selected object.
   protected method centre_selected_object_ {} {
      set id [lindex [lindex [$results_ get_selected] 0] [$w_.cat id_col]]
      if {"$id" == "" || $canvas_ == "" } {
         return
      }
      if { [llength [set box [$canvas_ bbox cat$id]]] } {
         lassign $box x0 y0 x1 y1
         set x [expr ($x1+$x0)/2.0]
         set y [expr ($y1+$y0)/2.0]

         set dw [$image_ dispwidth]
         set dh [$image_ dispheight]
         set cw [winfo width $canvas_]
         set ch [winfo height $canvas_]
         if {$cw != 1 && $dw && $dh} {
            $canvas_ xview moveto [expr (($x-$cw/2.0)/$dw)]
            $canvas_ yview moveto [expr (($y-$ch/2.0)/$dh)]
         }
      }
   }

   #  Display any selected rows in a new catalogue window.
   protected method extract_selected_ {} {
      set selected [$results_ get_selected]
      if { $selected == "" } {
         error_dialog "No table rows are selected"
         return
      }

      #  Save these to a temporary catalogue.
      incr count_
      set name [gaia::GaiaTempName::make_name \
                   "GaiaTableExtract" $count_ ".TAB"]
      $results_ save_to_file $name $selected [$results_ get_headings]

      #  And display it.
      new_local_catalog $name $itk_option(-id) gaia::GaiaSearch "catalog" $w_
   }

   #  Save table to a named file.
   public method save_to_file {filename} {
      $results_ save_to_named_file $filename
   }

   #  Override plot to stop use of image WCS system if asked.
   public method plot {} {
      if { ! $itk_option(-plot_wcs) } {
         $image_ configure -plot_wcs 0
      }
      skycat::SkySearch::plot
      if { ! $itk_option(-plot_wcs) } {
         $image_ configure -plot_wcs 1
      }
   }

   #  Override the imgplot method to use AST for aligning the catalogue
   #  positions to the image. In this version we need to pass an AST FrameSet
   #  reference that describes the WCS of the catalogue.
   protected method imgplot_ {equinox} {
      set_wcs_type $equinox
      if {[catch {$w_.cat imgplot $image_ $info_ $astref_ $headings_} msg]} {
         error_dialog $msg
      }
      restore_equinox_
   }

   #  Get a WCS description for the catalogue and set its display format.
   #  Only two allowed, degrees and hms. A side-effect is the updating
   #  of the astref_ frameset.
   public method set_wcs_type { {equinox {}} } {

      #  Release previous version.
      if { $astref_ != 0 } {
         catch {gaiautils::astannul $astref_}
         set astref_ 0
      }

      #  Check catalogue for additional meta-data describing the coordinate
      #  system. This can be either an AST FrameSet from a KAPPA compatible
      #  application, or the description from a VOTable (which will become STC
      #  someday). If neither is available we use the Skycat equinox.
      set comments [$w_.cat comments]
      if { $comments != {} } {
         set astref_ [get_kaplibs_frameset $comments]
         if { $astref_ != 0 } {

            #  KAPLIBS framesets for catalogues contain a WCS that
            #  describes more than two columns (in fact they have an axis per
            #  column). In that case we just want a suitable skyframe, so
            #  search for one, these are usually just the first two axes
            if { [gaiautils::astget $astref_ naxes] > 2 } {
               set skyframe [gaiautils::astskyframe "MaxAxes=20"]
               set skyref [gaiautils::astfindframe $astref_ $skyframe ","]
               if { $skyref != 0 } {
                  catch {gaiautils::astannul $astref_}
                  set astref_ $skyref
                  catch {gaiautils::astannul $skyframe}
               }
            }
         }
      }
      if { $astref_ == 0 } {

         #  Check for the system, equinox and epoch values of the catalogue.
         #  These will be present for formerly VOTables.  Note we need a system
         #  set, this make's sure we do not just use the defaults for
         #  catalogues that do not set these values.
         set att {}
         set system [$w_.cat system]
         if { $system != {} } {
            append att "System=$system,"
            set equinox [$w_.cat equinox]
            if { $equinox != {} } {
               append att "Equinox=$equinox,"
            }
            set epoch [$w_.cat epoch]
            if { $epoch != {} } {
               append att "Epoch=$epoch"
            }
            if { $att != {} } {
               set astref_ [gaiautils::astskyframeset $att]
            }
         }
      }

      #  If AST frameset from the catalogue meta-data is being used, any
      #  equinox values can be misused in the default Skycat behaviour
      #  (where all catalogues are assumed to be in J2000) stop that.
      #  Note use restore_equinox_ to recover after the search/query is
      #  finished. Also the search radius is not going to work if the
      #  catalogue and image have different systems, so disable it.
      set oldequinox_ {}
      if { $astref_ != 0 } {
         $searchopts_ set_equinox {}
         set oldequinox_ [$w_.cat setequinox J2000]
         set qopts [$searchopts_ get_image_center_radius [$w_.cat iswcs]]
         catch {$searchopts_ set_pos_radius [list [lindex $qopts 0] [lindex $qopts 1] {} {}]} msg
      }

      if { $astref_ == 0 } {

         #  Use the equinox, Skycat fashion. Can be realized too quickly
         #  for local catalogues, so fallback to image equinox, which will
         #  be the default anyway.
         if { $equinox == {} } {
            if { [ catch {set equinox [get_equinox]} ] } {
               lassign [$image_ wcscenter] ra dec equinox
            }
         }

         switch -glob $equinox {
            B* {
               set att "System=FK4,Equinox=$equinox"
            }
            J* {
               set att "System=FK5,Equinox=$equinox"
            }
            default {
               if { $equinox != {} } {
                  set att "Equinox=$equinox"
               } else {
                  set att "System=FK5,Equinox=J2000"
               }
            }
         }

         set astref_ [gaiautils::astskyframeset $att]
      }

      #  Check the AST FrameSet to see what type of coordinates are expected.
      #  These are "hms" if an RA axis is found, catch when axes are not even
      #  sky axes.
      set hms 1
      catch {
         set astime [gaiautils::astget $astref_ "astime(1)"]
         if { ! $astime } {
            set astime [gaiautils::astget $astref_ "astime(2)"]
            if { ! $astime } {
               set hms 0
            }
         }
      }

      #  Set display format to hms or degrees as appropriate for the catalogue.
      $w_.cat hms $hms
   }

   #  Return any comments in the current catalogue.
   public method comments {} {
      return [$w_.cat comments]
   }

   #  Set or reset the origin used when plotting positions and
   #  grabbing regions from the image.
   public method set_origin {args} {
      if { $itk_option(-use_origin) } {
         $image_ origin xo yo
         $w_.cat origin [expr $xo-1.5] [expr $yo-1.5]
      } else {
         $w_.cat origin 0.0 0.0
      }
   }

   #  Clear all the objects.
   public method clear_all_objects {} {
      deselect_objects
      delete_objects
   }

   #  Public method to label the selected object (gives access to
   #  protected method in SkySearch).
   public method label_selected_row {} {
      label_selected_object
   }

   #  Label all the objects!
   public method label_all_objects {} {
      set name [$w_.cat shortname $itk_option(-catalog)]
      foreach line [$results_ get_contents] {
         set id [lindex $line [$w_.cat id_col]]
         label_object_in_image $id $name
      }
   }

   #  Insert the id for the given object in the image near the object
   #  and return a string containing status info. Name identifies the
   #  source catalog (short_name). Override to use configured colour.
   public method label_object_in_image {id name} {
      if { $canvas_ == {} } {
         return
      }

      if { [llength [set box [$canvas_ bbox cat$id]]] } {
         lassign $box x0 y0 x1 y1
         make_label $name $id [expr ($x1+$x0)/2.0] [expr ($y1+$y0)/2.0] \
            canvas $id $itk_option(-labelcolour)
         return "labeled object '$id' in image"
      } else {
         return "object '$id' is not visible"
      }
   }

   #  This member procedure is used to open a window for the named
   #  local catalogue, or reuse the existing one for the catalog, if it
   #  is already open. It assumes that the local catalogue exists.
   #
   #    name      is the long name of catalogue
   #
   #    id        is an optional unique id to be associated with a new
   #              catalogue widget.
   #
   #    classname is the name of the AstroCat subclass to use to
   #              create new catalogue widgets (defaults to "AstroCat").
   #
   #    debug     is a flag: if true, run queries in foreground
   #
   #    w         should be the top level window of the caller, if specified
   #
   #    open_cmd  a command to call just after the catalogue is opened
   #              (use callback to apply runtime info modifications).
   public proc new_local_catalog {name {id ""} {classname AstroCat}
                                  {debug 0} {type "catalog"} {w ""}
                                  {open_cmd ""} } {

      #  Check for existing catalogue.
      set i "$name,$id"
      if {[info exists instances_($i)] && [winfo exists $instances_($i)]} {
         utilRaiseWindow $instances_($i)
         if {"[$instances_($i).cat servtype]" == "local"} {

            #  Start a search to update the window.
            $instances_($i) search
         }
         return
      }

      #  If $w was specified, put the window under that top level window
      #  so we get the window numbers right (for cloning, see TopLevelWidget).
      if {[winfo exists $w]} {
         set instname $w.ac[incr n_instances_]
      } else {
         set instname .ac[incr n_instances_]
      }

      #  Create the new window.
      set instances_($i) \
         [$classname $instname \
             -id $id \
             -debug $debug \
             -catalog $name \
             -catalogtype catalog \
             -tcs 0 \
             -transient 0 \
             -center 0\
             -open_cmd $open_cmd]
   }

   public proc apply_history {skycat filename defaultcut } {

      if {"$filename" == "" || [string first /tmp $filename] == 0 \
             || ! [file exists $filename]} {
         # ignore temporary and non-existant files
         return
      }

      set notset 0
      set catalog $history_catalog_
      set image [{*}$skycat get_image]
      if {[catch {$astrocat_ open $catalog}]} {
         # no catalog yet
         set notset 1
      }

      #  Catch problems with corrupted history files.
      set list ""
      catch {
         set list [$astrocat_ query -id [file tail $filename]]
      }
      if {[llength $list] == 0} {
         # not in catalog
         set notset 1
      }

      if { $notset } {
         if { $defaultcut != 100.0 } {
            $image autocut -percent $defaultcut
         }
      } else {
         #  Let SkySearch restore from catalogue.
         skycat::SkySearch::apply_history $skycat $filename
      }
   }

   #  Pop up a dialog to set the plot symbols to use for this catalog.
   #  Override to use GaiaSymbolConfig and offer local symbols.
   public method set_plot_symbols {} {
      set columns $headings_
      if {[llength $columns] == 0} {
         info_dialog "Please make a query first so that the column names are known" $w_
         return
      }
      utilReUseWidget gaia::GaiaSymbolConfig $w_.symconf \
         -catalog $itk_option(-catalog) \
         -astrocat [code $w_.cat] \
         -columns $columns \
         -command [code $this plot]
   }

   #  Ask the user for the name of a local catalog file and then
   #  open a window for the catalog.
   #
   #  Use this instead of the local_catalog proc so that catalogues with HDU
   #  specifications can be handled and MEFs can be browsed HDUs.
   public proc get_local_catalog {id w} {
      set file [get_file_ "." "*" $id $w]
      if { $file != {} } {
         set file [regsub -all {\[} $file "\{"]
         set file [regsub -all {\]} $file "\}"]
         GaiaSearch::browsed_open_ $id $w "table" "$file"
      }
   }

   #  Get a file containing a local catalogue, also provides browsing
   #  FITS MEFS for HDUs.
   protected proc get_file_ {dir pattern id w} {
      set exists [winfo exists .searchselect]
      utilReUseWidget util::FileSelect .searchselect \
         -transient 1 \
         -withdraw 1 \
         -button_4 "Browse" \
         -cmd_4 [code GaiaSearch::browse_file_ $id $w]

      #  If new set once-only values (do not switch directories & filters).
      if { ! $exists } {
         .searchselect configure \
            -dir $dir \
            -filter $pattern \
            -filter_types {{any *} {FIT(.fit) *.fit} {FIT(.fits) *.fits}}
      }

      #  Now a transient of this window, not one that created it.
      wm transient .searchselect [winfo toplevel $w]

      #  Also deiconfy and raise in case previous parent is iconised.
      wm deiconify .searchselect
      raise .searchselect

      if {[.searchselect activate]} {
         return [.searchselect get]
      }
   }

   #  Browse the content of the file selected in the dialog.
   #  Use to look for HDUs.
   protected proc browse_file_ {id w} {

      #  Release the file selection window.
      set file [.searchselect get]
      if { [::file exists $file] && [::file isfile $file] } {
         wm withdraw .searchselect
         utilReUseWidget gaia::GaiaHduBrowser .searchbrowser \
            -file $file \
            -transient 1 \
            -open_cmd [code GaiaSearch::browsed_open_ $id $w] \
            -cancel_cmd [code GaiaSearch::get_local_catalog $id $w]

         #  Now a transient of this window, not one that created it.
         wm transient .searchbrowser [winfo toplevel $w]
      } else {
         #  Ignore, no such file.
         warning_dialog "Not a disk filename ($file)" $w
         get_local_catalog $id $w
      }
   }

   #  Handle an open request from the file browser. Only handle
   #  tables.
   protected proc browsed_open_ {id w type name {naxes 0}} {
      if { $type == "table" } {

         #  Set the catalog config entry from the $catinfo table if this
         #  isn't just a simple filename (note this still may cause problems
         #  updating the catalog info from the headers).
         if { ! [::file exists $name] } {
            if { [catch {$astrocat_ entry get $name}] } {
               if { "[string index $name 0]" != "/"} {
                  set fname "[pwd]/$name"
               } else {
                  set fname "$name"
               }
               $astrocat_ entry add \
                  [list "serv_type local" "long_name $fname" \
                      "short_name $name" "url $fname"]
            }
         }

         #  Display the catalogue.
         gaia::GaiaSearch::new_local_catalog "$name" $id ::gaia::GaiaSearch "catalog" $w
      } else {
         warning_dialog "Not a table ($type)" $w
      }
   }


   #  Populate the Data-Servers menu with the standard items and VO.  Copy of
   #  cat::AstroCat::add_catalog_menu proc so we can add VO items (this proc is
   #  called to completely re-create this menu with new catalogs). See Gaia.tcl
   #  which calls this instead of cat::AstroCat::add_catalog_menu.
   #
   #  w is the caller's toplevel window (an instance of TopLevelWidget).
   #
   #  id is an optional unique id to be associated with a new catalog widget.
   #
   #  classname is the name of the AstroCat subclass to use to create new
   #  catalog widgets (defaults to "AstroCat").
   #
   #  debug is a flag is passed from the command line. If true, queries
   #  are run in the foreground, to make debugging easier.
   public proc add_catalog_menu {w {id ""} {classname AstroCat} {debug 0}} {

      # save this info so we can update the menu automatically later
      set catalog_menu_info_($w) \
         [code cat::AstroCat::add_catalog_menu $w $id $classname $debug]

      # add the menu to the caller's menubar
      set m [$w add_menubutton "Data-Servers" "Display Data Servers menu"]

      $w add_menuitem $m cascade "Catalogs" \
         {Select a catalog from the menu} \
         -menu [menu $m.catalogs]
      fill_catalog_menu $w $id $classname $m.catalogs $debug catalog

      $w add_menuitem $m cascade "Image Servers" \
         {Select an image server from the menu} \
         -menu [menu $m.imagesvr]
      fill_catalog_menu $w $id $classname $m.imagesvr $debug imagesvr

      $w add_menuitem $m cascade "Archives" \
         {Select an archive from the menu} \
         -menu [menu $m.archive]
      fill_catalog_menu $w $id $classname $m.archive $debug archive

      $w add_menuitem $m cascade "Local Catalogs" \
         {Select a local catalog from the menu} \
         -menu [menu $m.local]
      fill_catalog_menu $w $id $classname $m.local $debug local shortname

      $m.local add separator

      $w add_menuitem $m.local command "Load from file..." \
         {Open a local catalog file} \
         -command [code cat::AstroCat::local_catalog $id $classname $debug $w] \
         -accelerator "Control-O"

      # clear the local catalogs (handy when many local catalogs have been
      # opened).
      $w add_menuitem $m.local command "Clear local catalogs" \
         {Clear all local catalogs from the list} \
         -command [code clear_local_catalogs]

      $m add separator

      $w add_menuitem $m command "Browse Catalog Directories..."  \
         "Browse the catalog directory hierarchy to view \
             catalogs or add them to the default list" \
         -command [code cat::AstroCat::catalog_directory $id $classname $debug $w] \
         -accelerator "Control-b"

      #  GAIA-VO items, if no VO services grey out.
      #  =============
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {
         set state normal
      } else {
         set state disabled
      }

      #  Cone search.
      $w add_menuitem $m command "Query a VO catalog server..." \
         {Find VO cone search servers and query for catalogs} \
         -command [code $w vo_find_cone] -state $state

      #  TAP query.
      $w add_menuitem $m command "Query a TAP service..." \
         {Find TAP services and query} \
         -command [code $w vo_find_tap] -state $state

      #  Add SIAP query dialogs
      $w add_menuitem $m command "Query a VO image server..." \
         {Find VO image servers and make requests for images} \
         -command [code $w vo_siap_query 0] -state $state

      $w add_menuitem $m command "Query list of VO image servers..." \
         {Query a list of VO image servers about image they hold on a region of sky} \
         -command [code $w vo_siap_query 1] -state $state



      $w add_menuitem $m command "Reload config file..."  \
         "Reload the default catalog config file after it was edited by hand" \
         -command [code cat::AstroCat::reload_config_file $w] \
         -accelerator "Control-R"

      # if there is a local catalog called "history", add it to the menu also
      if {"[$astrocat_ servtype history]" == "local"} {
         $m add separator
         $w add_menuitem $m command "History..."  \
            "Open an automatically generated catalog of previously viewed images" \
            -command [code open_history_catalog $id $classname 0 $debug $w] \
            -accelerator "Control-h"
      }
   }

   protected proc open_history_catalog {id classname tcs_flag debug w} {
        cat::AstroCat::select_catalog "$history_catalog_" local $id $classname $tcs_flag $debug $w
   }

   #  See if a list of comments extracted from a table contain a KAPPA-like
   #  AST FrameSet for aligning the table with the image. If so return
   #  the FrameSet, otherwise return 0.
   #
   #  KAPLIBS comments (in a tab-table) look like:
   #     '^#[C|T| ]!!.*$'
   #  where .* is the AST information. This is complicated as continuation
   #  lines are also present. These look like:
   #     '^#[C|T| ]!!\+.*$'
   #
   public proc get_kaplibs_frameset {comments} {

      set object {}
      set value {}
      foreach line [split $comments "\n"] {
         if { [regexp {\#?!!\+(.*)} $line -> ast] } {
            append value $ast
         } elseif { [regexp {\#?!!(.*)} $line -> ast] } {
            if { $value != {} } {
               append object "$value\n"
            }
            set value $ast
         }
      }
      if { $value != {} } {
         append object "$value\n"
      }

      if { $object != {} } {
         return [gaiautils::astcreate native $object]
      }
      return 0
   }

   #  Called when the "Filter" button is pressed. The usual behaviour for
   #  that function is to remove any undrawn objects from the list (the
   #  wording implies that generally includes off-image objects, but that
   #  isn't true, or has become untrue at some point), so if all objects
   #  are retained (i.e. drawn) we try to honour that here. To do that we
   #  need image positions for all rows, not the world coordinates, so we
   #  get the image extent in canvas coordinates and check each objects
   #  canvas coordinates against that.
   public method filter_query_results {} {
      $w_.progress config -text "Filtering out off-image objects..."
      set new_info {}
      set n 0
      busy {
         foreach row $info_ {
            set id [lindex $row [$w_.cat id_col]]
            if {[llength [$canvas_ find withtag cat$id]]} {
               lappend new_info $row
               incr n
            }
         }
         set t [$results_ total_rows]
         if {$n != $t} {
            $results_ config \
               -info [set info_ $new_info] \
               -title "Search Results ($n*)"
            plot
            $w_.progress config -text \
               "Removed [expr $t-$n] objects from the list."
         } else {

            #  No undrawn objects, so remove any off image ones.
            #  Get position of image in canvas coordinates.
            $image_ convert coords 1 1 image cx0 cy0 canvas
            $image_ convert coords [$image_ width] [$image_ height] image \
               cx1 cy1 canvas

            set x0 [expr int(min($cx0,$cx1))]
            set y0 [expr int(min($cy0,$cy1))]
            set x1 [expr int(max($cx0,$cx1))]
            set y1 [expr int(max($cy0,$cy1))]

            #  And check this against the drawn graphics coordinates.
            set new_info {}
            set n 0
            foreach row $info_ {
               set id [lindex $row [$w_.cat id_col]]
               lassign [$canvas_ coords cat$id] cx cy
               if { $cx > $x0 && $cx < $x1 && $cy > $y0 && $cy < $y1 } {
                  lappend new_info $row
                  incr n
               }
            }
            if {$n != $t} {
               $results_ config \
                  -info [set info_ $new_info] \
                  -title "Search Results ($n*)"
               plot
               $w_.progress config -text \
                  "Removed [expr $t-$n] objects from the list."
            } else {
               $w_.progress config -text "No change."
            }
         }
      }
   }

   #  Set the location of the history file.
   public proc set_history_catalog {catalog} {
      set history_catalog_ "$catalog"
   }

   #  Configuration options (public variables):
   #  =========================================

   #  Whether to disable use of image WCS when scaling / orienting
   #  symbols (this can speed up plotting a lot), but you need X and Y
   #  positions in the catalogue.
   itk_option define -plot_wcs plot_wcs Plot_Wcs 1 {}

   #  Set whether to correct the any NDF origin information when
   #  plotting. This allows X and Y coordinates which are displayed in
   #  NDF pixel coordinates to be plotted correctly.
   itk_option define -use_origin use_origin Use_Origin 0

   #  A command that will be called just after the catalogue is opened.
   itk_option define -open_cmd open_cmd Open_Cmd {}

   #  Possible fonts for drawing labels.
   itk_option define -fonts fonts Fonts {
      -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-240-*-*-*-*-*-*
   }

   #  Colour for labelling objects.
   itk_option define -labelcolour labelcolour LabelColour white

   #  Possible colours for labelling objects.
   itk_option define -colors colors Colors {
      white
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
      black
      red green blue cyan magenta yellow
   }

   #  Protected variables: (available to instance):
   #  =============================================

   #  Reference to the AST frameset describing the catalogue coordinates.
   protected variable astref_ 0

   protected variable oldequinox_ {}

   #  Common variables (shared between all instances):
   #  ================================================

   #  Allow/disallow searches (temporarily).
   common allow_searches_ 1

   #  Reference counter for creating unique files.
   common count_ 0
}
