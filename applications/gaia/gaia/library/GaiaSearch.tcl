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
#     Copyright (C) 1998-2001 Central Laboratory of the Research Councils

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
   constructor {args} {
      set tag_ {};               #  Not sure why this is needed.
      eval itk_initialize $args
   }
   
   #  Init method, called after the options have been evaluated.
   public method init {} {

      #  Extra GAIA symbols for catalogues.
      #set symbols_("rotbox") 1; TODO: add changes needed to support this.
      SkySearch::init
      
      #  Remove the "Save with image" menu option. This is not
      #  available.
      if { $iscat_ } { 
	 set m [get_menu File] 
	 $m delete "Save with image" 
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

         #  Add labels to all objects.
         add_menuitem $m command "Label all objects" \
            {Label all objects displayed on image (same as double clicking on all rows)} \
            -command [code $this label_all_objects]

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
         set results_ [GaiaQueryResult $w_.results \
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

   #  Method to set the symbol used when plotting. This is the same
   #  format as a line in a local catalogue.
   public method set_symbol {args} {
      $w_.cat symbol $args
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
         set searchopts_ [GaiaQuery $w_.searchopts \
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
         AstroCat::search $args
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
      set name "GaiaTableExtract[incr count_].TAB"
      $results_ save_to_file $name $selected [$results_ get_headings]

      #  And display it.
      new_local_catalog $name $itk_option(-id) gaia::GaiaSearch
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
      SkySearch::plot
      if { ! $itk_option(-plot_wcs) } {
         $image_ configure -plot_wcs 1
      }
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
   public proc new_local_catalog {name {id ""} {classname AstroCat}
                                  {debug 0} {type "catalog"} {w ""}} { 

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
             -center 0]
   }

   public proc apply_history {skycat filename defaultcut } {

      if {"$filename" == "" || [string first /tmp $filename] == 0 \
             || ! [file exists $filename]} {
         # ignore temporary and non-existant files
         return
      }

      set notset 0
      set catalog $history_catalog_
      set image [$skycat get_image]
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
         SkySearch::apply_history $skycat $filename
      }
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

   #  Common variables (shared between all instances):
   #  ================================================

   #  Allow/disallow searches (temporarily).
   common allow_searches_ 1

   #  Reference counter for creating unique files.
   common count_ 0
}
