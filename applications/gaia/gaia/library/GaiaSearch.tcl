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
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-SEP-1998 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSearch {}

#  This class extends the SkySearch catalog widget browser class (see
#  SkySearch (n) and AstroCat(n) to add support for plotting objects
#  and displaying images.

class gaia::GaiaSearch {
   inherit skycat::SkySearch

   #  Constructor.
   constructor {args} {
      set tag_ {};               #  Not sure why this is needed.
      eval itk_initialize $args
   }
   
   #  Init method, called after the options have been
   #  evaluated. Invoke the SkySearch::init method, but catch any
   #  errors so we can destroy this object (this happens if the foreign
   #  catalogue conversion process fails. Also remove <Enter> binding
   #  as this slows down the zoom window a lot.
   public method init {} {
      SkySearch::init
      $canvas_ bind $object_tag_  <Any-Enter> {}
      $canvas_ bind $object_tag_  <Any-Leave> {}

      #  Add menu option to centre on selected row. Only add these
      #  options for catalogue windows, not image servers.
      if { $iscat_ } {
         set m [get_menu Options]
         $m add separator
         add_menuitem $m command "Center on selected row" \
            {Centre main image on selected object (also bound to {bitmap b2} in table)} \
            -command [code $this centre_selected_object_]

      #  Add option for a some help on this window (not image servers).
         global env
         set m [add_help_button $env(GAIA_DIR)/Catalogue.hlp "Catalogues Overview..." \
                   {General information about catalogues}   ]
         set m [add_help_button $env(GAIA_DIR)/GaiaSearch.hlp "On Window..." \
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

   #  Set the maximum number of objects.
   public method set_maxobjs {value} {
      if { [info exists searchopts_] && [winfo exists $searchopts_] } {
         return [$searchopts_ set_maxobjs $value]
      }
      return 0
   }

   #  Redefine search method. Do this so we can inhibit automatic
   #  searching on local catalogues (we need to change the default
   #  behaviour, before doing a search).
   public method search {args} {
      if { $allow_searches_ } {
         AstroCat::search $args
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

   #  Configuration options (public variables):
   #  =========================================

   #  Whether to disable use of image WCS when scaling / orienting
   #  symbols (this can speed up plotting a lot), but you need X and Y 
   #  positions in the catalogue.
   itk_option define -plot_wcs plot_wcs Plot_Wcs 1 {}

   #  Common variables (shared between all instances):
   #  ================================================

   #  Allow/disallow searches (temporarily).
   common allow_searches_ 1
}
