#+
#  Name:
#     GaiaHduChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends SkyCatHduChooser to add features required for GAIA.

#  Description: 
#     This class extends SkyCatHduChooser so that GAIA specific
#     features can be added. At present this just amounts to changing the
#     way that FITS tables are accessed, so that we can use the GAIA
#     filters which have specific knowledge about CURSA formats.

#  Invocations:
#
#        GaiaHduChooser object_name [configuration options]
#
#     This creates an instance of a GaiaHduChooser object. The return is
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
#     See method definitions below.

#  Inheritance:
#     skycat::SkyCatHduChooser

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-JAN_2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaHduChooser {}

itcl::class gaia::GaiaHduChooser {

   #  Inheritances:
   #  -------------
   inherit skycat::SkyCatHduChooser

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Display the current FITS table, override to use GAIA methods
   #  which are based on external conversion filters.
   protected method display_fits_table {name hdu} {
      
      #  Get name of FITS file.
      set file [$image_ cget -file]
      
      #  May be an image masquerading as a table. Check for that.
      if { "$name" == "COMPRESSED_IMAGE" } {
         error_dialog "No support for in-line compressed images"
         return
      }

      #  Construct CURSA name.
      incr hdu -1
      if { $hdu > 0 } { 
         set catalogue "$file\{$hdu\}"
      } else {
         set catalogue "$file"
      }
      
      #  Set the catalog config entry from the $catinfo table
      if { [catch "$astrocat_ entry get $catalogue"] } {
         set fname [full_name_ $catalogue]
         $astrocat_ entry add \
            [list "serv_type local" "long_name $fname" "short_name \
                  $catalogue" "url $fname"]
      }
      
      #  Display the catalogue.
      gaia::GaiaSearch::new_local_catalog $catalogue $itk_option(-image) \
         ::gaia::GaiaSearch
   }
   
   #  Expand name to full path relative to current directory.
   protected method full_name_ {name} {
      if { "[string index $name 0]" != "/"} {
         set fname [pwd]/$name
      } else {
         set fname $name
      }
      return $fname
   }

   #  Configuration options: (public variables)
   #  ----------------------


   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
