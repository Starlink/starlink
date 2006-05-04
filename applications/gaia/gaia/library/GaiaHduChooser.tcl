#+
#  Name:
#     GaiaHduChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends SkyCatHduChooser to add features required for GAIA.

#  Description: 
#     This class extends SkyCatHduChooser so that GAIA specific
#     features can be added. At present this amounts to changing the
#     way that FITS tables are accessed, so that we can use the GAIA
#     filters which have specific knowledge about CURSA formats.
#     We also intercept any binary tables that are of type "COMPRESSED_IMAGE"
#     and convert these into temporary FITS image files using the CFITSIO
#     example program "imcopy".

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

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-JAN_2000 (PWD):
#        Original version.
#     03-MAY-2005 (PWD):
#        Added decompression of inline images.
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
      
      #  May be a compressed image masquerading as a table. Check for that.
      if { "$name" == "COMPRESSED_IMAGE" } {

         #  Arrange for decompression of this extension and then load it.
         decompress_inline_ $file [expr $hdu-1]
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

   #  Decompress an image stored in an extension (usually RICE format) and
   #  display it. Will reuse a decompressed extension, if already done.
   protected method decompress_inline_ {file hdu} {

      #  Create the task that does the conversion.
      if { $imcopy_ == {} } { 
         global gaia_dir
         set imcopy_ [GaiaForeignExec \#auto \
                         -application $gaia_dir/imcopy \
                         -show_output 0]
      }

      #  If this extension is already done, and the file still exists,
      #  reuse it.
      if { [info exists tempfiles_($file,$hdu)] && 
           [file exists $tempfiles_($file,$hdu)] } {
         set converted_file $tempfiles_($file,$hdu)
      } else {

         #  Perform the conversion.
         incr count_
         set converted_file "GaiaHduImg${count_}.fits"
         if { [file exists $converted_file] } {
            file delete -force $converted_file
         }
         catch {
            $imcopy_ runwith $file\[$hdu\] $converted_file
         } msg
         if { $msg != "" } { 
            error_dialog "$msg"
            return
         }

         #  Success so cache for next time.
         set tempfiles_($file,$hdu) $converted_file
      }

      #  Display the image. Note this is not marked temporary as we reuse
      #  these files and they do not represent a processed result of any kind.
      $itk_option(-image) configure -file $converted_file
   }

   #  Remove all the temporary files we have created. Needs to be called
   #  sometime before the application exits. Not done by the destructor as 
   #  that would stop any chance of caching and reusing converted files.
   public proc release_temporary_files {} {
      if { [info exists tempfiles_] } {
         foreach f [array names tempfiles_] {
            if { [file exists $tempfiles_($f)] } {
               file delete -force $tempfiles_($f)
               set tempfiles_($f) {}
            }
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------


   #  Protected variables: (available to instance)
   #  --------------------

   #  The application used to do the conversion of a compressed extension.
   protected variable imcopy_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Instances of this class.
   common instances_ 0

   #  Counter for temporary names.
   common count_ 0

   #  Array of temporary files. These are indexed by the filename and
   #  extension and may be reused. Shared by all instances.
   common tempfiles_

#  End of class definition.
}
