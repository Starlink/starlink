#+
#  Name:
#     GaiaHduChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends SkyCatHduChooser to add features required for GAIA.

#  Description:

#     This class extends SkyCatHduChooser so that GAIA specific features can
#     be added. At present this amounts to changing the way that FITS tables
#     are accessed, so that we can apply heuristics to catalogue configuration
#     (pick up RA and DEC columns, reconstruct the symbol etc). This is done
#     by using the GaiaConvertTable conversions (which also deal with CAT &
#     ASCII formats). We also intercept any binary tables that have
#     "COMPRESSED_IMAGE" in their EXTNAME and convert these into temporary
#     FITS image files.
#
#     Later addition. Check for cubes and display them appropriately.

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
#     Copyright (C) 2007 Science and Technology Facilities Council.
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
#     27-JAN_2000 (PWD):
#        Original version.
#     03-MAY-2005 (PWD):
#        Added decompression of inline images.
#     21-MAY-2007 (PWD):
#        Stop using CAT, effectively same class but uses new GaiaConvertTable.
#        Stop using imcopy to access compressed images.
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

   #  Display the current FITS table, which may include a compressed image.
   protected method display_fits_table {name hdu} {

      #  Get name of FITS file.
      set file [$image_ cget -file]

      #  May be a compressed image masquerading as a table. Check for that.
      if { [string first "COMPRESSED_IMAGE" $name] > -1 } {

         #  Arrange for decompression of this extension and then load it.
         decompress_inline_ $file $hdu
         return
      }

      #  Construct qualified table name for use by GaiaConvertTable (used be
      #  be accessed as a CAT table, but now uses native access, so hdu not
      #  decremented). Note catalogue HDU should always be 2 or greater.
      if { $hdu > 1 } {
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

   #  Decompress an image stored in an extension (RICE format for instance)
   #  and display it. Will reuse a decompressed extension, if already
   #  processed. Decompression is supported by the StarFITSIO write method, so
   #  we just move to the HDU and save it to disk.
   protected method decompress_inline_ {file hdu} {

      #  If this extension is already done, and the file still exists,
      #  reuse it.
      if { [info exists tempfiles_($file,$hdu)] &&
           [file exists $tempfiles_($file,$hdu)] } {
         set converted_file $tempfiles_($file,$hdu)
      } else {

         #  Perform the conversion.
         incr count_
         set converted_file [gaia::GaiaTempName::make_name \
                                "GaiaHduImg" $count_ ".fits"]
         if { [file exists $converted_file] } {
            file delete -force $converted_file
         }

         #  Write the HDU out to disk.
         if { [catch {$image_ hdu get $hdu $converted_file} msg] } {
            error "Failed to decompress image to file $converted_file \
                   (no write permission?) \n$msg"
         } else {
            #  Success so cache for next time.
            set tempfiles_($file,$hdu) $converted_file
         }
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
