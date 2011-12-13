#+
#  Name:
#     GaiaImageName

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for encapsulating GAIA image names.

#  Description:
#     This class provides objects which have a consistent set of
#     methods for querying and processing image names as understood by
#     GAIA.
#
#     These names may be simple disk file names, such as the top-level
#     containers of NDFs and FITS, or more complex ones that include
#     NDF slices, FITS extensions and possibly HDS component paths.
#
#     So the sort of names that we might get for images are:
#
#        file.fits         FITS may also be .fit .fits.gz .fits.Z etc.
#
#        file.fits[1]      first FITS extension.
#
#        file              an NDF, which is expanded to file.sdf
#
#        file.imh          IRAF file to be processed by NDF, other
#                          types are list in env(NDF_FORMATS_IN)
#
#        file(1:100,2:200) NDF with slice, expanded to file.sdf(1:100,2:200)
#
#        file.sdf(1:100,2:200) same as above
#
#        file.sdf          simple NDF
#
#        file.ndf          NDF stored in a container file, expanded to
#                          file.sdf.ndf
#
#        file.sdf.ndf(1:100,2:100) Slice of an NDF stored in a
#                                  container file. This name is fully
#                                  expanded.
#
#     To use this class create an object with the imagename, then use
#     its methods to get fully qualified names (which are understood by
#     rtdimage), slices, paths and disk file names. ADAM tasks should
#     use the "ndfname" method.

#  Invocations:
#
#        GaiaImageName object_name [configuration options]
#
#     This creates an instance of a GaiaImageName object. The return is
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
#      See public variable descriptions below.

#  Methods:
#      See method descriptions.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
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
#     26-AUG-1999 (PWD):
#        Original version.
#     14-JUL-2000 (PWD):
#        Added support for FITS extensions ([1] etc.).
#     08-MAR-2001 (PWD):
#        Added name support for FITS files with multiple periods in
#        their names.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaImageName {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Get the GAIA compliant fully expanded name. Do not add the FITS
   #  extension, unless it is needed (i.e. we have a slice) when
   #  requested.
   public method fullname { {fitsext 1} } {
      if { $fitsext || $fitsext_ == {} || $slice_ != {} } {
         return $fullname_
      } else {
         return "$diskfile_$path_$slice_"
      }
   }

   #  Get the diskfile name.
   public method diskfile {} {
      return $diskfile_
   }

   #  Get the NDF slice.
   public method slice {} {
      return $slice_
   }

   #  Get the FITS extension.
   public method fitsext {} {
      return $fitsext_
   }

   #  Get the FITS extension number. Note this is 0, if an NDF slice
   #  is available.
   public method fitshdunum {} {
      if { $slice_ == {} } {
         return $fitshdu_
      } else {
         return 0
      }
   }

   #  Set the FITS extension number, ignored for NDFs. Note invalidates
   #  any previous checks and reformats filename.
   public method setfitshdunum {hdu} {
      if { $slice_ == {} } {
         set fitshdu_ $hdu
         set fitsext_ "\[$hdu\]"
         get_fullname_
      }
   }

   #  Get the HDS path.
   public method path {} {
      return $path_
   }

   #  Set the HDS path. Note invalidates any previous checks and reformats filename.
   public method setpath {path} {
      set path_ $path
      get_fullname_
   }

   #  Get the diskfile type.
   public method type {} {
      return $type_
   }

   #  Get the filename as used by Starlink applications. This is the
   #  fullname without the ".sdf". Note we need single quotes to get
   #  any FITS extensions out to ADAM tasks. Also FITS files with
   #  extensions use a different numbering scheme. If protect is false
   #  then foreign names are not protected with single quotes.
   public method ndfname {{protect 1}} {
      if { $type_ == ".sdf" } {
	 set i1 [string first {.sdf} $fullname_]
	 if { $i1 > -1 } {
	    incr i1 -1
	    if { [regsub {\.sdf} $fullname_ "" name] } {
	       return $name
	    }
	 }
      }

      #  Foreign format. If this is FITS with extension then need to
      #  decrement the extension number (from Skycat HDU count). Always
      #  return these in single quotes to protect special characters.
      if { $fitshdu_ != 0 } {
         set extnum [expr $fitshdu_ -1]
         if { $protect } {
            return "'$diskfile_\[$extnum\]$slice_'"
         } else {
            return "$diskfile_\[$extnum\]$slice_"
         }
      }
      if { $protect } {
         return "'$fullname_'"
      }
      return "$fullname_"
   }


   #  Create a modified name by either prefixing or postfixing a given
   #  string to the name. This is equivalent to ndfname, not fullname.
   public method modname {prefix value} {
      set index [string first $type_ $diskfile_]
      incr index -1
      set shortname [string range $diskfile_ 0 $index]
      if { $prefix } {
         set dirname [file dirname $shortname]
         set tail [file tail $shortname]
         set shortname [file join $dirname "${value}${tail}"]
      } else {
         set shortname "${shortname}${value}"
      }
      set olddiskfile $diskfile_
      set diskfile_ ${shortname}${type_}
      get_fullname_

      set result [ndfname 0]

      set diskfile_ $olddiskfile
      get_fullname_

      return $result
   }


   #  Check if diskfile exists, is readable and a plain file.
   public method exists {} {
      if { [file readable $diskfile_] && [file isfile $diskfile_] } {
	 return 1
      } else {
	 return 0
      }
   }

   #  Make name absolute (i.e. start with leading "/"). Note slight
   #  hack to remove /tmp_mnt from NFS names.
   public method absolute {} {
      if { ! [string match {/*} $imagename] } {
	 if { ! [catch {set here [pwd]}] } {
            if { [string range $here 0 7] == "/tmp_mnt" } {
               set here [string range $here 8 end]
            }
	    set imagename "$here/$imagename"
	    parse_name_
	 }
      }
   }

   #  If requested then protect the FITS extension specifier from
   #  expansion as a command. Note the ndfname method may fail after
   #  using this.
   public method protect {} {
      if { $fitsext_ != {} } {
         regsub {\[} $fitsext_ {\[} quoted
         regsub {\]} $quoted {\]} fitsext_

         #  Reconstruct related variables.
         get_fullname_
      }
   }

   #  Parse the imagename, obtaining the fully expanded name, the
   #  diskfile name and any NDF extended parts.
   protected method parse_name_ {} {
      reset_
      get_slice_
      get_fitsext_
      get_type_
      if { ! [check_type_] } {
	 get_path_
      }
      get_diskfile_
      get_fullname_
   }

   #  Get any slice information from the image name.
   protected method get_slice_ {} {
      set i1 [string last "\(" $imagename]
      set i2 [string last "\)" $imagename]

      set slice_ ""
      if { $i1 > -1 && $i2 > -1 } {
         #  The closing ) must be the last character, if this is a slice
         #  (otherwise it will be an HDS structure index).
         if { [string index $imagename end] == "\)" } {
            set slice_ [string range $imagename $i1 $i2]
         }
      }
   }

   #  Get any FITS extension information from the image name. Extract
   #  the extension number as we need to decrement this for FITS files
   #  that are to be processed by CONVERT & NDF (these use 1 for the
   #  first extension, not 2 as in Skycat).
   protected method get_fitsext_ {} {
      set i1 [string last {[} $imagename]
      set i2  [string last {]} $imagename]
      if { $i1 > -1 && $i2 > -1 } {
	 set fitsext_ [string range $imagename $i1 $i2]
         set fitshdu_ [string range $imagename [incr i1] [incr i2 -1]]
      } else {
	 set fitsext_ ""
      }
   }

   #  Get the file type. This is the string (minus any FITS extension
   #  or NDF slice) after the first "."  in the string after the last
   #  directory separator. If no type is given then it defaults to
   #  ".sdf". A special case is files that have multiple periods in
   #  their names, but refer to an actual disk file.
   protected method get_type_ {} {

      #  Remove the FITS extension or slice and check for file existence.
      set i1 "end"
      if { $fitsext_ != {} } {
         set i1 [expr [string first $fitsext_ $imagename] -1]
         set type_ [string range $imagename $i1 end]
      } elseif { $slice_ != {} } {
         set i1 [expr [string first $slice_ $imagename] -1]
         set type_ [string range $imagename $i1 end]
      }
      set diskname [string range $imagename 0 $i1]
      if {  [ file exists $diskname ] && [ file isfile $diskname ] } {
         set type_ [file extension $diskname]
         if { ! [check_type_] } {
            #  Use type as after first period, not last.
            set tail [file tail $imagename]
            set i1 [string first {.} $tail]
            set type_ [string range $tail $i1 end]
            if { $type_ == $tail } {
               #  No type, but file exists, pick another with .sdf extension
               #  if that exists.
               if { [file exists ${diskname}.sdf] } {
                  set type_ ".sdf"
               }
            }
         }
      } else {
         set tail [file tail $diskname]
         set i1 [string first {.} $tail]
         if { $i1 > -1 } {
            set type_ [string range $tail $i1 end]
         } else {
            set type_ ".sdf"
         }
      }
   }

   #  Check if the file type is known to the NDF system, or is a FITS
   #  description.
   protected method check_type_ {} {
      if { [string match ".fit*" $type_] ||
	   [string match ".fts*" $type_] ||
	   [string match ".FIT*" $type_] ||
	   [string match ".sdf" $type_] } {
	 return 1
      }

      #  If the file type matches a string in NDF_FORMATS_IN we accept
      #  that. Take care to pick out the full type "($type_)", could match a
      #  part of it, if just looked for "$type_"
      global env
      if { [info exists env(NDF_FORMATS_IN)] } {
         if { [regexp "\\(${type_}\\)" $env(NDF_FORMATS_IN) m1] } {
            if { [info exists m1] && "$m1" == "(${type_})" } {
               return 1
            }
	 }
      }
      return 0
   }

   #  Construct name of diskfile. Assumes type_ already set.
   protected method get_diskfile_ {} {
      set i1 [string first $type_ $imagename]
      if { $i1 > -1 } {
	 incr i1 -1
	 set diskfile_ "[string range $imagename 0 $i1]$type_"
      } else {
	 #  Type not in imagename, so fallback to path_.
	 set i1 [string first $path_ $imagename]
	 if { $i1 > -1 } {
	    incr i1 -1
	    set diskfile_ "[string range $imagename 0 $i1]$type_"
	 } else {

	    #  No type or path, so name must be complete, just remove
	    #  slice.
	    if { $slice_ != {} } {
	       set i2 [expr [string first $slice_ $imagename]-1]
	    } else {
	       set i2 end
	    }
	    set diskfile_ "[string range $imagename 0 $i2]$type_"
	 }
      }
   }

   #  Construct the full name from the various parts.
   protected method get_fullname_ {} {
      set fullname_ "$diskfile_$path_$fitsext_$slice_"
   }

   #  Get the path component from the name. Assumes a check_type_ has
   #  been failed, so missing type information implies an NDF and the
   #  current potential path is stored in "type_".
   protected method get_path_ {} {
      set i1 [string first {.sdf} $type_]
      if { $i1 > -1 } {
	 set i1 [expr $i1+4]
	 if { $slice_ != {} } {
	    set i2 [string first $slice_ $type_]
            if { $i2 > -1 } {
               incr i2 -1
            } else {
               set i2 end
            }
	 } else {
	    set i2 end
	 }
	 set path_ [string range $type_ $i1 $i2]
      } else {
	 #  No ".sdf", so assume what looks like a file extension is a
	 #  path.
	 if { $slice_ != {} } {
	    set i2 [expr [string first $slice_ $type_]-1]
            if { $i2 < 0 } {
               set i2 end
            }
	 } else {
	    set i2 end
	 }
	 set path_ [string range $type_ 0 $i2]
      }
      set type_ ".sdf"
   }

   #  Reset internal configuration (when new name supplied).
   protected method reset_ {} {
      set fullname_ {}
      set diskfile_ {}
      set slice_ {}
      set fitsext_ {}
      set fitshdu_ 0
      set path_ {}
      set type_ {.sdf}
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the image as supplied by the user.
   public variable imagename {} {
      if { $imagename != {} } {
	 parse_name_
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Fully expanded name.
   protected variable fullname_ {}

   #  Disk file name
   protected variable diskfile_ {}

   #  NDF slice.
   protected variable slice_ {}

   #  FITS extension specification ([int]).
   protected variable fitsext_ {}

   #  HDU number of FITS extension.
   protected variable fitshdu_ 0

   #  HDS path
   protected variable path_ {}

   #  Disk file type.
   protected variable type_ {.sdf}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
