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
#     NDF slices and possibly HDS component paths.
#
#     So the sort of names that we might get for images are:
#
#        file.fits         FITS may also be .fit .fits.gz .fits.Z etc.
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
#     rtdimage), slices, paths and disk file names

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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-AUG-1999 (PDRAPER):
#        Original version.
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

   #  Get the GAIA compliant fully expanded name.
   public method fullname {} {
      return $fullname_
   }

   #  Get the diskfile name.
   public method diskfile {} {
      return $diskfile_
   }

   #  Get the NDF slice.
   public method slice {} {
      return $slice_
   }

   #  Get the HDS path.
   public method path {} {
      return $path_
   }

   #  Get the diskfile type.
   public method type {} {
      return $type_
   }

   #  Get the filename as used by Starlink applications. This is the
   #  fullname without the ".sdf".
   public method ndfname {} {
      if { $type_ == ".sdf" } { 
	 set i1 [string first {.sdf} $fullname_]
	 if { $i1 > -1 } { 
	    incr i1 -1
	    if { [regsub {.sdf} $fullname_ "" name] } { 
	       return $name
	    }
	 }
      } 
      return $fullname_
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

   #  Parse the imagename, obtaining the fully expanded name, the
   #  diskfile name and any NDF extended parts.
   protected method parse_name_ {} {
      reset_
      get_slice_
      get_type_
      if { ! [check_type_] } {
	 get_path_
      }
      get_diskfile_
      get_fullname_
   }

   #  Get any slice information from the image name.
   protected method get_slice_ {} {
      set i1 [string last {(} $imagename]
      set i2  [string last {)} $imagename]
      if { $i1 > -1 && $i2 > -1 } {
	 set slice_ [string range $imagename $i1 $i2]
      } else {
	 set slice_ ""
      }
   }

   #  Get the file type. This is the string after the first "."
   #  in the string after the last directory separator. If no type is
   #  given then it defaults to ".sdf".
   protected method get_type_ {} {
      set tail [file tail $imagename]
      set i1 [string first {.} $tail]
      if { $i1 > -1 } {
	 set type_ [string range $tail $i1 end]
      } else {
	 set type_ ".sdf"
      }
   }

   #  Check if the file type is known to the NDF system, or is a FITS
   #  description.
   protected method check_type_ {} {
      if { [string match ".fit*" $type_] ||
	   [string match ".FIT*" $type_] ||
	   [string match ".sdf" $type_] } {
	 return 1
      }

      #  If the file type matches a string in NDF_FORMATS_IN, that's great.
      global env
      if { [info exists env(NDF_FORMATS_IN)] } {
	 if { [string first $type_ $env(NDF_FORMATS_IN)] > -1 } {
	    return 1
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
      set fullname_ "$diskfile_$path_$slice_"
   }

   #  Get the path component from the name. Assumes a check_type_ has
   #  been failed, so missing type information implies an NDF and the
   #  current potential path is stored in "type_".
   protected method get_path_ {} {
      set i1 [string first {.sdf} $type_]
      if { $i1 > -1 } {
	 set i1 [expr $i1+4]
	 if { $slice_ != {} } {
	    set i2 [expr [string first $slice_ $type_]-1]
	 } else {
	    set i2 end
	 }
	 set path_ [string range $type_ $i1 $i2]
	 
      } else {

	 #  No ".sdf", so assume what looks like a file extension is a 
	 #  path.
	 if { $slice_ != {} } {
	    set i2 [expr [string first $slice_ $type_]-1]
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

   #  HDS path
   protected variable path_ {}

   #  Disk file type.
   protected variable type_ {.sdf}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
