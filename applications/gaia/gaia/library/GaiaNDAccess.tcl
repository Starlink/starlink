#+
#  Name:
#     GaiaNDAccess

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Handles multidimensional dataset access in GAIA.

#  Description:
#     This class is designed to handle the access and description of datasets
#     that don't necessarily have 2 dimensions. Superficially it is datatype
#     independent, supporting access to NDF and FITS data, relying on the
#     ndf::, fits:: and array:: Tcl commands. Sections of the data can be
#     directly extracted from cubes for passing into GAIA/Skycat for display
#     as images and spectra.

#  Invocations:
#
#        GaiaNDAccess object_name [configuration options]
#
#     This creates an instance of a GaiaNDAccess object. The return is
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
#     See below.

#  Methods:
#     See below.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
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
#     PWD: Peter Draper (JAC - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-MAR-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaNDAccess {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------

   #  One argument, the specification of the dataset. NDF or FITS file.
   constructor { args } {

      #  Create object for parsing image names.
      set namer_ [GaiaImageName \#auto]

      #  Evaluate any options, should be the dataset name usually.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      close
   }

   #  Methods:
   #  --------

   #  Parse specification to determine data type and get an access name.
   protected method parse_name_ {} {

      #  Release previous dataset, if any.
      close

      $namer_ configure -imagename $dataset
      if { "[$namer_ type]" == ".sdf" } {
         set type_ "ndf"
         set cnfmap_ 1
      } else {
         set type_ "fits"
         set cnfmap_ 1 ;# No harm. Needed for mapped memory inserted into NDF.
      }

      #  Open the dataset.
      open_
   }

   #  Open the dataset. Wraps two methods, one for NDFs and one for FITS files.
   #  These should be light-weight accesses that just get meta-data at this
   #  stage.
   protected method open_ {} {
      set handle_ [${type_}::open [$namer_ ndfname 0]]
   }

   #  Close the dataset, if open.
   public method close {} {
      if { $handle_ != {} } {
         ${type_}::close $handle_
         set handle_ {}
         set addr_ 0
         set cnfmap_ 0
         set dims_ {}
         set bounds_ {}
      }
   }

   #  Acquire an already opened NDF by name and "handle" (NDF identifier).
   public method acquire {name handle} {
      set handle_ $handle
      $namer_ configure -imagename $name
      set dataset $name
      set type_ "ndf"
      set cnfmap_ 1
   }

   #  Get the dimensions of the full data. Returns a list of integers.
   #  If trunc is true then any trailing redundant axes are trimmed.
   public method getdims {trunc} {
      if { $dims_ == {} } {
         set dims_ [${type_}::getdims $handle_ $trunc]
      }
      return $dims_
   }

   #  Get the pixel ranges/bounds of the full data. Returns pairs of
   #  integers, one for each dimension. For FITS files the lower bound will
   #  always be 1. If trunc is true then any trailing redundant axes are
   #  trimmed.
   public method getbounds {trunc} {
      if { $bounds_ == {} } {
         set bounds_ [${type_}::getbounds $handle_ $trunc]
      }
      return $bounds_
   }

   #  Return the coordinate of a position along a given axis.
   #
   #  The arguments are the index of the axis, a list of all the pixel indices
   #  needed to identify the coordinate, and an optional boolean argument that
   #  determines whether to format the value (using astFormat) and if to add
   #  trailing label and units strings.
   public method getcoord {axis indices {formatted 1} {trail 0} } {
      return [${type_}::getcoord $handle_ $axis $indices $formatted $trail]
   }

   #  Map in the dataset "data component". Returns a structure that can be
   #  queried using the getinfo method. The mapping uses mmap, if possible and
   #  currently the default, and the given access mode,  one of "READ",
   #  "UPDATE" or "WRITE". Clearly this must match what access the file
   #  supports.
   public method map { {access "READ"} } {
      if { $addr_ != 0 } {
         unmap
      }
      set addr_ [${type_}::map $handle_ $usemmap $access]
      return $addr_
   }

   #  Unmap the dataset "data component", if mapped.
   public method unmap {} {
      if { $addr_ != 0 } {
         ${type_}::unmap $handle_ $addr_
         set addr_ 0
      }
   }

   #  Return the value of a "character component" of the dataset. These may be
   #  the units of the data and a label describing the units, nothing else is
   #  supported. So valid values for "what" are "units" and "label".
   public method getc {what} {
      return [${type_}::getc $handle_ $what]
   }

   #  Return a WCS describing the coordinates of a given WCS axis. Note axes
   #  may or may not be fixed to a given dataset axis, that isn't worried
   #  about here. The shift value is used when an offset in grid position
   #  along the axis is required (set to 0 for no effect). Usually this will
   #  be the alow value used in a call to getspectrum.
   public method getaxiswcs {axis shift} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::getaxiswcs $wcs $axis $shift]
   }

   #  Return the address of a spectral line of data. This will only
   #  work for cubes and requires that the complete data are mapped first.
   #  The axis is the spectral axis along which the line will be extracted
   #  (1,2,3). The alow and ahigh values define a range along the axis to
   #  extract (-1 for end points). The p1 and p2 positions, are grid
   #  coordinates of the spectrum along  the other two dimensions (increasing
   #  dimension order). The trunc argument provides for the removal of any
   #  trailing redundant axes (that axes of size 1, the first three must be
   #  significant).
   public method getspectrum {axis alow ahigh p1 p2 trunc} {
      if { $addr_ != 0 } {
         set dims [getdims $trunc]
         lassign [eval "array::getspectrum $addr_ $dims $axis $alow $ahigh \
                                           $p1 $p2 $cnfmap_"] adr
         return $adr
      }
      return 0
   }

   #  Access the data of an image plane. This will only work for cubes and
   #  requires that the complete data are mapped first. The axis is the
   #  spectral axis along which the image will be extracted (1,2,3), the index
   #  value selects the plane along that axis.  The trunc argument provides for
   #  the removal of any trailing redundant axes (that axes of size 1, the
   #  first three must be significant). The address of an array info structure
   #  is returned (query using getinfo).
   public method getimage {axis index trunc} {
      if { $addr_ != 0 } {
         set dims [getdims $trunc]
         return [eval "array::getimage $addr_ $dims $axis $index $cnfmap_"]
      }
      return 0
   }

   #  Free data allocated by any of the get methods (spectra and image
   #  sections).
   public method release {adr} {
      array::release $adr $cnfmap_
   }

   #  Return the underlying information about an accessor array. This
   #  is the real memory address, number of elements, HDS data type of
   #  the cube data and the HDS data type of any extracted data (will
   #  differ for FITS scaled data).
   public method getinfo {adr} {
      return [array::getinfo $adr]
   }

   #  Create an NDF that represents the bare bones of an image extracted from
   #  the attached dataset. The bare bones are an NDF of the correct
   #  dimensions and bounds, with an appropriate WCS. No data components are
   #  copied. Returns a new instance of this class wrapping the new NDF.
   public method createimage {name axis} {
      if { $addr_ == 0 } {
         error "Must map in cube data before creating an image"
      }

      #  Get the underlying info. Note the cube type may not be the image type
      #  (because of scaling of variants) so we must check what getimage will
      #  return.
      lassign [getinfo $addr_] adr nel hdstype fulltype

      #  Select the image axes, these are not axis.
      if { $axis == 1 } {
         set axis1 2
         set axis2 3
      } elseif { $axis == 2 } {
         set axis1 1
         set axis2 3
      } else {
         set axis1 1
         set axis2 2
      }

      #  Get a 2D WCS for our chosen axes.
      set fullwcs [${type_}::getwcs $handle_]
      set dims [getdims 1]
      set dim1 [lindex $dims [expr $axis1-1]]
      set dim2 [lindex $dims [expr $axis2-1]]
      set dim3 [expr [lindex $dims [expr $axis-1]]/2]
      set imagewcs [gaiautils::get2dwcs $fullwcs $axis1 $axis2 $dim1 $dim2 $dim3]

      #  Get the bounds for our chosen axes.
      set bounds [getbounds 1]
      set lbnd1 [lindex $bounds_ [expr (${axis1}-1)*2]]
      set ubnd1 [lindex $bounds_ [expr (${axis1}-1)*2+1]]
      set lbnd2 [lindex $bounds_ [expr (${axis2}-1)*2]]
      set ubnd2 [lindex $bounds_ [expr (${axis2}-1)*2+1]]

      #  And create the NDF.
      set newhandle \
         [ndf::create $name $lbnd1 $ubnd1 $lbnd2 $ubnd2 $fulltype $imagewcs]

      #  Create a new instance to manage the new NDF.
      set accessor [uplevel \#0 GaiaNDAccess \#auto]
      $accessor acquire $name $newhandle

      return $accessor
   }

   #  Create a WCS that represents the coordinates of an image extracted from
   #  the attached dataset. The axis value is the axis that is not part of the
   #  image and index is an index along this axis (its coordinates is used to
   #  replace all coordinates along that axis when it is removed, so should
   #  normally be the image plane index).
   public method getimagewcs {axis index} {
      if { $addr_ != 0 } {

         #  Select the image axes, these are not axis.
         if { $axis == 1 } {
            set axis1 2
            set axis2 3
         } elseif { $axis == 2 } {
            set axis1 1
            set axis2 3
         } else {
            set axis1 1
            set axis2 2
         }

         #  Get a 2D WCS for our chosen axes.
         set fullwcs [${type_}::getwcs $handle_]
         set dims [getdims 1]
         set dim1 [lindex $dims [expr $axis1-1]]
         set dim2 [lindex $dims [expr $axis2-1]]
         set imagewcs \
            [gaiautils::get2dwcs $fullwcs $axis1 $axis2 $dim1 $dim2 $index]
         return $imagewcs
      }
      return 0
   }

   #  Apply a strings of AST attributes to the WCS component of the dataset.
   public method astset {attribs} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::astset $wcs $attribs]
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the dataset as supplied by the user.
   public variable dataset {} {
      if { $dataset != {} } {
         parse_name_
      }
   }

   #  Sets the "mapping mode" to be used when reading in the data
   #  component. By default (1) this is "mmap", assuming the underlying system
   #  supports if, otherwise file i/o and malloc should be used. If changed
   #  after a mmap it is the callers responsibility to unmap and remap.
   public variable usemmap 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Object to parse names.
   protected variable namer_ {}

   #  Data access type, one of "ndf" or "fits".
   protected variable type_ {}

   #  The dimensionality of the data (list).
   protected variable dims_ {}

   #  The bounds of the data (list).
   protected variable bounds_ {}

   #  The handle to the opened dataset. NDF or FITS identifier.
   protected variable handle_ {}

   #  The memory address of the dataset data component.
   protected variable addr_ 0

   #  Whether mapped data should be registered with CNF, currently this
   #  is always true.
   protected variable cnfmap_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
