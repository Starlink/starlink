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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

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
      set namer_ [gaia::GaiaImageName \#auto]

      #  No data components are mapped.
      set addr_(DATA) 0
      set addr_(VARIANCE) 0
      set addr_(QUALITY) 0

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

      #  Get underlying data access type. Allow NDFs to be compressed.
      $namer_ configure -imagename $dataset
      if { [string match ".sdf*" [$namer_ type]] } {
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
      set handle_ [${type_}::open [$namer_ fullname 1]]
      set prop_changes_ 0
   }

   #  Close the dataset, if open, returns 1 in that case.
   public method close {} {
      if { $handle_ != {} } {
         if { $type_ == "fits" } {
            # FITS data needs unmapping, NDF happens automatically.
            unmap "*"
         }

         ${type_}::close $handle_
         set handle_ {}
         unset addr_
         set addr_(DATA) 0
         set addr_(VARIANCE) 0
         set addr_(QUALITY) 0
         set cnfmap_ 0
         set dims_ {}
         set tdims_ {}
         set bounds_ {}
         set tbounds_ {}
         set maperrors 0
         return 1
      }
      return 0
   }

   #  Acquire an already opened dataset by name and "handle".
   public method acquire {name handle} {
      set handle_ $handle
      $namer_ configure -imagename $name
      set dataset $name
      if { "[$namer_ type]" == ".sdf" } {
         set type_ "ndf"
         set cnfmap_ 1
      } else {
         set type_ "fits"
         set cnfmap_ 1
      }
   }

   #  Determine if a named component exists.
   public method exists {component} {
      return [${type_}::exists $handle_ $component]
   }

   #  Get the dimensions of the full data. Returns a list of integers.
   #  If trunc is true then any trailing redundant axes are trimmed.
   public method getdims {trunc} {
      if { $trunc } {
         if { $tdims_ == {} } {
            set tdims_ [${type_}::getdims $handle_ 1]

            #  If necessary pad the dimensions up or down to the expected
            #  size, provided the extra dimensions are trivial.
            set needbounds $ndims
            set gotbounds [llength $tdims_]
            if { $gotbounds < $needbounds } {
               for {set i $gotbounds} {$i < $needbounds} {incr i} {
                  lappend tdims_ 1
               }
            } elseif { $gotbounds > $needbounds } {
               for {set i [expr $gotbounds -1]} {$i >= $needbounds} {incr i -1} {
                  if { [lindex $tdims_ $i] == 1 } {
                     set tdims_ [lrange $tdims_ 0 [expr $i -1]]
                  } else {
                     #  None trivial, this is now an error.
                     error "Too many non-trivial dimensions"
                     break
                  }
               }
            }
         }
         return $tdims_
      }
      if { $dims_ == {} } {
         set dims_ [${type_}::getdims $handle_ 0]
      }
      return $dims_
   }

   #  Get the pixel ranges/bounds of the full data. Returns pairs of
   #  integers, one for each dimension. For FITS files the lower bound will
   #  always be 1. If trunc is true then any trailing redundant axes are
   #  trimmed.
   public method getbounds {trunc} {
      if { $trunc } {
         if { $tbounds_ == {} } {
            set tbounds_ [${type_}::getbounds $handle_ 1]

            #  If necessary pad the bounds up to the expected size.
            set needbounds [expr $ndims*2]
            set gotbounds [llength $tbounds_]
            if { $gotbounds < $needbounds } {
               for {set i $gotbounds} {$i < $needbounds} {incr i} {
                  lappend tbounds_ 1
               }
            }
         }
         return $tbounds_
      }
      if { $bounds_ == {} } {
         set bounds_ [${type_}::getbounds $handle_ 0]
      }
      return $bounds_
   }

   #  Return the coordinate of a position along a given axis.
   #
   #  The arguments are the index of the axis, a list of all the pixel indices
   #  needed to identify the coordinate, and an optional boolean argument that
   #  determines whether to format the value (using astFormat) and if to add
   #  label and units strings (trail) which can be formatted for reading.
   public method getcoord {axis indices {formatted 1} {trail 0} {readable 0}} {
      return [${type_}::getcoord $handle_ $axis $indices $formatted $trail $readable]
   }

   #  Map in a component of the dataset. Returns a structure that can be
   #  queried using the getinfo method. The mapping uses mmap, if possible and
   #  currently the default, and the given access mode,  one of "READ",
   #  "UPDATE" or "WRITE". Clearly this must match what access the file
   #  supports. The only component available for FITS is "DATA"
   public method map { {access "READ"} {component "DATA"} } {
      if { $addr_($component) != 0 } {
         unmap $component
      }

      #  Map VARIANCE as ERROR if asked. Note only have one of these
      #  mapped at the same time.
      set comp $component
      if { $comp == "VARIANCE" && $maperrors } {
         set comp "ERROR"
      }

      set addr_($component) [${type_}::map $handle_ $usemmap $access $comp]
      return $addr_($component)
   }

   #  Unmap a component of the dataset, all are unmapped if component == "*".
   public method unmap {{component "DATA"}} {
      if { $component == "*" } {
         foreach component [array names addr_] {
            if { $addr_($component) != 0 } {
               ${type_}::unmap $handle_ $component $addr_($component)
               set addr_($component) 0
            }
         }
      } else {
         if { $addr_($component) != 0 } {
            ${type_}::unmap $handle_ $component $addr_($component)
            set addr_($component) 0
         }
      }
   }

   #  Return if a component is mapped.
   public method ismapped {component} {
      if { [info exists addr_($component)] && $addr_($component) != 0 } {
         return 1
      }
      return 0
   }

   #  Return the value of a "character component" of the dataset. These may be
   #  the units of the data and a label describing the units, nothing else is
   #  supported. So valid values for "what" are "units" and "label".
   public method getc {what} {
      return [${type_}::getc $handle_ $what]
   }

   #  Return a WCS describing the coordinates of the dataset. The backing
   #  implementation should cache this as it may be modified.
   public method getwcs {} {
      return [${type_}::getwcs $handle_]
   }

   #  Return a WCS describing the coordinates of a given WCS axis. Note axes
   #  may or may not be fixed to a given dataset axis, that isn't worried
   #  about here. If only part of the axis is being extracted then shift
   #  should be set to the pixel index of the lower bound.
   public method getaxiswcs {axis {shift 0} } {
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
   public method getspectrum {axis alow ahigh p1 p2 trunc {component "DATA"}} {
      set last_spectrum_args_ "$axis $alow $ahigh $p1 $p2 $trunc"
      set last_spectrum_type_ point
      if { $addr_($component) != 0 } {
         set dims [getdims $trunc]
         set adr [eval "array::getspectrum $addr_($component) \
                           $dims $axis $alow $ahigh $p1 $p2 $cnfmap_"]
         return $adr
      }
      return 0
   }

   #  Re-extract an component array that matches the last extracted spectrum.
   #  Must be preceeded by a call to getspectrum.
   public method getlastspectrum {{component "DATA"}} {
      if { $last_spectrum_args_ == {} } {
         error "getlastspectrum called before getspectrum or getregionspectrum"
      }
      if { $last_spectrum_type_ == "point" } {
         lassign $last_spectrum_args_ axis alow ahigh p1 p2 trunc
         return [getspectrum $axis $alow $ahigh $p1 $p2 $trunc $component]
      }
      lassign $last_spectrum_args_ axis alow ahigh desc meth trunc
      return \
         [getregionspectrum $axis $alow $ahigh $desc $meth $trunc $component]
   }

   #  Return the info held about the last spectrum that was extracted.
   #  Returns type (point||region), axis, alow, ahigh, p1, p2 and trunc.
   public method getlastspectruminfo {} {
      if { $last_spectrum_args_ == {} } {
         error "getlastspectruminfo called before \
                getspectrum or getregionspectrum"
      }
      lassign $last_spectrum_args_ axis alow ahigh p1 p2 trunc
      return "$last_spectrum_type_ $axis $alow $ahigh $p1 $p2 $trunc"
   }

   #  Return the address of a spectral line of data extracted from an image
   #  region projected along the spectral axis. This will only work for cubes
   #  and requires that the complete data are mapped first.  The axis is the
   #  spectral axis along which the line will be extracted (1,2,3). The alow
   #  and ahigh values define a range along the axis to extract (-1 for end
   #  points). The desc argument is a 2D ARD description of the area to
   #  extract, and method the data combination technique (each spectral
   #  position is the combination of data from the ARD region of an image
   #  plane), only "mean" is supported at present. All coordinates should be be
   #  in the GRID domain.  The trunc argument provides for the removal of any
   #  trailing redundant axes (that axes of size 1, the first three must be
   #  significant). Note desc may contain newlines etc.
   public method getregionspectrum {axis alow ahigh desc method trunc
                                    {component "DATA"}} {
      set last_spectrum_args_ "$axis $alow $ahigh $desc $method $trunc"
      set last_spectrum_type_ "region"
      if { $addr_($component) != 0 } {
         set dims [getdims $trunc]
         set adr [eval "array::getregionspectrum $addr_($component) \
                           $dims $axis $alow $ahigh \$desc $method $cnfmap_"]
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
   public method getimage {axis index trunc {component "DATA"}} {
      if { $addr_($component) != 0 } {

         set dims [getdims $trunc]
         return [eval "array::getimage $addr_($component) \
                          $dims $axis $index $cnfmap_"]
      }
      return 0
   }

   #  Free data allocated by any of the get methods (spectra and image
   #  sections).
   public method release {adr} {
      array::release $adr
   }

   #  Return the underlying information about an accessor array. This is the
   #  real memory address, number of elements, HDS data type of the data and
   #  the HDS data type of any extracted data (will differ for FITS scaled
   #  data).
   public method getinfo {adr} {
      return [array::getinfo $adr]
   }

   #  Create an NDF that represents the bare bones of an image extracted from
   #  the attached dataset, which must be a cube. The bare bones are an NDF of
   #  the include everything, except the data components. Returns a new
   #  instance of this class wrapping the new NDF.
   public method createimage {name axis {component "DATA"}} {
      if { $addr_($component) == 0 } {
         error "Must map in cube data before creating an image"
      }

      #  Check this is a cube.
      set dims [getdims 0]
      if { [llength $dims] < 3 } {
         error "createimage only works on cubes"
      }

      #  Get the underlying info. Note the cube type may not be the image type
      #  (because of scaling of variants) so we must check what getimage will
      #  return.
      lassign [getinfo $addr_($component)] adr nel hdstype fulltype

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
      set dim1 [lindex $dims [expr $axis1-1]]
      set dim2 [lindex $dims [expr $axis2-1]]
      set dim3 [expr [lindex $dims [expr $axis-1]]/2]
      set imagewcs [gaiautils::get2dwcs $fullwcs $axis1 $axis2 $dim1 $dim2 $dim3]

      #  Get the bounds for our chosen axes.
      set bounds [getbounds 0]
      set lbnd1 [lindex $bounds [expr (${axis1}-1)*2]]
      set ubnd1 [lindex $bounds [expr (${axis1}-1)*2+1]]
      set lbnd2 [lindex $bounds [expr (${axis2}-1)*2]]
      set ubnd2 [lindex $bounds [expr (${axis2}-1)*2+1]]

      #  And create the NDF as a copy of this NDF, but without any
      #  data arrays. If deep searches are enabled propagate the extension.
      #  If underlying cube is FITS create basic NDF only.
      if { $type_ == "ndf" && $deep_search } {
         set newhandle [ndf::copy $name $handle_ "AXIS, UNITS" \
                           "$lbnd1 $lbnd2" "$ubnd1 $ubnd2" $fulltype $imagewcs]
      } else {
         #  Create a simple NDF.
         set newhandle [ndf::create $name "$lbnd1 $lbnd2" "$ubnd1 $ubnd2" \
                           $fulltype $imagewcs]
      }

      #  Create a new instance to manage the new NDF.
      set accessor [uplevel \#0 gaia::GaiaNDAccess \#auto]
      $accessor acquire $name $newhandle

      return $accessor
   }

   #  Create an NDF that represents a bare bones copy the attached dataset,
   #  The bare bones are an NDF of the include everything, except the data
   #  components. Returns a new instance of this class wrapping the new NDF.
   public method createcopy {name {component "DATA"}} {
      if { $addr_($component) == 0 } {
         error "Must map in data before creating an image"
      }

      #  Get the underlying info. Note the type may not be the image type
      #  (because of scaling of variants) so we must check what getimage will
      #  return.
      lassign [getinfo $addr_($component)] adr nel hdstype fulltype

      #  Get the WCS.
      set fullwcs [${type_}::getwcs $handle_]

      #  Get the bounds.
      set bounds [getbounds 0]
      set lbnd {}
      set ubnd {}
      foreach {l u} $bounds {
         lappend lbnd $l
         lappend ubnd $u
      }

      #  And create the NDF as a copy, but without any data arrays. If deep
      #  searches are enabled propagate the extension. If underlying data is
      #  FITS create basic NDF only.
      if { $type_ == "ndf" && $deep_search } {
         set newhandle [ndf::copy $name $handle_ "AXIS, UNITS" \
                           $lbnd $ubnd $fulltype $fullwcs]
      } else {
         #  Create a simple NDF.
         set newhandle [ndf::create $name $lbnd $ubnd $fulltype $fullwcs]
      }

      #  Create a new instance to manage the new NDF.
      set accessor [uplevel \#0 gaia::GaiaNDAccess \#auto]
      $accessor acquire $name $newhandle

      return $accessor
   }

   #  Create a WCS that represents the coordinates of an image extracted from
   #  the attached dataset. The axis value is the axis that is not part of the
   #  image and index is an index along this axis (its coordinates is used to
   #  replace all coordinates along that axis when it is removed, so should
   #  normally be the image plane index).
   public method getimagewcs {axis index {component "DATA"}} {
      if { $addr_($component) != 0 } {

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

   #  Create an NDF or FITS file that represents the bare bones of the last
   #  spectrum extracted from the attached dataset. The bare bones are
   #  the spectral data, in the original data type, plus a WCS that describes
   #  the spectrum. In the case of an NDF further data components may be added
   #  as the returned instance of GaiaNDAccess is opened for write access.
   #
   #  The title should be a meaningful string that identifies this spectrum in
   #  the original cube.
   public method createspectrum {format name title} {
      if { $addr_(DATA) == 0 } {
         error "Must map in cube data before creating a spectrum"
      }
      if { $last_spectrum_args_ == {} } {
         error "Must extract a spectrum before creating a copy"
      }

      #  Recover information about the last extracted spectrum.
      lassign $last_spectrum_args_ axis alow ahigh p1 p2 trunc

      #  Get the underlying info. Note the cube type may not be the spectrum
      #  type (because of scaling of variants), we use the expanded type.
      lassign [getinfo $addr_(DATA)] adr nel hdstype fulltype

      #  Get a 1D WCS for our chosen axes.
      set spectralwcs [getaxiswcs $axis [expr $alow-1]]

      #  Extract the spectral data values and copy into place.
      set specdata [getlastspectrum "DATA"]

      if { $format == "NDF" } {
         if { $type_ == "ndf" } {
            #  Create the NDF as a copy of this NDF without the array
            #  components, but set the pixel origins and data type of these.
            #  Need to handle copying of the AXIS component, if
            #  any, so that is implicit.
            set newhandle [ndf::copy $name $handle_ "AXIS, UNITS" \
                              $alow $ahigh $fulltype $spectralwcs $axis]
         } else {
            #  Create a simple NDF.
            set newhandle [ndf::create $name $alow $ahigh \
                              $fulltype $spectralwcs]

            #  Set the UNITS obtained from the FITS cube.
            ndf::putc $newhandle "UNITS" [fits::getc $handle_ "UNITS"]
         }
         ndf::putc $newhandle "TITLE" $title

         #  Create a new instance to manage the new NDF.
         set accessor [uplevel \#0 gaia::GaiaNDAccess \#auto]
         $accessor acquire $name $newhandle

         #  And copy the spectral data into place.
         set specdatacomp [$accessor map "WRITE/BAD" "DATA"]
         array::copy $specdata $specdatacomp

      } else {
         #  Create simple FITS format spectrum, in same type as extracted
         #  spectrum.
         set newhandle [fits::create $name $specdata [expr $ahigh-$alow+1] \
                           $spectralwcs $title \
                           [${type_}::getc $handle_ "UNITS"]]

         #  Create a new instance to manage the new FITS file.
         set accessor [uplevel \#0 gaia::GaiaNDAccess \#auto]
         $accessor configure -dataset $name
      }
      return $accessor
   }

   #  Current number of property changes. Use this value as a cheap
   #  datestamp for modifications to the WCS and FITS headers (by
   #  comparing the value from the last access to the current one).
   public method getpropchanges {} {
      return $prop_changes_
   }

   #  Apply a strings of AST attributes to the WCS component of the dataset.
   public method astset {attribs} {
      set wcs [${type_}::getwcs $handle_]
      incr prop_changes_
      return [gaiautils::astset $wcs $attribs]
   }

   #  Get the value of an AST attribute.
   public method astget {attrib} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::astget $wcs $attrib]
   }

   #  Clear the value of an AST attribute.
   public method astclear {attrib} {
      set wcs [${type_}::getwcs $handle_]
      incr prop_changes_
      return [gaiautils::astclear $wcs $attrib]
   }

   #  Test if the value of an AST attribute has been set.
   public method asttest {attrib} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::asttest $wcs $attrib]
   }

   #  Return a list of the domains in the WCS component.
   public method astdomains {} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::astdomains $wcs]
   }

   #  See if the axes of the dataset frameset is a known frametype. These
   #  can be "specframe", "fluxframe", "dsbspecframe" and "timeframe".
   public method isaxisframetype {axis type} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::frameisa $wcs $axis $type]
   }

   #  Get descriptions for all the axis of the frameset. These are generic
   #  labels "spec", "time", "skylat" and "skylon", axis which are not one of
   #  these will be described as "unknown".
   public method axisdescriptions {} {
      set wcs [${type_}::getwcs $handle_]
      return [gaiautils::describeaxes $wcs]
   }

   #  Get the value of a property. A property is a primitive value stored
   #  in a named extension. Normally an extension follows the NDF concept and
   #  the name is the HDS component within the extension, for other formats
   #  support will be limited (for FITS this call is equivalent to looking
   #  for a value in the header cards, with extension "FITS" and name
   #  the keyword).
   public method getproperty {extension name} {
      return [${type_}::getproperty $handle_ $extension $name]
   }

   #  As getproperty, except get as a double precision number. Keeps
   #  precision for NDF extension primitives.
   public method getdoubleproperty {extension name} {

      #  For FITS we just read the encoded value anyway, so no useful
      #  functionality.
      if { $type_ == "fits" } {
         return [fits::getproperty $handle_ $extension $name]
      }
      return [${type_}::getdoubleproperty $handle_ $extension $name]
   }

   #  Check if a named extension exists.
   public method extensionexists {extension} {
      return [${type_}::extensionexists $handle_ $extension]
   }

   #  Get the dimensions of a property in an extension. Returns an empty
   #  string if the extension or property do not exist.
   public method getpropertydims {extension component} {
      #  Only valid for NDFs.
      if { $type_ == "ndf" } {
         if { [catch {ndf::getpropertydims \
                         $handle_ $extension $component} dims] == 0 } {
            return $dims
         }
      }
      return ""
   }

   #  Get the FITS headers as a single string (newline separated cards).
   public method fitsheaders {} {
      return [${type_}::fitsheaders $handle_]
   }

   #  Get the value of a FITS card.
   public method fitsread {keyword} {
      return [${type_}::fitsread $handle_ $keyword]
   }

   #  Set the value of a FITS card. Will be saved, if the dataset is opened
   #  for write access. If only one value is given it is assumed to be a
   #  pre-formatted card. The type value should be set to either numeric
   #  or char (numeric avoids surrounding the value in quotes).
   public method fitswrite {keyword {value ""} {comment ""} {type "char"}} {
      if { $value != "" } {
         ${type_}::fitswrite $handle_ $keyword $value $comment $type
      } else {
         ${type_}::fitswrite $handle_ $keyword
      }
      incr prop_changes_
   }

   #  HDU access method. Returns number of HDUs and meta-data describing
   #  the HDUs found. Also provides for the switching of the HDU and
   #  querying the current HDU. The args can be "list" or "listheadings"
   #  and "get <n> filename" for FITS (save compressed images to disk).
   public method hdu {args} {
      if { $args == {} } {
         return [${type_}::hdu $handle_]
      }
      if { $args == "list" } {
         return [${type_}::hdu $handle_ list $deep_search]
      }
      return [eval ${type_}::hdu $handle_ $args]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the dataset as supplied by the user.
   public variable dataset {} {
      if { $dataset != {} } {
         parse_name_
      }
   }

   #  Set the expected dimensionality. When accessing truncated bounds or
   #  dimensions this is the number you always want returning (3 for a cube)
   #  for consistency in the handling. Insignificant later dimensions will
   #  be returned as dimension 1.
   public variable ndims 3 {

      #  Make sure these are regenerated.
      set tbounds_ {}
      set tdims_ {}
   }

   #  Sets the "mapping mode" to be used when reading in the data
   #  component. By default (1) this is "mmap", assuming the underlying system
   #  supports if, otherwise file i/o and malloc should be used. If changed
   #  after a mmap it is the callers responsibility to unmap and remap.
   public variable usemmap 1

   #  Whether to map the variance array as standard deviations.
   public variable maperrors 0 {
      #  Twist, if already mapped and this value toggles we need to release
      #  the component.
      if { $maperrors != $last_maperrors_ && $addr_(VARIANCE) != 0 } {
         unmap "VARIANCE"
      }
      set last_maperrors_ $maperrors
   }

   #  Whether all "hdu list" calls should perform a deep search (global
   #  as this class is used in many places which are difficult to track).
   #  Also controls if NDF extensions are propagated, not needed if no
   #  searching is done.
   public proc set_deep_search {value} {
      set deep_search $value
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Object to parse names.
   protected variable namer_ {}

   #  Data access type, one of "ndf" or "fits".
   protected variable type_ {}

   #  The truncated and untruncated dimensionality of the data.
   protected variable dims_ {}
   protected variable tdims_ {}

   #  The truncated and untruncated bounds of the data.
   protected variable bounds_ {}
   protected variable tbounds_ {}

   #  The handle to the opened dataset. NDF or FITS identifier.
   protected variable handle_ {}

   #  The memory addresses of the dataset data components. Usually
   #  only "DATA" is available.
   protected variable addr_

   #  Whether mapped data should be registered with CNF, currently this
   #  is always true.
   protected variable cnfmap_ 0

   #  All the arguments used to extract the last spectrum. Can be used
   #  with getlastspectrum to retrieve this.
   protected variable last_spectrum_args_ {}
   protected variable last_spectrum_type_ point

   #  Integer which will be incremented when a property is changed.
   protected variable prop_changes_ 0

   #  Last value of maperrors.
   protected variable last_maperrors_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Whether to search for related NDFs in the NDF extensions (hdu list).
   protected common deep_search 1

#  End of class definition.
}
