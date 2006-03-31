#+
#  Name:
#     GaiaNDAccess

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Handles multidimensional dataset access in GAIA. 

#  Description:
#     This class is designed to handle the access and description of datasets
#     that don't have 2 dimensions. Superficially it is datatype independent,
#     supporting access to NDF and FITS data, relying on the ndf:: and
#     fits:: (to be written) Tcl commands. Sections of the data can be
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
         error {Cannot access FITS files that are not 2D. \
                You should convert this file to an NDF first using the \
                ndf2fits command in the CONVERT package.}
         set cnfmap_ 0
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
         set nel_ 0
         set hdstype_ {}
         set cnfmap_ 0
         set dims_ {}
      }
   }

   #  Get the dimensions of the full data. Returns a list of integers.
   public method dims {} {
      if { $dims_ == {} } {
         set dims_ [${type_}::dims $handle_]
      }
      return $dims_
   }

   #  Get the pixel ranges/bounds of the full data. Returns pairs of
   #  integers, one for each dimension. For FITS files the lower bound will
   #  always be 1.
   public method bounds {} {
      return [${type_}::bounds $handle_]
   }

   #  Return the coordinate of a position along a given axis.
   #
   #  The arguments are the index of the axis, a list of all the pixel indices
   #  needed to identify the coordinate, and an optional boolean arguments that
   #  determines whether to format the value (using astFormat) and if to add
   #  trailing label and units strings.
   public method coord {axis indices {formatted 1} {trail 0} } {
      return [${type_}::coord $handle_ $axis $indices $formatted $trail]
   }

   #  Map in the dataset "data component". Returns the address, number of
   #  elements and the data type (these are in the HDS format). The mapping
   #  uses mmap, if possible and requested.
   public method map {} {
      lassign [${type_}::map $handle_ $usemmap] addr_ nel_ hdstype_
      return [list $addr_ $nel_ $hdstype_]
   }

   #  Unmap in the dataset "data component", if mapped.
   public method unmap {} {
      if { $addr_ != 0 } {
         ${type_}::unmap $handle_
         set addr_ 0
         set nel_ 0
         set hdstype_ 0
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
      return [${type_}::getwcs $handle_ $axis $shift]
   }

   #  Return the address of a spectral line of data. This will only 
   #  work for cubes and requires that the complete data are mapped first. 
   #  The axis is the spectral axis along which the line will be extracted
   #  (1,2,3). The alow and ahigh values define a range along the axis to
   #  extract (-1 for end points). The p1 and p2 positions, are grid
   #  coordinates of the spectrum along  the other two dimensions (increasing
   #  dimension order). 
   public method getspectrum {axis alow ahigh p1 p2} {
      if { $addr_ != {} } {
         set dims_ [dims]
         lassign [eval "array::getspectrum $addr_ $hdstype_ $dims_ $axis \
                     $alow $ahigh $p1 $p2 $cnfmap_"] adr nel
         return "$adr $nel $hdstype_"
      }
   }

   #  Return the address of an image plane. This will only work for cubes 
   #  and requires that the complete data are mapped first. The axis is the
   #  spectral axis along which the image will be extracted (1,2,3), the 
   #  index value selects the plane along that axis.
   public method getimage {axis index} {
      if { $addr_ != {} } {
         set dims_ [dims]
         lassign [eval "array::getimage $addr_ $hdstype_ $dims_ $axis \
                     $index $cnfmap_"] adr nel
         return "$adr $nel $hdstype_"
      }
   }

   #  Free data allocated by any of the get methods (spectra and image
   #  sections).
   public method release {adr} {
      array::release $adr $cnfmap_
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

   #  The handle to the opened dataset. NDF or FITS identifier.
   protected variable handle_ {}

   #  The memory address of the dataset data component.
   protected variable addr_ 0

   #  The number of elements in the dataset data component.
   protected variable nel_ 0

   #  The HDS data type of the dataset data.
   protected variable hdstype_ {}

   #  Whether mapped data should be registered with CNF (required for some
   #  NDF actions).
   protected variable cnfmap_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
