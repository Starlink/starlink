#+
#  Name:
#     Gaia3dVtkCubeData

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Wrapper class for accessing vtkImageData instances.

#  Description:
#     Accesses the cube data in a instance of GaiaNDArray and makes
#     it available as a vtkImageData instance. Also provides facilities
#     for creating a sub-volume of the cube and handles the changes
#     to the world coordinate and apparent size.

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     16-NOV-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkCubeData {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $imagedata_ != {} } {
         $imagedata_ Delete
      }
      if { $imageimport_ != {} && [info exists $imageimport_] } {
         $imageimport_ Delete
      }
      if { $limits != {} } {
         if { $wcs_ != {} } {
            catch {gaiautils::astannul $wcs_}
         }
      }
      if { $stencil_ != {} } {
         $stencil_ Delete
      }
      if { $stencil_filter_ != {} } {
         $stencil_filter_ Delete
      }
   }

   #  Methods and procedures:
   #  -----------------------

   #  Access the data of the cube. Nothing is usually done until this is
   #  called so that unnecessary accesses are not performed. Note a subsection
   #  will be accessed if the limits public variable is set.
   public method access {} {
      if { $cubeaccessor == {} } {
         return
      }

      #  Get the full dimensionality.
      set dims [$cubeaccessor getdims 0]
      set ndims [llength $dims]

      #  Make sure the cube data is available.
      set arrayinfo [$cubeaccessor map "READ" "DATA"]

      #  Make vtkImageImport instance for assigning the imported data to a
      #  vtkImageData instance. XXX need to test if exists, why?
      if { $imageimport_ != {} && [info exists $imageimport_] } {
         $imageimport_ Delete
      }
      set imageimport_ [::vtkImageImport New]

      #  Parse the limits to get the subsection of the data, if any.
      parse_limits_ $dims

      #  Set dimensions to import. The cube will be sectioned by GAIA, so this
      #  is the whole extent.
      set d1 [expr $ubnd_(1)-$lbnd_(1)-1]
      set d2 [expr $ubnd_(2)-$lbnd_(2)-1]
      set d3 [expr $ubnd_(3)-$lbnd_(3)-1]
      $imageimport_ SetWholeExtent 0 $d1 0 $d2 0 $d3
      $imageimport_ SetDataExtentToWholeExtent

      #  Do the sharing of the data from GAIA to VTK, if possible.
      #  This normalises FITS data and returns if any bad values were
      #  detected (strong truth), it can also replace any data values
      #  with a null value (need to do this for volumes). If this happens
      #  then a copy of the data is made.
      set nobad [gvtk::setarray $arrayinfo $imageimport_ $dims \
                    $limits $checkbad $nullvalue]

      #  Read data into vktImageData.
      if { $imagedata_ != {} } {
         $imagedata_ Delete
      }
      set imagedata_ [$imageimport_ GetOutput]

      #  Origins are 1, not 0 for AST GRID coordinates, so stick to that.
      #  We have a default spacing of 1,1,1.
      $imagedata_ SetOrigin 1.0 1.0 1.0

      #  If we have limits that only use part of the cube need to match the
      #  WCS so that this looks like the full data.
      set wcs_ [$cubeaccessor getwcs]
      if { $limits != {} } {
         set shifts "[lindex $limits 0] [lindex $limits 2] [lindex $limits 4]"
         set wcs_ [gaiautils::shiftwcs $wcs_ $shifts 1]
      }

      #  The next section would deal with BAD pixels, if VisibilityConstraint
      #  worked with point data, current it doesn't and only works with cells.
      #  Left in place in case that changes. Note when that happens we could
      #  extend VisibilityConstriant to self-test the data, instead of
      #  using a visibility array.
      #
      #  Wrap with vtkUniformGrid with bespoke VisibilityConstraint to support
      #  blanking for some filters (not volume rendering). Need to just compare
      #  values with known BAD values, rather than use a blank array (unsigned
      #  char), but will require modifications to VTK so we can subclass.
      #  if { $visibility_check && ( ! $nobad ) } {
      #     set mask [::vtkUnsignedCharArray New]
      #     if { [gvtk::createmask $arrayinfo $mask] } {
      #        #  BAD pixels found. Do the wrap and set the mask array.
      #        set uniformgrid [::vtkUniformGrid New]
      #        #  Get data before we can copy.
      #        $imagedata_ Update
      #        $uniformgrid ShallowCopy $imagedata_
      #        $uniformgrid SetPointVisibilityArray $mask
      #        $imagedata_ Delete
      #        $mask Delete
      #        set imagedata_ $uniformgrid
      #      }
      #   }
      return
   }

   #  If required apply the CUPID mask to pick out regions of the data.
   #  Should be done sometime after access but before anything has been
   #  realised. Usually you will setup the masks between a call to access
   #  and this.
   public method applymasks {} {
      if { $applymask && $pixelmask != {} } {

         #  The base segmenter should contain the relevant mask.
         if { [$pixelmask exists 0] } {
            if { $stencil_ == {} } {
               set stencil_ [::vtkImageStencil New]
            }
            if { $stencil_filter_ == {} } {
               set stencil_filter_ [::vtkPolyDataToImageStencil New]
            }
            $stencil_ SetInput $imagedata_
            $stencil_ SetStencil [$stencil_filter_ GetOutput]
            $stencil_ SetBackgroundValue 0.0

            if { [$pixelmask invert 0] } {
               $stencil_ ReverseStencilOn
            } else {
               $stencil_ ReverseStencilOff
            }

            $pixelmask connect_stencil_filter 0 $stencil_filter_
         }
      } else {
         #  Clear any existing stencil objects.
         if { $stencil_ != {} } {
            $stencil_ Delete
            set stencil_ {}
         }
         if { $stencil_filter_ != {} } {
            $stencil_filter_ Delete
            set stencil_filter_ {}
         }
      }
      return
   }

   #  Access the vtkImageData instance.
   public method get_imagedata {} {
      return $imagedata_
   }

   #  Access vtkImageStencil if being applied, empty if not.
   public method get_stencil {} {
      return $stencil_
   }

   #  Access the WCS of the cube, you should use this method, rather than the
   #  cubeaccessor as section may be used.
   public method get_wcs {} {
      return $wcs_
   }

   #  Access the dimensions of the cube, you should use this method, rather
   #  than the cubeaccessor as section may be used.
   public method get_dims {} {
      return $dims_
   }

   #  Convert a pixel index from the full cube into a grid index for the
   #  used part of the cube along a given axis.
   public method pixel2grid {axis pindex} {
      if { $limits != {} } {
         set zo [expr $flbnds_($axis)+$lbnd_($axis)]
      } else {
         set zo $flbnds_($axis)
      }
      return [expr round($pindex+1-$zo)]
   }

   #  Convert a grid index from the apparent cube into a grid index for the
   #  full cube along a given axis.
   public method grid2pixel {axis gindex} {
      if { $limits != {} } {
         set zo [expr $flbnds_($axis)+$lbnd_($axis)]
      } else {
         set zo $flbnds_($axis)
      }
      return [expr round($gindex-1+$zo)]
   }

   #  Parse the limits into bounds and dimensions. This extracts and clips to
   #  the given dims and also resets limits if the bounds are found to be those
   #  of the data or need modification.
   protected method parse_limits_ { dims } {
      lassign $dims d(1) d(2) d(3)
      if { $limits != {} } {
         lassign $limits lbnd_(1) ubnd_(1) lbnd_(2) ubnd_(2) lbnd_(3) ubnd_(3)
         set nset 0
         for { set i 1 } { $i < 4 } { incr i } {
            if { $lbnd_($i) <= 0 } {
               set lbnd_($i) 0
               incr nset
            }
            if { $ubnd_($i) >= $d($i) } {
               set ubnd_($i) $d($i)
               incr nset
            }
         }

         #  If these are now the actual cube bounds reset to blank so we don't
         #  do any unnecessary work (note above uses <= & >= to test edges).
         #  Otherwise set to the actual limits used.
         if { $nset == 6 } {
            set limits {}
            set dims_ $dims
         } else {
            set limits \
               "$lbnd_(1) $ubnd_(1) $lbnd_(2) $ubnd_(2) $lbnd_(3) $ubnd_(3)"
            set dims_ "[expr $ubnd_(1)-$lbnd_(1)] \
                       [expr $ubnd_(2)-$lbnd_(2)] \
                       [expr $ubnd_(3)-$lbnd_(3)]"
         }
      } else {
         set dims_ $dims
         set lbnd_(1) 0
         set ubnd_(1) $d(1)
         set lbnd_(2) 0
         set ubnd_(2) $d(2)
         set lbnd_(3) 0
         set ubnd_(3) $d(3)
      }

      #  Record pixel bounds of the cube. Used to convert.
      lassign [$cubeaccessor getbounds 0] \
         flbnds_(1) fubnds_(1) flbnds_(2) fubnds_(2) flbnds_(3) fubnds_(3)
   }

   #  Set the limits for an axis. The other limits are set to the cube
   #  dimensions. These will be applied during the next access.
   public method set_axis_limits {axis axlimits} {
      set dims [$cubeaccessor getdims 0]
      lassign $dims nx ny nz
      lassign $axlimits ll ul
      if { $axis == 1 } {
         set limits "$ll $ul 0 $ny 0 $nz"
      } elseif { $axis == 2 } {
         set limits "0 $nx $ll $ul 0 $nz"
      } else {
         set limits "0 $nx 0 $ny $ll $ul"
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaNDArray instance that wraps the data-cube.
   public variable cubeaccessor {}

   #  Whether the data should be checked for bad values.
   public variable checkbad 0

   #  The value to use as a replacement for bad values.
   public variable nullvalue 0

   #  Limits of the data. Do not set if full data is required. The values
   #  are pairs along each axis.
   public variable limits {}

   #  Whether the data should have regions extracted using the CUPID mask.
   public variable applymask 0

   #  Instance of Gaia3dCupidMasks that manages the CUPID mask.
   public variable pixelmask {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable imagedata_ {}
   protected variable imageimport_ {}

   #  The limits of the data if a subsection is being used. Index
   #  by axis.
   protected variable lbnd_
   protected variable ubnd_

   #  The AST FrameSet describing the cube coordinates. Will be changed if
   #  limits are being used.
   protected variable wcs_ {}

   #  The apparent dimensions of the cube. Will differ if limits are being
   #  used.
   protected variable dims_ {}

   #  Pixel bounds of the full cube.
   protected variable flbnds_
   protected variable fubnds_

   #  The stencil and stencil filter.
   protected variable stencil_ {}
   protected variable stencil_filter_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
