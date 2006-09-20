#+
#  Name:
#     GaiaSpecWriter

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Provide facilities for saving the current spectrum to a disk file.

#  Description:
#     Writes the current spectrum, displayed by a related instance of
#     GaiaCubeSpectrum, to disk file. Understands the SPLAT text-file
#     format, NDF and FITS and can save the spectrum to any of these types.
#     NDF and TEXT files support the addition of variance information, and
#     the NDF format, origin, FITS headers, quality and history, if the
#     associated cube is an NDF.

#  Invocations:
#
#        GaiaSpecWriter object_name [configuration options]
#
#     This creates an instance of a GaiaSpecWriter object. The return is
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
#     See individual method declarations below.

#  Inheritance:
#     Nothing.

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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     30-AUG-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpecWriter {}

itcl::class gaia::GaiaSpecWriter {

   #  Inheritances:
   #  -------------
   #  Nothing for this class.

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options, requires a GaiaCube and GaiaCubeSpectrum
      #  instances.
      eval configure $args
      if { $cubespectrum == {} } {
         error "Need to define a GaiaCubeSpectrum instance"
      }
      if { $gaiacube == {} } {
         error "Need to define a GaiaCube instance"
      }
   }

   #  Destructor:
   #  -----------
   destructor {
      #  Nothing to do.
   }

   #  Methods:
   #  --------

   #  Set the menu used for the save actions. Should only be called once.
   public method set_menu {menu} {
      #  Keep the associated toplevel window.
      set w_ [winfo toplevel $menu]

      #  Add each of the actions to the menu.
      $menu add command -label {Save as text} \
         -command [code $this save_as_text]

      $menu add command -label {Save as NDF} \
         -command [code $this save_as_ndf]

      $menu add command -label {Save as FITS} \
         -command [code $this save_as_fits]
   }

   # SPLAT Text file format.
   # -----------------------
   # Uses access to the GaiaSpectralPlot and writes the displayed data.
   # See SPLAT for the details of the format.

   #  Save the current spectrum to a text file.
   public method save_as_text {} {
      set w [FileSelect .\#auto -title "Save spectrum to text file"]
      if { [$w activate] } {
         if { [catch {write_as_text [$w get]} msg] } {
            error_dialog "Failed to write spectrum: $msg"
         }
      }
      destroy $w
   }

   #  Write the current spectrum to a text file. Use SPLAT style.
   public method write_as_text { filename } {
      if { $filename != {} } {

         #  Get the GaiaSpectralPlot instance and its components.
         set gaiaspectrum [$cubespectrum get_spectrum]
         set spectrum [$gaiaspectrum get_spectrum]
         if { $spectrum != {} } {

            blt::busy hold $w_

            set canvas [$gaiaspectrum component canvas]
            set frameset [$canvas itemcget $spectrum -frameset]

            #  Open the output file.
            set fid [::open $filename w]

            #  Write the header section.
            puts $fid "#BEGIN"
            puts $fid "# File created by GAIA"

            #  Set name of the spectrum.
            set shortname [$cubespectrum sectioned_name]
            if { $shortname == {} } {
               puts $fid "# name $filename"
            } else {
               puts $fid "# name $shortname"
            }

            #  Write all known SpecFrame attributes.
            foreach att $specframe_atts_ {
               write_ast_att_ $fid $frameset $att
            }

            #  And units for future FluxFrames.
            set units [$canvas itemcget $spectrum -dataunits]
            if { $units != {} } {
               puts $fid "# DataUnits $units"
            }
            set label [$canvas itemcget $spectrum -datalabel]
            if { $label != {} } {
               puts $fid "# DataLabel $label"
            }
            puts $fid "#END"

            #  Now write the values.
            foreach {x y} [$canvas coords $spectrum] {
               puts $fid "$x $y"
            }

            #  Finally close the file.
            ::close $fid
         }

         blt::busy release $w_
      }
   }
   protected method write_ast_att_ {fid frameset att} {
      catch {
         set value [gaiautils::astget $frameset $att]
         puts $fid "# $att $value"
      }
   }

   #  NDF format
   #  ----------

   #  Save the current spectrum to an NDF.
   public method save_as_ndf {} {
      set w [FileSelect .\#auto -title "Save spectrum to an NDF"]
      if { [$w activate] } {
         if { [catch {write_as_ndf [$w get]} msg] } {
            error_dialog "Failed to write spectrum: $msg"
         }
      }
      destroy $w
   }

   #  Save a simulation of the currently displayed spectrum as an NDF.
   public method write_as_ndf { filename } {
      if { $filename != {} } {
         blt::busy hold $w_

         #  Use the GaiaCubeSpectrum to get access to the GaiaNDAccess
         #  instance and use that to re-extract the data.
         set cubeaccessor [$gaiacube get_cubeaccessor]

         #  Set name of the spectrum.
         set shortname [$cubespectrum sectioned_name]
         if { $shortname == {} } {
            set shortname "$filename"
         }

         #  Create the basic spectral NDF. Contains data from the last
         #  spectrum that was extracted.
         set specaccessor [$cubeaccessor createspectrum "NDF" $filename \
                              $shortname]

         #  Check for other data components and copy them too.
         if { [$cubeaccessor exists "VARIANCE"] } {
            $cubeaccessor map "READ" "VARIANCE"
            set specdatacomp [$specaccessor map "WRITE/BAD" "VARIANCE"]
            set cubespecdatacomp [$cubeaccessor getlastspectrum "VARIANCE"]
            array::copy $cubespecdatacomp $specdatacomp
            $cubeaccessor unmap "VARIANCE"
         }
         
         if { [$cubeaccessor exists "QUALITY"] } {
            $cubeaccessor map "READ" "QUALITY"
            set specdatacomp [$specaccessor map "WRITE/BAD" "QUALITY"]
            set cubespecdatacomp [$cubeaccessor getlastspectrum "QUALITY"]
            array::copy $cubespecdatacomp $specdatacomp
            $cubeaccessor unmap "QUALITY"
         }
         $specaccessor close

         blt::busy release $w_
      }
   }

   #  FITS format
   #  -----------

   #  Save the current spectrum to a FITS file.
   public method save_as_fits {} {
      set w [FileSelect .\#auto -title "Save spectrum to FITS"]
      if { [$w activate] } {
         if { [catch {write_as_fits [$w get]} msg] } {
            error_dialog "Failed to write spectrum: $msg"
         }
      }
      destroy $w
   }

   #  Save a simulation of the currently displayed spectrum as a FITS file.
   public method write_as_fits { filename } {
      if { $filename != {} } {
         blt::busy hold $w_

         #  Use the GaiaCubeSpectrum to get access to the GaiaNDAccess
         #  instance and use that to re-extract the data.
         set cubeaccessor [$gaiacube get_cubeaccessor]

         #  Set name of the spectrum.
         set shortname [$cubespectrum sectioned_name]
         if { $shortname == {} } {
            set shortname "$filename"
         }

         #  Create the spectral FITS file and add data from the last
         #  spectrum that was extracted.
         set specaccessor [$cubeaccessor createspectrum "FITS" $filename \
                              $shortname]
         $specaccessor close

         blt::busy release $w_
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaCube instance.
   public variable gaiacube {}

   #  The GaiaCubeSpectrum instance.
   public variable cubespectrum {}

   #  The menu that we will add our save actions to.
   public variable menu {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The top-level window of our menu.
   protected variable w_ {}

   #  Attributes that a DSBSpecFrame may have. Need to save these in the text
   #  format.
   protected variable specframe_atts_ {
      System
      Unit
      AlignSystem
      Domain
      Epoch
      Format
      Label
      ObsLat
      ObsLon
      Symbol
      Title
      AlignStdOfRest
      RefDec
      RefRA
      RestFreq
      SourceVRF
      SourceVel
      StdOfRest
      DSBCentre
      IF
      SideBand
      ImageFreq
   }

   #  Common variables: (shared by all instances)
   #  -----------------
}
