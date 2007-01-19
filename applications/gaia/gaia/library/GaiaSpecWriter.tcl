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

            #  Get spectral axis of dataset WCS.
            set canvas [$gaiaspectrum component canvas]
            set axis [$canvas itemcget $spectrum -axis]

            #  Get spectral WCS (need to pick out 1D from full WCS).
            set cubeaccessor [$gaiacube get_cubeaccessor]
            set frameset [$cubeaccessor getaxiswcs $axis 0]

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

         #  Check for other data components and copy them too, but only
         #  if this is a point spectrum. When averaged over some region
         #  a simple extraction would be incorrect.
         if { [$cubespectrum last_extracted_type] == "point" } {
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

            #  If this is a raw ACSIS cube we will try to extract and record
            #  the TSYS and TRX values and work out the pointing information.
            set rawacsis 0
            if { [$cubeaccessor extensionexists "ACSIS"] &&
                 [$cubeaccessor extensionexists "JCMTSTATE"] } {

               #  Must be extracting spectra. These are axis 1. The image
               #  positions are time and receptor index, these index the
               #  TSYS and TRX arrays.
               lassign [$cubeaccessor getlastspectruminfo] \
                  type axis alow ahigh p1 p2
               if { $axis == 1 } {
                  set tsys [$cubeaccessor getdoubleproperty ACSIS \
                               "TSYS\($p1,$p2\)"]
                  if { $tsys != "BAD" } {
                     $specaccessor fitswrite TSYS $tsys "Median system temp"
                  }

                  set trx [$cubeaccessor getdoubleproperty ACSIS \
                              "TRX\($p1,$p2\)"]
                  if { $trx != "BAD" } {
                     $specaccessor fitswrite TRX $trx "Receiver temp"
                  }

                  #  Attempt to determine the pointing information.
                  set rawacsis [add_acsis_coords_ \
                                   $cubeaccessor $specaccessor $p1 $p2]
               }
            }

            if { ! $rawacsis } {
               #  Record the world coordinates of this position. These
               #  document the extraction for other applications. Note we fall
               #  back to this when raw ACSIS extraction also fails.
               add_fits_coords_ $specaccessor
            }
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
         set shortname [get_shortname]
         if { $shortname == {} } {
            set shortname "$filename"
         }

         #  Create the spectral FITS file and add data from the last
         #  spectrum that was extracted.
         set specaccessor [$cubeaccessor createspectrum "FITS" $filename \
                              $shortname]


         #  Record the world coordinates of this position. These
         #  document the extraction for other applications.
         add_fits_coords_ $specaccessor

         $specaccessor close

         blt::busy release $w_
      }
   }

   #  Return a suitable shortname.
   public method get_shortname {} {
      return [$cubespectrum sectioned_name]
   }

   #  Add world coordinates and offsets that describe a point extracted
   #  spectrum as various FITS cards. Note all in same world coordinate
   #  system.
   protected method add_fits_coords_ {specaccessor} {

      #  Not fatal if this fails.
      catch {
         lassign [$cubespectrum get_last_coords] \
            ra dec xra xdec dra ddec refra refdec drefra drefdec
         if { $ra != "" } {
            $specaccessor fitswrite EXRA  $ra \
               "Image centre for spectral extraction"
            $specaccessor fitswrite EXDEC $dec \
               "Image centre for spectral extraction"
            $specaccessor fitswrite EXRAX $xra \
               "Position of spectral extraction"
            $specaccessor fitswrite EXDECX $xdec \
               "Position of spectral extraction"
            $specaccessor fitswrite EXRAOF  $dra \
               "Offset from EXRA (arcsec)"
            $specaccessor fitswrite EXDECOF $ddec \
               "Offset from EXDEC (arcsec)"

            $specaccessor fitswrite EXRRA  $refra \
               "Reference centre for spectral extraction"
            $specaccessor fitswrite EXRDEC $refdec \
               "Reference centre for spectral extraction"
            $specaccessor fitswrite EXRRAOF  $drefra \
               "Offset from EXRRA (arcsec)"
            $specaccessor fitswrite EXRDECOF $drefdec \
               "Offset from EXRDEC (arcsec)"
         }
      }
   }

   #  Determine the ACSIS pointing for the current spectrum.
   protected method add_acsis_coords_ {cubeaccessor specaccessor p1 p2} {

      set rra {}
      set rdec {}
      set sra {}
      set sdec {}
      set dateobs {}

      #  System, either AZEL or TRACKING.
      set tsys [$cubeaccessor getproperty ACSIS "RECEPPOS_SYS"]
      if { $tsys != "" } {

         #  If TRACKING that's in some sky coordinate system.
         if { $tsys == "TRACKING" } {
            set tsys \
               [$cubeaccessor getproperty JCMTSTATE "TCS_TR_SYS\($p2\)"]
            if { $tsys != {} } {

               #  Transform into AST name for that system.
               set tsys [jcmt_tracking_to_ast_ $tsys]
            }
         }

         if { $tsys != {} } {
            #  Receptor position in radians.
            set rx [$cubeaccessor getdoubleproperty ACSIS \
                       "RECEPPOS\(1,$p1,$p2\)"]
            set ry [$cubeaccessor getdoubleproperty ACSIS \
                       "RECEPPOS\(2,$p1,$p2\)"]

            #  Source position in radians. Only when TSYS != GAPPT, as
            #  the reference position moves on the sky.
            if { $tsys != "GAPPT" } {
               
               set sx [$cubeaccessor getdoubleproperty JCMTSTATE \
                          "TCS_TR_BC1\($p2\)"]
               set sy [$cubeaccessor getdoubleproperty JCMTSTATE \
                          "TCS_TR_BC2\($p2\)"]
            } else {
               set sx "BAD"
               set sy "BAD"
            }

            if { $rx != "BAD" && $ry != "BAD" } {

               #  Observer longitude and latitude.
               set lonobs [$cubeaccessor getproperty FITS "LONG-OBS"]
               set latobs [$cubeaccessor getproperty FITS "LAT-OBS"]
               if { $latobs != {} && $lonobs != {} } {

                  set lonobs [string trim $lonobs]
                  set latobs [string trim $latobs]

                  #  Determine the Epoch, needed for AZEL.
                  #  Correct to TAI to TDB.
                  set epoch [$cubeaccessor getdoubleproperty JCMTSTATE \
                                "TCS_TAI\($p2\)"]
                  if { $epoch == "BAD" } {
                     set epoch [$cubeaccessor getdoubleproperty JCMTSTATE \
                                   "RTS_END\($p2\)"]
                     set steptime [$cubeaccessor getproperty FITS "STEPTIME"]
                     set epoch [expr $epoch + (32.184-0.5*$steptime)/86400.0]
                  } else {
                     set epoch [expr $epoch + (32.184/86400.0)]
                  }

                  #  Gather all information.
                  set atts "System=$tsys,Epoch=MJD $epoch,\
                            Obslat=$latobs,Obslon=$lonobs,Digits=9"

                  #  Correction from UT1 to UTC, if known (value in days).
                  #  Needs to be correct for the epoch of observation.
                  set dut1 [$cubeaccessor getproperty FITS "DUT1"]
                  if { $dut1 != {} } {
                     set dut1 [expr $dut1*86400.0]
                     append atts ",DUT1=$dut1"
                  }

                  #  Create the SkyFrame.
                  set skyframe [gaiautils::astskyframe $atts]

                  #  If the tsys is AZEL then transform to FK5.
                  if { $tsys == "AZEL" } {
                     set toframe [gaiautils::astcopy $skyframe]
                     gaiautils::astset $toframe "system=FK5"

                     set wcs [gaiautils::astconvert $skyframe $toframe "SKY"]

                     lassign [gaiautils::asttran2 $wcs $rx $ry] rx ry
                     
                     if { $sx != "BAD" && $sy != "BAD" } {
                        lassign [gaiautils::asttran2 $wcs $sx $sy] sx sy
                     }

                     gaiautils::astannul $skyframe
                     gaiautils::astannul $wcs
                     set skyframe $toframe
                  }

                  #  Format the receptor position for display.
                  set rra [gaiautils::astformat $skyframe 1 $rx]
                  set rdec [gaiautils::astformat $skyframe 2 $ry]
                  
                  #  Same for source position, if available.
                  if { $sx != "BAD" && $sy != "BAD" } {
                     set sra [gaiautils::astformat $skyframe 1 $sx]
                     set sdec [gaiautils::astformat $skyframe 2 $sy]
                  }

                  #  The DATE-OBS keyword for this spectrum can be defined
                  #  more accurately as the Epoch, so do that. Note we need to
                  #  move back to TAI (/UTC) from TDB.
                  set epoch [expr $epoch - (32.184/86400.0)]
                  set dateobs [sla::datemjd2obs $epoch]

                  gaiautils::astannul $skyframe
               }
            }
         }
      }

      if { $rra != {} && $rdec != {} } {
         $specaccessor fitswrite EXRAX  $rra "Spectral extraction position"
         $specaccessor fitswrite EXDECX $rdec "Spectral extraction position"
         $specaccessor fitswrite EXSYS $tsys "Extraction coordinate system"

         if { $sra != {} && $sdec != {} } {
            $specaccessor fitswrite EXRRA  $sra "Position of source"
            $specaccessor fitswrite EXRDEC $sdec "Position of source"
         }
         
         if { $dateobs != {} } {
            $specaccessor fitswrite DATE-OBS $dateobs "Time of observation"
         }
         return 1
      }
      return 0
   }

   #  Convert JCMT TRACKING system into the equivalent AST celestial
   #  coordinate system. Returns input if no match (OK for some systems).
   protected method jcmt_tracking_to_ast_ {tsys} {
      switch -glob "$tsys" {
         J2* {
            set tsys "FK5"
         }
         B19* {
            set tsys "FK4"
         }
         APP* {
            set tsys "GAPPT"
         }
      }
      return $tsys
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
