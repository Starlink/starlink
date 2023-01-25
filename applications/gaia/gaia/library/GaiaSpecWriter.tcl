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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

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
      set w [util::FileSelect .\#auto -title "Save spectrum to text file"]
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
      set w [util::FileSelect .\#auto -title "Save spectrum to an NDF"]
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

         #  Use the GaiaCube to get access to the GaiaNDAccess
         #  instance and use that to re-extract the data.
         set cubeaccessor [$gaiacube get_cubeaccessor]

         #  Set name of the spectrum to the extracted spectrum section name.
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
               set mapped [$cubeaccessor ismapped "VARIANCE"]
               if { ! $mapped } {
                  $cubeaccessor map "READ" "VARIANCE"
               }
               set specdatacomp [$specaccessor map "WRITE/BAD" "VARIANCE"]
               set cubespecdatacomp [$cubeaccessor getlastspectrum "VARIANCE"]
               array::copy $cubespecdatacomp $specdatacomp
               if { ! $mapped } {
                  $cubeaccessor unmap "VARIANCE"
               }
            }

            if { [$cubeaccessor exists "QUALITY"] } {
               set mapped [$cubeaccessor ismapped "QUALITY"]
               if { ! $mapped } {
                  $cubeaccessor map "READ" "QUALITY"
               }
               set specdatacomp [$specaccessor map "WRITE/BAD" "QUALITY"]
               set cubespecdatacomp [$cubeaccessor getlastspectrum "QUALITY"]
               array::copy $cubespecdatacomp $specdatacomp
               if { ! $mapped } {
                  $cubeaccessor unmap "QUALITY"
               }
            }

            #  Save the headers describing the pointing and ACSIS meta-data.
            write_extraction_headers_ $cubeaccessor $specaccessor
         }
         $specaccessor close

         blt::busy release $w_
      }
   }

   #  FITS format
   #  -----------

   #  Save the current spectrum to a FITS file.
   public method save_as_fits {} {
      set w [util::FileSelect .\#auto -title "Save spectrum to FITS"]
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

         #  Use the GaiaCube to get access to the GaiaNDAccess
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

         #  Save the headers describing the pointing and ACSIS meta-data.
         #  Only useful for point extraction.
         if { [$cubespectrum last_extracted_type] == "point" } {
            write_extraction_headers_ $cubeaccessor $specaccessor
         }
         $specaccessor close

         blt::busy release $w_
      }
   }

   #  Return a suitable shortname.
   public method get_shortname {} {
      return [$cubespectrum sectioned_name]
   }


   #  If this is a raw ACSIS cube try to extract and record the TSYS and TRX
   #  values and work out the pointing information. If this isn't an ACSIS
   #  cube then write standard pointing information.
   protected method write_extraction_headers_ {cubeaccessor specaccessor} {

      $specaccessor fitswrite "        ---- GAIA spectral extraction ----"

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
               set tsys [format "%.4f" $tsys]
               $specaccessor fitswrite TSYS $tsys "System temp" "numeric"
            }

            set trx [$cubeaccessor getdoubleproperty ACSIS \
                        "TRX\($p1,$p2\)"]
            if { $trx != "BAD" } {
               set trx [format "%.4f" $trx]
               $specaccessor fitswrite TRX $trx "Receiver temp" "numeric"
            }

            #  The exposure time is ACS_EXPOSURE(==ton)+
            #  ACS_OFFEXPOSURE(==toff).
            set ton [$cubeaccessor getdoubleproperty JCMTSTATE \
                        "ACS_EXPOSURE\($p2\)"]
            set toff [$cubeaccessor getdoubleproperty JCMTSTATE \
                         "ACS_OFFEXPOSURE\($p2\)"]
            if { $ton != "BAD" && $toff != "BAD" } {
               set exptime [format "%.4f" [expr $ton+$toff]]
               $specaccessor fitswrite EXTIME $exptime "Exposure time" \
                  "numeric"
            }

            #  Attempt to determine the pointing information.
            set rawacsis [add_acsis_coords_ \
                             $cubeaccessor $specaccessor $p1 $p2]
         }
      }

      #  Not a timeseries cube could be a SMURF/ACSIS cube. In
      #  that case we can get the exposure time, plus the TSYS.
      if { ! $rawacsis && [$cubeaccessor extensionexists "SMURF"] } {

         set isacsis 1

         #  Must be extracting spectra. These are axis 3. The image
         #  positions index the TSYS, EXP_TIME and EFF_TIME arrays.
         #  Note that the size of these arrays must match the image
         #  dimensions, otherwise they are assumed to contain invalid data.
         lassign [$cubeaccessor getlastspectruminfo] type axis alow ahigh p1 p2
         if { $axis == 3 } {
            set cmpdims [$cubeaccessor getpropertydims "SMURF" \
                            "TSYS.DATA_ARRAY.DATA"]
            set ndfdims [$cubeaccessor getdims 0]

            if { $cmpdims != "" &&
                 [lindex $cmpdims 0] == [lindex $ndfdims 0] &&
                 [lindex $cmpdims 1] == [lindex $ndfdims 1] } {

               set tsys [$cubeaccessor getdoubleproperty SMURF \
                            "TSYS.DATA_ARRAY.DATA\($p1,$p2\)"]
               if { $tsys != "BAD" } {
                  set tsys [format "%.4f" $tsys]
                  $specaccessor fitswrite TSYS $tsys "System temp" "numeric"
               }

               set exptime [$cubeaccessor getdoubleproperty SMURF \
                               "EXP_TIME.DATA_ARRAY.DATA\($p1,$p2\)"]
               if { $exptime != "BAD" } {
                  set exptime [format "%.4f" $exptime]
                  $specaccessor fitswrite EXTIME $exptime "Exposure time" \
                     "numeric"
               }

               set efftime [$cubeaccessor getdoubleproperty SMURF \
                               "EFF_TIME.DATA_ARRAY.DATA\($p1,$p2\)"]
               if { $efftime != "BAD" } {
                  set efftime [format "%.4f" $efftime]
                  $specaccessor fitswrite EXEFFT $efftime \
                     "Effective exposure time (x4)" "numeric"
               }
            }
         }
      }

      if { ! $rawacsis } {
         #  Record the world coordinates of this position. These
         #  document the extraction for other applications. Note we fall
         #  back to this when raw ACSIS extraction also fails.
         add_fits_coords_ $specaccessor
      }
   }

   #  Add world coordinates and offsets that describe a point extracted
   #  spectrum as various FITS cards. Note all in same world coordinate
   #  system.
   protected method add_fits_coords_ {specaccessor} {

      #  Not fatal if this fails.
      catch {
         lassign [$cubespectrum get_last_coords] \
            ra dec xra xdec dra ddec refra refdec drefra drefdec
         if { $ra != {} } {
            $specaccessor fitswrite EXRA  $ra \
               "Image centre for spectral extraction" "char"
            $specaccessor fitswrite EXDEC $dec \
               "Image centre for spectral extraction" "char"
            $specaccessor fitswrite EXRAX $xra \
               "Position of spectral extraction" "char"
            $specaccessor fitswrite EXDECX $xdec \
               "Position of spectral extraction" "char"
            $specaccessor fitswrite EXRAOF  $dra \
               "Offset from EXRA (arcsec)" "numeric"
            $specaccessor fitswrite EXDECOF $ddec \
               "Offset from EXDEC (arcsec)" "numeric"

            $specaccessor fitswrite EXRRA  $refra \
               "Reference centre for spectral extraction" "char"
            $specaccessor fitswrite EXRDEC $refdec \
               "Reference centre for spectral extraction" "char"
            $specaccessor fitswrite EXRRAOF  $drefra \
               "Offset from EXRRA (arcsec)" "numeric"
            $specaccessor fitswrite EXRDECOF $drefdec \
               "Offset from EXRDEC (arcsec)" "numeric"
         }
      }
   }

   #  Determine the ACSIS pointing for the current spectrum. Uses the
   #  ACSIS and JCMTSTATE extensions of a raw timeseries NDF.
   protected method add_acsis_coords_ {cubeaccessor specaccessor p1 p2} {

      set rra {}
      set rdec {}
      set sra {}
      set sdec {}
      set drra {}
      set drdec {}
      set dateobs {}
      set system {}
      set equinox {}

      #  Receptor system, either AZEL or TRACKING.
      set rsys [$cubeaccessor getproperty ACSIS "RECEPPOS_SYS"]
      if { $rsys != {} } {

         #  TCS system.
         set tcssys [$cubeaccessor getproperty JCMTSTATE "TCS_TR_SYS\($p2\)"]

         #  Transform into AST name for that system.
         if { $tcssys != {} } {
            set tcssys [jcmt_tracking_to_ast_ $tcssys]
         }

         #  If receptor system is TRACKING that's in some sky coordinate
         #  system which is the tcssys.
         if { $rsys == "TRACKING" && $tcssys != {} } {
            set rsys $tcssys
         }

         if { $rsys != {} } {

            #  Receptor position in radians.
            set rx [$cubeaccessor getdoubleproperty ACSIS \
                       "RECEPPOS\(1,$p1,$p2\)"]
            set ry [$cubeaccessor getdoubleproperty ACSIS \
                       "RECEPPOS\(2,$p1,$p2\)"]

            #  Source position in radians.
            set sx [$cubeaccessor getdoubleproperty JCMTSTATE \
                       "TCS_TR_BC1\($p2\)"]
            set sy [$cubeaccessor getdoubleproperty JCMTSTATE \
                       "TCS_TR_BC2\($p2\)"]

            if { $rx != "BAD" && $ry != "BAD" } {

               #  Observer longitude and latitude.
               set lonobs [$cubeaccessor getproperty FITS "LONG-OBS"]
               set latobs [$cubeaccessor getproperty FITS "LAT-OBS"]
               if { $latobs != {} && $lonobs != {} } {

                  set lonobs [string trim $lonobs]
                  set latobs [string trim $latobs]

                  #  Determine the Epoch, needed for AZEL & GAPPT.
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
                  set atts "System=$rsys,Epoch=MJD $epoch,\
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

                  #  If the source position is GAPPT then need to convert
                  #  the receptor position to GAPPT to get the offset.
                  #  We do not calculate a source position for GAPPT as
                  #  it is moving.
                  if { $tcssys == "GAPPT" } {
                     set toframe [gaiautils::astcopy $skyframe]
                     gaiautils::astset $toframe "system=GAPPT,Digits=9"
                     set wcs \
                        [gaiautils::astconvert $skyframe $toframe "SKY"]

                     lassign [gaiautils::asttran2 $wcs $rx $ry] grx gry

                     #  Determine the separation between these positions.
                     lassign [gaiautils::astaxoffset \
                                 $wcs $grx $gry $sx $sy] drra drdec
                     gaiautils::astannul $toframe
                     gaiautils::astannul $wcs
                  }

                  #  If the receptor system is AZEL, convert to FK5 for
                  #  display.
                  if { $rsys == "AZEL" } {

                     set toframe [gaiautils::astcopy $skyframe]
                     gaiautils::astset $toframe "system=FK5,Digits=9"
                     set wcs \
                        [gaiautils::astconvert $skyframe $toframe "SKY"]

                     #  Transform and format values in FK5.
                     lassign [gaiautils::asttran2 $wcs $rx $ry] rx ry
                     set rra [gaiautils::astformat $wcs 1 $rx]
                     set rdec [gaiautils::astformat $wcs 2 $ry]

                     if {$sx != "BAD" && $sy != "BAD" && $tcssys != "GAPPT"} {

                        #  Source position. When rsys is AZEL the TCS system
                        #  may differ, so we need to transform from tcssys to
                        #  FK5 also. Note done when in GAPPT, source is moving.
                        gaiautils::astset $wcs "invert=1"
                        gaiautils::astset $wcs "system=$tcssys"

                        #  Transform and format values in FK5.
                        lassign [gaiautils::asttran2 $wcs $sx $sy] sx sy
                        set sra [gaiautils::astformat $wcs 1 $sx]
                        set sdec [gaiautils::astformat $wcs 2 $sy]

                        #  Determine the separation between these positions.
                        lassign [gaiautils::astaxoffset \
                                    $wcs $rx $ry $sx $sy] drra drdec
                     }

                     #  Switch wcs to skyframe, so we get the right system
                     #  recorded.
                     gaiautils::astannul $skyframe
                     set skyframe $wcs
                     gaiautils::astannul $toframe
                  } else {

                     #  Receptor system not AZEL, so just format.
                     set rra [gaiautils::astformat $skyframe 1 $rx]
                     set rdec [gaiautils::astformat $skyframe 2 $ry]

                     if {$sx != "BAD" && $sy != "BAD" && $tcssys != "GAPPT"} {

                        #  Same for source position, is same when not AZEL or
                        #  GAPPT.
                        set sra [gaiautils::astformat $skyframe 1 $sx]
                        set sdec [gaiautils::astformat $skyframe 2 $sy]

                        #  Determine the separation between these positions.
                        lassign [gaiautils::astaxoffset \
                                    $skyframe $rx $ry $sx $sy] drra drdec
                     }
                  }

                  #  The DATE-OBS keyword for this spectrum can be defined
                  #  more accurately as the Epoch, so do that. Note we need to
                  #  move back to TAI from TDB and then to UTC.
                  set epoch [expr $epoch - (32.184/86400.0)]
                  set leaps [sla::dat $epoch]
                  set epoch [expr $epoch - ($leaps/86400.0)]
                  set dateobs [sla::datemjd2obs $epoch]

                  #  Record system and equinox of coordinates.
                  set system [gaiautils::astget $skyframe "System"]
                  if { $system == "FK5" || $system == "FK4" } {
                     set equinox [gaiautils::astget $skyframe "Equinox"]
                     if { $equinox < 1984.0 } {
                        set equinox "B$equinox"
                     } else {
                        set equinox "J$equinox"
                     }
                  } else {
                     set equinox {}
                  }
                  gaiautils::astannul $skyframe
               }
            }
         }
      }

      if { $rra != {} && $rdec != {} } {
         $specaccessor fitswrite EXRAX  $rra "Spectral extraction position" \
            "char"
         $specaccessor fitswrite EXDECX $rdec "Spectral extraction position" \
            "char"
         $specaccessor fitswrite EXSYS "$system $equinox" \
            "Extraction coordinate system" "char"

         if { $sra != {} && $sdec != {} && $tcssys != "GAPPT" } {
            $specaccessor fitswrite EXRRA  $sra "Position of source" "char"
            $specaccessor fitswrite EXRDEC $sdec "Position of source" "char"
         }

         if { $drra != {} && $drdec != {} } {

            #  Values are assumed to be radians, want arcsecs.
            set drra [format "%.4f" [expr $drra*3600.0*180.0/$PI_]]
            set drdec [format "%.4f" [expr $drdec*3600.0*180.0/$PI_]]

            $specaccessor fitswrite EXRRAOF  $drra "Offset of extraction" \
               "numeric"
            $specaccessor fitswrite EXRDECOF $drdec "Offset of extraction" \
               "numeric"
         }

         if { $dateobs != {} } {
            $specaccessor fitswrite DATE-OBS $dateobs "Time of observation" \
               "char"
         }
         return 1
      }
      return 0
   }

   #  Convert JCMT TRACKING system into the equivalent AST celestial
   #  coordinate system. Returns input if no match (OK for some systems).
   protected method jcmt_tracking_to_ast_ {rsys} {
      switch -glob "$rsys" {
         J2* {
            set rsys "FK5"
         }
         B19* {
            set rsys "FK4"
         }
         APP* {
            set rsys "GAPPT"
         }
      }
      return $rsys
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

   common PI_ 3.14159265358979323846264338328
}
