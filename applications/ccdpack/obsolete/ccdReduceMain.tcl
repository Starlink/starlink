proc ccdReduceMain {} {
#+
#  Name:
#    ccdReduceMain

#  Purpose:
#    Main routine of reduce script.

#  Language:
#     Tcl script.

#  Invocation:
#     ccdReduceMain

#  Description:
#     This is the main routine for CCDPACK automated reductions
#     performed using a command-line environment. Use the X version if
#     you can.

#  Notes:
#     This interface is only known to work with Tcl7.4p2.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     18-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.

   global CCDdir
   global env
   global CCDdetectors
   global CCDglobalpars
#.


#  Initialisation.
#  Set the global variable which controls where to pickup source etc.
   if { [ info exists env(CCDPACK_DIR) ] } {
      set CCDdir $env(CCDPACK_DIR)
   } else {
      set CCDdir /star/bin/ccdpack
   }

#  Define the autoload path etc.
   lappend auto_path $CCDdir

#  Defaults for CCD geometry.
   set CCDglobalpars(EXTENT) ""
   set CCDglobalpars(DIRECTION) ""
   set CCDglobalpars(BOUNDS) ""

#  Introduction.
   ccdPagedOutput {

                   REDUCE -- an automated CCD reduction aid.
                   =========================================

  Before you can perform an "automated" reduction you need to identify and
  type your data. It may also be necessary to supply some information about
  the CCD "characteristics".

  For some CCD/telescope combinations all this information (including the type
  of data) can be determined and all you may need to do is identify the
  detector/telescope combination you have used and the names of your data
  files. For others some information about the CCD characteristics has been
  determined. In this case all you need to do is organize your data into the
  different "frame types".

  If no information is available about your CCD then you will need to
  determine how the data is to be processed (in particular how the debiassing
  will be done) and you will then need to determine certain geometric
  properties such as positions of the bias strips, what the useful area of the
  CCD is etc. If you are unsure about these then consult the documentation
  supplied by the observatories, talk to local experts and, as a last resort,
  read SUN/139.

  }

#  Offer to display known detectors.
   set reply \
      [ccdInquireBoolean "Do you want to inspect the known detectors?"]

#  And select one if appropriate.
   if { $reply == "y" } {
      set detector [ccdChooseDetector]
      if { $detector != -1 } {

#  Detector chosen act on its type.
         set action $CCDdetectors($detector,type)
      }
   } else {
      set detector -1
   }

#  If detector is set at -1 after this section then no choice has been
#  made about how to proceed. So we must ask.
   if { $detector == -1 } {
      ccdPagedOutput {

  Since you have chosen not to, or have been unable to, select from the list
  of known detectors, you now need to proceed either by organizing your data
  into their different frame types "by hand" or by supplying an import control
  table.

  Import control tables tell how the information already in your data
  (supplied as FITS values by the observatories) can be used to determine the
  frame types and the CCD characteristics. More information about these can be
  found in SUN/139. It is unlikely that one of these is available unless you
  have created it yourself or have obtained one from a colleague.

      }
      set action [ccdInquireBoolean "Will you set frame types manually" y]
      if { $action == "y" } {
         set CCDdetectors($detector,type) "(setup)"
         ccdPagedOutput {

  Do you want to select a file for restoring the setup from a previous
  reduction?

  Restoration files are created by the CCDSETUP program. These contain a
  description of the state of this program and can be used to return it to
  this state. CCDSETUP sets the CCD characteristics (where the bias strips
  are, the useful CCD area etc.) and some more general preferences (such as
  whether to generate data errors).

  It is unlikely that you have a restoration file unless you have created one
  by a previous run of CCDSETUP (this can be done from this procedure), or
  have been given one.

         }
         set action [ccdInquireBoolean "Select a restoration file" n]
         if { $action == "y" } {
            set setup [ccdInquireFile *.DAT]
            if { $setup == "" } {
               puts stderr "!! No restoration file selected"
               exit 1
            } else {
               set CCDdetectors($detector,file) $setup
            }
         }
      } else {
         set CCDdetectors($detector,type) "(table)"

#  Make sure we have a table.
         set table [ccdInquireFile *.DAT]
         if { $table == "" } {
            puts stderr "!! No table selected"
            exit 1
         } else {
            set CCDdetectors($detector,file) $table
         }
      }
   }

#  Ok now perform the required setup.
   if { $CCDdetectors($detector,type) == "(table)" } {
      ccdPagedOutput {

  Clearing any existing irrelevant CCDPACK global parameters.

      }
      ccdRunTermTask ccdclear byname=true \
         names='extent,direction,bounds,adc,rnoise,deferred,saturation'
      ccdPagedOutput {

  Configure the general CCDPACK options.

  If you do not have any preferences then accept the defaults. If you do not
  have a data mask then make sure that you respond with an "!"  (this is the
  null parameter).

  If you do not understand the significance of a parameter then use a \"?\" to
  get help.
      }
      ccdRunTermTask ccdsetup extent=! direction=! bounds=! adc=! rnoise=! \
         deferred=! saturation=! ndfnames=! restore=false save=false
   } else {

#  No table available so need to use a setup file or get all values from
#  user.
      if { [info exists CCDdetectors($detector,file)] } {
         puts "

  Restoring the values of the CCDPACK global parameters using file
  $CCDdetectors($detector,file)...

         "
         ccdRunTermTask ccdsetup restore=true \
            restorefile=$CCDdetectors($detector,file) reset accept
      } else {
         puts {

  Do you wish to clear any existing global parameters (these might be set from
  a previous reduction and may well be irrelevant). If you are not sure then
  clear them.

         }
         set action [ccdInquireBoolean "Clear existing parameters" y]
         if { $action == "y" } {
            puts {

  Clearing any existing CCDPACK global parameters.

            }
            ccdRunTermTask ccdclear byname=false accept
         }

#  Offer advice about this section and the chance to determine the CCD
#  geometries.
         ccdPagedOutput {

  It is vital that you set all parameters necessary for the type of reduction
  that is required. For instance if you intend to debias your frames using
  specifically taken bias frames and you want to fine tune the offset using
  the bias strips then you must set the BOUNDS and DIRECTION parameters. You
  must also set these if you want to debias by interpolation (or
  extrapolation) of the bias strips.  If you have bias strips then you should
  set the EXTENT parameter to exclude these from the final data.

  If you want to generate errors for your data then you must set the
  parameters ADC, RNOISE and GENVAR.
         }

#  Offer a change to determine the CCD geometries now if an X display
#  is available.
         if { [info exists env(DISPLAY)] } {
            ccdPagedOutput {

  Since you have an X display available (DISPLAY variable set) you can
  determine any of the required CCD geometries interactively. To do
  this you must first choose a suitable image for display.

            }
            set reply \
               [ccdInquireBoolean "Do you want to set the CCD geometry now?" y]
            if { $reply == "y" } { ccdGetGeometry }
         }
         puts {

  Now set the CCDPACK global parameters.

  If you are unsure about a parameter accept the default or use a "?"  to get
  help.

         }
         set moreargs "ndfnames=! restore=false reset "
         if { $CCDglobalpars(EXTENT) != "" } {
            append moreargs "extent=\[$CCDglobalpars(EXTENT)\] "
         }
         if { $CCDglobalpars(DIRECTION) != "" } {
            append moreargs "direction=$CCDglobalpars(DIRECTION) "
         }
         if { $CCDglobalpars(BOUNDS) != "" } {
            append moreargs "bounds=\[$CCDglobalpars(BOUNDS)\]"
         }
         ccdRunTermTask ccdsetup $moreargs
      }
   }

#  Import the NDFs.
   if { $CCDdetectors($detector,type) == "(table)" } {

#  All NDFs given in response to one prompt.
      puts {

  Give the names of all the NDFs to be processed.

      }
      ccdRunTermTask import table=$CCDdetectors($detector,file) \
         namelist='REDUCE.NDFS' reset
   } else {
      ccdPagedOutput {

  Organization of data frames.

  Your data frames now need to be identified with the correct frame
  types. This stage also records the information that you gave previously
  (about the CCD characteristics) in the data. If your data have different
  filter types then you will also need to indicate this here (if they all have
  the same filter then just give NONE as the filter type). It may well be
  worth inspecting the output to ensure that the correct information is being
  stored with your data.

  Some of the frame types that are used are:

     o target -- these are the astronomy data
     o flat   -- flatfield frames
     o bias   -- CCD bias frames.

      }
      ccdRunTermTask present simple=false modify=true namelist='REDUCE.NDFS' \
                     reset
   }

#  And finally perform the reduction.
   ccdPagedOutput {

  Schedule a reduction.

  Your data frames will now be inspected to see how they should be reduced.
  You will also be given the opportunity to specify the sort of debiassing
  that you want to do (from a list of possible options given the data
  available). Whether you want to reduce the amount of disk space used by
  removing any intermediary files (you can also remove the original data
  using the options "LOTS", but you shouldn't use this unless absolutely
  sure).

  Finally a reduction will be performed by a background process.

   }
   ccdRunTermTask schedule in='^REDUCE.NDFS' execute=true \
      script=reduce.csh stype=csh exelogfile=reduce.log

#  End of procedure
}

#-----------------------------------------------------------------------------
#  Start procedure.
ccdReduceMain

#  All done so now exit.
exit
# $Id$
