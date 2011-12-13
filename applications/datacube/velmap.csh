#!/bin/csh
#+
#
#  Name:
#     velmap.csh
#
#  Purpose:
#     Builds a velocity map of an emission line from a spectral-cube NDF
#     by line fitting.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     velmap [-a] [-c number] [-ci index] [-f] [-i filename] [-l filename]
#            [-o filename] [-p] [-r number] [-s system] [-v] [-z/+z]
#
#  Description:
#     This shell script reads a three-dimensional spectral-cube NDF and
#     presents you with a white-light image of the cube.  You can then select
#     an X-Y position using the cursor.  The script will extract and display
#     this reference spectrum.  You will then be prompted to specify various
#     fitting parameters, such as the peak position, using the cursor.  The
#     script will then attempt to fit the emission line.  The fit will be
#     displayed and you are consulted regarding the goodness of fit.  If you
#     consider the fit to be good enough, the script will attempt to perform
#     similar fits to all spectra within the cube, building a two-dimensional
#     NDF image of the velocity of the line.  These will use the same initial
#     parameters as the reference spectrum, unless option -a is selected.
#     You may view this image drawn with a key (option -d), and overlay a
#     contour plot (with a key) of the white-light image (option -c).
#
#     If you do not force the fit to be considered "good" by using the -f
#     command-line option, the script will offer the opportunity to manually
#     refit the spectral feature for individual pixels, such as those that
#     were unsuccessfully fitted by the automatic procedure.  In this case
#     the velocity map will be plotted and replotted after the new fit,
#     regardless of the -p option.
#
#  Parameters:
#     -a
#       Requests that each fit may be inspected then approved or re-fit, not
#       just the initial reference fit.  A re-fit will change the initial
#       parameter guesses for subsequent fits, so it is recommended that you
#       note the co-ordinates of spectra to re-fit and tackle these
#       individually in the final manual re-fit stage.  [FALSE]
#     -c number
#       Number of contours in the white-light image.  Set to fewer
#       than 1 means no contours are overlaid.  [15]
#     -ci index
#       The palette colour index of the contours.  It should be an
#       integer in the range 0 to 15.  It is best to choose an index
#       corresponding to white, or black or another dark colour to make
#       the contours stand out from other elements of the plot.  0 is
#       the background colour.  KAPPA:GDSTATE will list the current
#       palette colours.  [0]
#     -f
#       Force the script to accept the first attempt to fit a Gaussian to
#       the line. This is a dangerous option; if the fit is poor, or
#       unobtainable the script may terminate abruptly if it is forced to
#       accept the fit.  This will additionally suppress manual re-fitting
#       of bad pixels at the end of the run of the script.  [FALSE]
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for
#       the input file.
#     -l filename
#       The name of an text log file containing the fitted Gaussian
#       coefficients for each spatial pixel.  The file is written as a
#       Starlink Small Text List (STL) described in SUN/190.  The STL file
#       comprises a schema to locate and describe the columns, and store
#       global properties; and a formatted table of the coefficients.  The
#       schema includes the units and a brief description of each column, and
#       the name of the input NDF used.  The table lists the Gaussian centre,
#       peak height the FWHM, and integrated flux, each with its fitting error.
#     -o filename
#       The filename for the output NDF of the velocity map.
#     -p
#       The script will plot the final image map to the current display
#       as well as saving it to an NDF file.  It will additionally overplot
#       the white-light image as a contour map for comparison.  [FALSE]
#     -r number
#       Rest-frame spectral unit of the line being fitted.
#     -s system
#       The co-ordinate system for velocities.  Allowed values are:
#          "VRAD" -- radio velocity;
#          "VOPT" -- optical velocity;
#          "ZOPT" -- redshift; and
#          "VELO" -- relativistic velocity.
#       If you supply any other value, the default is used.  ["VOPT"]
#     -v
#       The script will generate a variance array from the line fits and
#       attach it to the velocity-map NDF.  [FALSE]
#     -z
#       The script will automatically prompt to select a region to zoom
#       before prompting for the region of interest.  [TRUE]
#     +z
#       The script will not prompt for a zoom before requesting the region
#       of interest.  [FALSE]
#
#  Notes:
#     -  The velocity-map display scales between the 2 and 98 percentiles.
#     The map uses a false-colour spectrum-like colour table so that
#     low-velocity regions appear in blue and high-velocity regions
#     appear in red.
#     -  CURSA:CATCOPY may be used to convert the STL log file (see the -l
#     option) to FITS format for analysis with the likes of TOPCAT, provided
#     the STL has the ".txt" file extension.  If you want just the tabulated
#     data for your own favourite tool, the schema can be easily removed
#     manually, or with sed excluding the lines up to and including the line
#     beginning "BEGINTABLE".
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA and
#     Figaro packages.
#
#  Copyright:
#     Copyright (C) 2000-2006 Central Laboratory of the Research Councils.
#     Copyright (C) 2007-2008 Science and Technology Research Council.
#     All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either Version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#     02110-1301, USA.
#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     04-SEP-2000 (AALLAN):
#       Original version.
#     06-SEP-2000 (AALLAN):
#       Heavy modifications.
#     13-SEP-2000 (AALLAN):
#       Rewritten on the train.
#     18-SEP-2000 (AALLAN):
#       Major rewrite to make full use of parameter system.
#     19-SEP-2000 (AALLAN):
#       Moved from using bc to using KAPPA calc for some calculations.
#     31-OCT-2000 (AALLAN):
#       Fixed some bugs.
#     05-NOV-2000 (AALLAN):
#       Added variance arrays.
#     09-NOV-2000 (AALLAN):
#       Changed input method of inputing the continuum measurement.
#     10-NOV-2000 (AALLAN):
#       Added bad fit check, modified plot colour for contour lines.
#     12-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#     23-NOV-2000 (AALLAN):
#       Added lots of command line options.
#       Added interrupt handling.
#     13-DEC-2000 (AALLAN):
#       Added manual refitting of bad data values.
#     31-DEC-2000 (AALLAN):
#       Allowed single-character responses to yes/no prompts.
#     08-JAN-2001 (AALLAN):
#       Variance calculation made more robust.
#       Fixed bug in magic value propogation.
#       Major bug in manual refitting routine fixed.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}.
#     2005 September 1 (MJC):
#       Replaced COPYAXIS with KAPPA:SETAXIS.
#     2005 September 2 (MJC):
#       Replaced PUTAXIS with KAPPA:SETAXIS in WCS mode.  Some tidying:
#       remove tabs, spelling corrections.  Added section headings in the
#       code.  Replace explicit wavelength in prompts with the current WCS
#       Frame's label for the spectral axis, and also used the corresponding
#       units in the output commentary.  Avoid :r.
#     2005 September 7 (MJC):
#       Convert the current WCS System into velocities.
#     2005 October 11 (MJC):
#       Added PARGET calls to access velocities and sent wcstran output to
#       dev/null.  Added -c option for contour levels.  Fixed bugs converting
#       the cursor position into negative pixel indices.  Ensured that the
#       plots of spectra use the axis co-ordinate system.  Called KAPPA:CALC
#       for floating-point arithmetic using appropriate precision, and ensure
#       a digit before a decimal point, unlike bc.  Plot spectra in histogram
#       mode for clarity.  Added defaults to parameter descriptions.
#       Appended data unit to reported peak values.  Inserted blank lines to
#       structure the commentary better.  Correct some output and comments:
#       e.g. "spectra" singular to "spectrum".
#     2005 October 21 (MJC):
#       Test for existence of RestFreq, and use it where available instead
#       of prompting the user.  Remove second prompt for Rest-frame
#       co-ordinate.  Ensure that the output velocities are in km/s.
#       Added -s option.  Align some commentary.  WCSTRAN calls for fixing
#       individual pixels now uses the correct NDF.  Bug deriving the
#       velocity of refitted point corrected, by resetting the WCS Frame,
#       System and Unit before transforming.
#     2005 November 1 (MJC):
#       Allow for UK data-cube format by creating a SPECTRUM Frame
#       with a Wavelength system and Angstrom units in the extracted
#       spectra; reset unclear labels to Wavelength.  For other cubes, search
#       for the SPECTRUM-domain Frame and select it in the extracted spectra,
#       if SPECTRUM is not already the current WCS Frame.  Assign Angstrom to
#       the spectral unit, if this is is not specified.  The prompt for the
#       rest-frame co-ordinate now includes the unit.
#     2005 November 2 (MJC):
#       Added a labelled velocity-map key to the right of the list of contour
#       heights.
#     2005 November 3 (MJC):
#       Add options waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.  Recognise
#       SKY-DSBSPECTRUM.  Corrected contour-mode value typo's and set
#       axis units for ripped file in the loop.
#     2006 March 6 (MJC):
#       Switched lower percentile for display from 15 to 2.  Allow -c
#       to be zero to mean no contours overlaid.
#     2006 March 9 (MJC):
#       Added -ci option.  Corrected the NDF name extraction when both
#       the file extension and an NDF section are supplied; this is via the
#       new checkndf script that also checks for a degenerate third axis.
#     2006 March 10 (MJC):
#       Switched from "colour" to spectrum colour table.
#     2006 March 16 (MJC):
#       Corrected the logic when deciding whether or not to create a new
#       SpecFrame.  Use any supplied spectral-axis section when ripping
#       each spectrum for display and line fitting.
#     2007 May 5 (MJC):
#       Call the new getfitcon script to determine the fit's initial-guess
#       parameters.  Format velocity errors in single-precision G.
#     2008 June 21 (MJC):
#       Added -a and -l options.  Provide case insensitive prompting.
#       Remove overloading of the fitgood variable.  Fixed bug that
#       stored the velocity error as the variance in the output NDF.
#       Remove duplicate token.  Added Licence.
#     2008 June 25 (MJC):
#       Added STL schema for log of fit parameters.  Recognised DSBSPECTRUM
#       as valid spectral domain.
#     2008 June 30 (MJC):
#       Fixed bugs: WCS Frame was not reset before re-plotting in final
#       individual-pixel refit loop.  Disentangled the logic for deciding
#       whether to create or switch WCS Frames.
#     {enter_further_changes_here}
#
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Clean up from previous runs.
rm -f ${tmpdir}/${user}/vmap* >& /dev/null
rm -f ${tmpdir}/${user}/?_?.sdf >& /dev/null

# Do the variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set fitfile = "${tmpdir}/${user}/vmap_fitgauss.tmp"
set colfile = "${tmpdir}/${user}/vmap_col"
set ripfile = "${tmpdir}/${user}/vmap_rip"
set mapfile = "${tmpdir}/${user}/vmap_map.dat"
set varfile = "${tmpdir}/${user}/vmap_var.dat"

# Set options access flags.
set approve = "FALSE"
set dovar = "FALSE"
set drawcontours = "TRUE"
set forcefit = "FALSE"
set gotinfile = "FALSE"
set gotlog = "FALSE"
set gotoutfile = "FALSE"
set gotrest = "FALSE"
set gotzoom = "ASK"
set plotspec = "FALSE"

# The SPECDRE extension is used to store the Gaussian fit.
set component = 1

# Specify the default number of contours used to display the white-light
# image.
set numcont = 15

# Other defaults.
set ci = 0
set fitgood = "yes"
set plotdev = "xwin"
set velsys = "VOPT"
set vunits = "km/s"

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -a:    # approve each fit?
      set approve = "TRUE"
      shift args
      breaksw
   case -c:    # Number of contours
      shift args
      set numcont = $args[1]
      if ( $numcont < 1 ) then
         set drawcontours = "FALSE"
      else if ( $numcont > 100 ) then
         set numcont = 15
      endif
      shift args
      breaksw
   case -ci:    # colour index of contours
      shift args
      set ci = `calc exp="nint($args[1])"`
      if ( $ci < 0 || $ci > 15 ) set ci = 0
      shift args
      breaksw
   case -f:    # force fit?
      set forcefit = "TRUE"
      shift args
      breaksw
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -l:    # output log file
      shift args
      set gotlog = "TRUE"
      set logfile = $args[1]
      shift args
      breaksw
   case -o:    # output velocity map
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectra
      set plotspec = "TRUE"
      set plotdev = "${plotdev}"
      shift args
      breaksw
   case -r:    # rest-frame spectral-unit of line
      shift args
      set gotrest = "TRUE"
      set rest_coord = $args[1]
      shift args
      breaksw
   case -s:    # velocity co-ordinate system
      shift args
      set velsys = `echo $args[1] | awk '{print toupper($0)}'`
      if ( $velsys != "VOPT" && $velsys != "VRAD" && \
           $velsys != "ZOPT" && $velsys != "VELO" ) set velsys = "VOPT"
      shift args
      breaksw
   case -v:    # generate variances?
      set dovar = "TRUE"
      shift args
     breaksw
   case -z:    # zoom?
      set gotzoom = "TRUE"
      shift args
      breaksw
   case +z:    # not zoom?
      set gotzoom = "FALSE"
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Invoke the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s velmap
if ( $status == 1 ) exit

# Find out the cube bounds and units.
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`
set unit = `parget units ndftrace`

# Check to see if the NDF has VARIANCE.
set variance = `parget variance ndftrace`

# Show the white-light image.
# ===========================

# Collapse the white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null

# Display the collapsed image.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2] reset" >&/dev/null

# Determine if we need to create or select a SPECTRUM-domain WCS Frame.
# =====================================================================

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# First do we need to change WCS Frames?  Check that the current frame is
# spectral or sky-spectral.  We can re-use the last trace of the ripped
# spectrum.
set change_frame = "FALSE"
set curframe = `parget current ndftrace`
set domain = `parget fdomain"($curframe)" ndftrace`
if ( "$domain" != "SPECTRUM" && "$domain" != "SKY-SPECTRUM" && \
     "$domain" != "DSBSPECTRUM" && "$domain" != "SKY-DSBSPECTRUM" ) then
   set change_frame = "TRUE"
endif

# Next question: is there a spectral frame to switch to in order to
# permit velocity calculations?  Loop through all the WCS Frames, looking
# for a SPECTRUM or a DSBSPECTRUM (or composite SKY-SPECTRUM or
# SKY-DSBSPECTRUM).
set create_specframe = "TRUE"
set nframe = `parget nframe ndftrace`
set i = 1
while ( $i <= $nframe && $create_specframe == "TRUE" )
   set domain = `parget fdomain"($i)" ndftrace`

   if ( "$domain" == "DSBSPECTRUM" || "$domain" == "SKY-DSBSPECTRUM" ) then
      set new_wcsframe = "DSBSPECTRUM"
      set create_specframe = "FALSE"

   else if ( "$domain" == "SPECTRUM" || "$domain" == "SKY-SPECTRUM" ) then
      set new_wcsframe = "SPECTRUM"
      set create_specframe = "FALSE"
   endif

   @ i++
end

# We need to create a WCS SPECTRUM domain, and hence not change frame.
if ( "$create_specframe" == "TRUE" ) then
   set change_frame = "FALSE"

# Check for the old-fashioned UK data-cube format that predates the
# SpecFrame.  Test for non-null units if there is no SpecFrame.  Set the
# label to something clearer than LAMBDA or Axis 3.
   if ( "${slabel}" == "LAMBDA" || "${slabel}" == "Axis 3" ) then
      set slabel = "Wavelength"
      if ( "$sunits" == "" ) then
         set sunits = "Angstrom"
      endif
   else
      echo " "
      echo "The input NDF does not have an SPECTRUM or DSBSPECTRUM "
      echo "WCS Domain or it is not in the UK data-cube format.  Will "
      echo "convert the current frame to a SPECTRUM of with System"
      echo "set to Wavelength to calculate VOPT velocities."
      echo " "

      if ( "$sunits" == "" ) then
         echo "Assuming that the undefined unit is Angstrom."
         set sunits = "Angstrom"
      endif
   endif
endif

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Grab the X-Y position.
echo " "
echo "  Left click on pixel to be extracted."
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# We don't clean up the collapsed white-light image here as normal because
# it will be used later to clone both the AXIS and WCS co-ordinates for the
# new velocity map.

# Extract the spectrum.
# =====================

echo " "
echo "      Extracting:"
echo "        (X,Y) pixel: ${xgrid},${ygrid}"

# Extract the spectrum from the cube.
ndfcopy "in=${infile}(${xgrid},${ygrid},${lbnd[3]}:${ubnd[3]}) out=${ripfile} trim trimwcs"

# Check to see if the NDF has an AXIS structure.  If one does not exist,
# create an array of axis centres, derived from the current WCS Frame,
# along the axis.
set axis = `parget axis ndftrace`

if ( ${axis} == "FALSE" ) then
   setaxis "ndf=${ripfile} dim=1 mode=wcs comp=Centre" >& /dev/null
   echo "        Axes: Adding AXIS centres."
endif

# To compare like with like, ensure that plotting uses the AXIS frame.
wcsframe "ndf=${ripfile} frame=axis"

# Specify the units, in case these weren't known originally.
axunits "ndf=${ripfile} units=${sunits}" >& /dev/null

# Obtain the precision of the axis centres.
# Assuming this is only _REAL or _DOUBLE.
ndftrace ${ripfile} fullaxis >& /dev/null
set prec = `parget atype ndftrace`

if ( ${variance} == "FALSE" ) then
   echo "        Variances: present."
else
   echo "        Variances: absent."
endif

# Indicate that we do not wish to merely replot, but also possibly zoom.
set newfit = " "

# Label for repeated fitting of the Gaussian.
refit:

# Plot the ripped spectrum with or without zooming, and obtain the fit
# estimated parameters.
source ${DATACUBE_DIR}/getfitcon.csh -i ${ripfile} -d ${plotdev} ${newfit} -z ${gotzoom}

# Fit the line.
# =============

echo "      Fitting:"

fitgauss \
    "in=${ripfile} mask1=${low_mask} mask2=${upp_mask} cont=${cont} " \
    "peak=${peak} fwhm=${fwhm} reguess=no remask=no ncomp=1 cf=0 " \
    "pf=0 wf=0 comp=${component} fitgood=${fitgood} dialog=f " \
    "centre=${position} logfil=${fitfile} device=${plotdev}" >& /dev/null

# Check to see whether or not fitting was successful.
if ( ! -e $fitfile ) then
   echo "        No fit available"
   echo ""
   echo -n "Refit (yes/no): "
   set refit = $<
   set refit = `echo ${refit} | awk '{print tolower($0)}'`
   echo " "

   if ( ${refit} == "no" || ${refit} == "n" ) then
      rm -f ${fitfile} >& /dev/null
      goto cleanup

# Try again.
   else
      if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
         set newfit = " "

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
      else
         set newfit = "-r"
      endif
      goto refit
   endif
endif

# Read the fit from the temporary file.
set results = `cat ${fitfile} | head -n 23 | tail -1`
set array = \
   `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

set centre_fit = $array[2]
set centre_err = $array[3]
set peak_height = $array[4]
set peak_err = $array[5]
set fwhm_fit = $array[6]
set fwhm_err = $array[7]
set integral = $array[8]
set integral_err = $array[9]

# Report the fit to the user.
echo "        Centre Position: ${centre_fit} +- ${centre_err} ${sunits}"
echo "        Peak Height: ${peak_height} +- ${peak_err} ${unit}"
echo "        FWHM: ${fwhm_fit} +- ${fwhm_err} ${sunits}"
echo "        Line integral: ${integral} +- ${integral_err} ${unit}"

# Fit ok?
echo " "

if ( ${forcefit} == "FALSE" ) then
   echo -n "Fit ok? (yes/no): "
   set fitok = $<
   set fitok = `echo ${fitok} | awk '{print tolower($0)}'`
   echo " "
else
   set fitok = yes
endif

if ( ${fitok} == "no" || ${fitok} == "n" ) then
   rm -f ${fitfile} >& /dev/null
   if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
      set newfit = " "

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
   else
      set newfit = "-r"
   endif
   goto refit

else
   rm -f ${fitfile} >& /dev/null
endif

# Obtain the rest-frame co-ordinate.
# ==================================
set rest_known = "FALSE"
if ( ${gotrest} == "FALSE" ) then

# Get the rest-frame value.  There is at the time of writing no
# clean way to test whether or not this was successful.  So search the
# possible error message.
   set astat = `wcsattrib "ndf=${infile} mode=get name=RestFreq"`
   set found = `echo $astat | awk '{if (index( $0, "bad attribute")>0 ) print "Bad"}'`
   if ( $found == "Bad" ) then
      echo -n "Rest ${slabel} ($sunits): "
      set rest_coord = $<
      echo " "
   else

# Obtain the rest frequency in Ghz.  Note that the value is already
# stored in the WCS attributes.
      set rest_coord = `parget value wcsattrib`
      set slabel = "Frequency"
      set sunits = "GHz"
      set rest_known = "TRUE"
   endif

endif

echo "      Rest ${slabel}" ":"
echo "        ${slabel}" " : ${rest_coord} ${sunits}"
echo " "

# Loop through the entire datacube.
# =================================

# Create a couple of results files and a logfile.
touch ${mapfile}
touch ${varfile}

if ( ${gotlog} == "TRUE" ) then
   touch ${logfile}

# Write STL schema and column headings.
   source ${DATACUBE_DIR}/make_fitgauss_table.csh -l ${logfile} -f ${infile} -v ${unit} -c ${sunits}
endif

# Start at the origin.
set x = 0
@ x = $x + $lbnd[1]
set y = 0
@ y = $y + $lbnd[2]

set line = ""
set vars = ""

# Setup output filename.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
   echo " "
endif

date > ${tmpdir}/${user}/vmap_time.dat

# Fit the cube in a similar fashion.
echo "      Fitting:"
while( $y <= ${ubnd[2]} )
   while ( $x <= ${ubnd[1]} )

# Label for repeated fitting of the Gaussian.
approvefit:

# Extract the spectrum at the current spatial position.
      set specfile = "${tmpdir}/${user}/s${x}_${y}"
      ndfcopy "in=${infile}(${x},${y},${lbnd[3]}:${ubnd[3]}) out=${specfile} " \
              "trim trimwcs"

# Create a SpecFrame if one is not present, and make it the current
# frame.  At present the earlier test for a missing SpecFrame only
# occurs for a UK data-cube format with LAMBDA as the label, so the
# system is wavelength and units Angstrom.
      if ( $create_specframe == "TRUE" ) then
         wcsadd ndf=${specfile} frame=axis maptype=unit frmtype=spec \
                domain=SPECTRUM attrs="'System=wave,Unit=Angstrom'" >& /dev/null

# The spectrum has a SPECTRUM or DSBSPECTRUM domain WCS Frame, so select it for
# velocity calculations.
      else if ( $change_frame == "TRUE" ) then
         wcsframe "ndf=${specfile} frame=${new_wcsframe}" >& /dev/null
      endif

# Specify the units, in case these weren't known originally.
      axunits "ndf=${specfile} units=${sunits}" >& /dev/null

# Automatic mode
# --------------
      if ( ${approve} != "TRUE" ) then

# Fit the Gaussian to the spectrum.
         fitgauss \
            "in=${specfile} mask1=${low_mask} mask2=${upp_mask} "\
            "cont=${cont} peak=${peak} fwhm=${fwhm} reguess=no remask=no "\
            "ncomp=1 cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
            "centre=${position} logfil=${fitfile} device=! "\
            "dialog=f" >& /dev/null

# Approval mode
# -------------
      else

# Check to see if the NDF has an AXIS structure.  If one does not exist,
# create an array of axis centres, derived from the current WCS Frame,
# along the axis.
         if ( ${axis} == "FALSE" ) then
            setaxis "ndf=${specfile} dim=1 mode=wcs comp=Centre" >& /dev/null
         endif

# To compare like with like, ensure that plotting uses the AXIS frame.
#         wcsframe "ndf=${specfile} frame=axis"

# Fit the Gaussian to the spectrum.
         fitgauss \
            "in=${specfile} mask1=${low_mask} mask2=${upp_mask} "\
            "cont=${cont} peak=${peak} fwhm=${fwhm} reguess=no remask=no "\
            "ncomp=1 cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
            "centre=${position} logfil=${fitfile} device=${plotdev} "\
            "dialog=f" >& /dev/null

# Check to see whether or not fitting was successful when the -a option
# is selected.
         if ( ! -e $fitfile ) then
            echo "        No fit available"
            echo ""
            echo -n "Refit (yes) or quit (no)?: "
         else
            echo -n "Refit at ($x,$y)? (yes/no): "
         endif
         set refit = $<
         set refit = `echo ${refit} | awk '{print tolower($0)}'`
         echo " "

# Abort.
         if ( ${refit} == "no" || ${refit} == "n" || \
              ${refit} == "quit" || ${refit} == "q" ) then
            if ( ! -e $fitfile ) then
               rm -f ${fitfile} >& /dev/null
               goto cleanup
            endif

# Plot the ripped spectrum with zooming, and obtain new fit estimated
# parameters.
         else
            if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
               source ${DATACUBE_DIR}/getfitcon.csh -i ${specfile} -d ${plotdev} -z ${gotzoom}

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
            else
               source ${DATACUBE_DIR}/getfitcon.csh -i ${specfile} -d ${plotdev} -r -z ${gotzoom}
            endif
            goto approvefit
         endif
      endif

      if ( -e $fitfile ) then
         set results = `cat ${fitfile} | head -n 23 | tail -1`
         set array = \
           `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

# Store the results.
         set centre_fit = $array[2]
         set centre_err = $array[3]
         set peak_height = $array[4]
         set peak_err = $array[5]
         set fwhm_fit = $array[6]
         set fwhm_err = $array[7]
         set integral = $array[8]
         set integral_err = $array[9]

# Record the fit results.
         if ( ${gotlog} == "TRUE" ) then
            printf "%6i %6i %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g\n" \
                   ${x} ${y} ${centre_fit} ${centre_err} ${peak_height} ${peak_err} \
                   ${fwhm_fit} ${fwhm_err} ${integral} ${integral_err} >> ${logfile}
         endif

# Something has gone wrong.  Store a null value for this fit.
         set condition = `echo "if ($peak_height < 0) 1" | bc`
         if ( $condition == 1 ) then
            echo "        Spectrum at ($x,$y)"
            set line = "${line} -9999.99"
            if ( ${dovar} == "TRUE" ) then
               set vars = "${vars} -9999.99"
            endif

         else
            echo "        Spectrum at ($x,$y): $centre_fit +- $centre_err ${sunits}"

# Set the rest-frame value, if it's not already stored in the WCS
# attributes.  For this to work the current WCS Frame must be a
# SpecFrame.  At present there is no test.
            if ( ${rest_known} == "FALSE" ) then
               wcsattrib "ndf=${specfile} mode=set name=RestFreq "\
                         "newval='${rest_coord} ${sunits}'" >& /dev/null
            endif

# Reset the System of the current WCS Frame to velocities in the desired
# system and units.
            wcsattrib "ndf=${specfile} mode=set name=System newval=${velsys}"
            wcsattrib "ndf=${specfile} mode=set name=Unit newval=${vunits}"

# Convert the line-centre position to optical velocity.
            wcstran "ndf=${specfile} posin=${centre_fit} framein=AXIS " \
                    "frameout=${new_wcsframe}" >& /dev/null
            set velocity = `parget posout wcstran`

            if ( ${dovar} == "TRUE" ) then
               echo -n "                           $velocity"
            else
               echo "                           $velocity $vunits"
            endif

            set line = "${line} ${velocity}"

# Calculate the error.
            if ( ${dovar} == "TRUE" ) then
               if ( ${centre_err} == "nan" || ${centre_err} == "INF" ) then

# Set variance to the null value.
                  echo " ${vunits}"
                  set vars = "${vars} -9999.99"

# Convert the upper error bound position to optical velocity.
               else
                  set upp_err = \
                    `calc exp="'${centre_fit} + ${centre_err}'" prec=_double`
                  wcstran "ndf=${specfile} posin=${upp_err} framein=AXIS " \
                          "frameout=${new_wcsframe}" >& /dev/null
                  set upp_vel = `parget posout wcstran`


                  set low_err = \
                    `calc exp="'${centre_fit} - ${centre_err}'" prec=_double`
                  wcstran "ndf=${specfile} posin=${low_err} framein=AXIS " \
                          "frameout=${new_wcsframe}" >& /dev/null
                  set low_vel = `parget posout wcstran`

                  set vel_err = \
                    `calc exp="'(${upp_vel}-${low_vel})/2.0E+00'" prec=_double`

                  if ( `echo "if ( $vel_err < 0.0 ) 1" | bc` ) then
                     set vel_err = `calc exp="'-($vel_err)'" prec=_double`
                  endif
                  set fvel_err = `printf "%-.5G" ${vel_err}`

                  echo " +- ${fvel_err} ${vunits}"

#  Add the velocity variance to the list of variances.
                  set vel_var = `calc "${vel_err}*${vel_err}"`
                  set vars = "${vars} ${vel_var}"
               endif
            endif
         endif

# No fit file.  Set dummy values.
      else
         echo "        Spectrum at ($x,$y)"
         set line = "${line} -9999.99"
         if ( ${dovar} == "TRUE" ) then
            set vars = "${vars} -9999.99"
         endif
      endif

# Remove temporary files for the current pixel.
      rm -f ${fitfile} >& /dev/null
      rm -f ${specfile}.sdf >& /dev/null

# Move to the next pixel.
      @ x = ${x} + 1
   end

# Store the results in a text file.
   echo "${line}" >> ${mapfile}
   set line = ""
   if ( ${dovar} == "TRUE" ) then
      echo "${vars}" >> ${varfile}
      set vars = ""
   endif

# Move to the next row.
   set x = 0
   @ x = $x + $lbnd[1]
   @ y = ${y} + 1
end

date >> ${tmpdir}/${user}/vmap_time.dat

# Convert the text file of the map to a two-dimensional NDF.
# ==========================================================

echo " "
echo "      Output NDF:"
echo "        Converting: Creating NDF from data."

ascii2ndf "in=${mapfile} out=${outfile}_tmp shape=[${dims[1]},${dims[2]}] "\
          "maxlen=2048 type='_real'" >& /dev/null

# Set the null values to the bad value (VAL__BADR).
setmagic "in=${outfile}_tmp out=${outfile} repval=-9999.99" >& /dev/null
if ( -e ${outfile}.sdf ) then
   rm -f "${outfile}_tmp.sdf" >& /dev/null
else
   echo "WARNING: Setting bad-pixel values failed."
   mv -f ${outfile}_tmp.sdf ${outfile}.sdf
endif

# Set the NDF origin.
echo "        Origin: Attaching origin (${lbnd[1]},${lbnd[2]})."
setorigin "ndf=${colfile} origin=[${lbnd[1]},${lbnd[2]}]" >& /dev/null
setorigin "ndf=${outfile} origin=[${lbnd[1]},${lbnd[2]}]" >& /dev/null

# Attach the VARIANCE array.  Note that ASCII2NDF does not allow
# COMP=Error so work in variances rather than error.
if ( ${dovar} == "TRUE" ) then
   echo "        Converting: Attaching VARIANCE array."
   ascii2ndf in=${varfile} comp="Variance" out=${outfile} \
             shape="[${dims[1]},${dims[2]}]" \
             maxlen=2048 type='_real'

# Replace its null values with the bad value.
   mv -f ${outfile}.sdf ${outfile}_tmp.sdf
   setmagic in=${outfile}_tmp out=${outfile} \
            comp="Variance" repval=-9999.99 >& /dev/null

   if ( -e ${outfile}.sdf ) then
      rm -f "${outfile}_tmp.sdf" >& /dev/null
   else
      echo "WARNING: Setting bad-pixel variance values failed."
      mv -f ${outfile}_tmp.sdf ${outfile}.sdf
   endif
endif

# Use the white-light image to clone the axis AXIS and WCS co-ordinates.
# The WCS information will be copied incorrectly if the AXIS structure does
# not exist before the WCS component is cloned.  If one an AXIS structure
# does not exist, create an array of axis centres, derived from the current
# WCS Frame, along each axis.
if ( ${axis} == "FALSE" ) then
   echo "        Axes: Creating AXIS centres."
   setaxis "ndf=${colfile} dim=1 mode=wcs comp=Centre" >& /dev/null
   setaxis "ndf=${colfile} dim=2 mode=wcs comp=Centre" >& /dev/null
endif

echo "        Axes: Attaching AXIS structures."
setaxis "ndf=${outfile} like=${colfile}" >& /dev/null

echo "        WCS: Attaching WCS information."
wcscopy "ndf=${outfile} like=${colfile}" >& /dev/null

echo "        Title: Setting title."
settitle "ndf=${outfile} title='Velocity Map'"
echo " "


# Plot the output velocity map.
# =============================

# Determine the gap size for the axis label.  In order to get it to
# the right of the plot---placing left shrinks the key---we need to
# know the approximate width of the values.  Here since we know valid
# values are from minus a few hundreds up to c, we can get an
# approximate length using the extreme values and allowing for a sign.
   stats ndf=${outfile}  >& /dev/null
   set minvel = `parget minimum stats`
   set maxvel = `parget maximum stats`
   set minc = `calc exp="'nint(log10(abs(${minvel}))+0.5)+max(0,-1*sign(1,${minvel}))'"`
   set maxc = `calc exp="'nint(log10(abs(${maxvel}))+0.5)+max(0,-1*sign(1,${maxvel}))'"`
   set gap = `calc exp="'-2.29-max(${minc},${maxc})*0.14'"`

# Check to see if we need to plot the output velocity map.
if ( ${plotspec} == "TRUE" ) then
   lutspec device=${plotdev}
   echo "      Plotting:"
   echo "        Display: Velocity map using percentile scaling."
   display "${outfile} device=${plotdev} mode=per percentiles=[2,98]"\
           "axes=yes margin=! key keypos=0.12 " \
            keystyle="'TextLab(1)=1,TextLabGap(1)=${gap},Label(1)=Velocity in ${vunits}'" >& /dev/null

   if ( $drawcontours == "TRUE" ) then
      echo "        Contour: White-light image with equally spaced contours."
      contour "ndf=${colfile} device=${plotdev} clear=no mode=equa" \
              "keypos=[0.025,1] keystyle='Digits(2)=4,Size=1.2' "\
              "axes=no ncont=${numcont} pens='colour=${ci}' margin=!" >& /dev/null
   endif
   echo " "
endif

# Loop for manual fitting.
# ========================

set loop_var = 1
if ( ${forcefit} == "FALSE" ) then
   if ( ${plotspec} == "FALSE" ) then
      lutspec device=${plotdev}
      echo "      Plotting:"
      echo "        Display: Velocity map using percentile scaling."
      display "${outfile} device=${plotdev} mode=per percentiles=[2,98]"\
              "axes=yes margin=! key keypos=0.12 " \
              keystyle="'TextLab(1)=1,TextLabGap(1)=${gap},Label(1)=Velocity in ${vunits}'" >& /dev/null

      if ( $drawcontours == "TRUE" ) then
         echo "        Contour: White-light image with equally spaced contours."
         contour "ndf=${colfile} device=${plotdev} clear=no mode=equa"\
                 "keypos=[0.025,1] keystyle='Digits(2)=4,Size=1.2' "\
                 "axes=no ncont=${numcont} pens='colour=${ci}' margin=!" >& /dev/null
      endif
   endif

   while ( ${loop_var} == 1 )
      echo " "
      echo -n "Refit a point (yes/no): "
      set refit = $<
      set refit = `echo ${refit} | awk '{print tolower($0)}'`
      echo " "

      if ( ${refit} == "yes" || ${refit} == "y" ) then

# Copy the current output file to a _tmp file.
         mv -f ${outfile}.sdf ${outfile}_tmp.sdf

# Grab the X-Y position.
         echo " "
         echo "  Left click on pixel to be extracted."
         source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Extract the spectrum.
# =====================
         echo " "
         echo "      Extracting:"
         echo "        (X,Y) pixel: ${xgrid},${ygrid}"

# Extract the spectrum from the cube.
         ndfcopy in="${infile}(${xgrid},${ygrid},${lbnd[3]}:${ubnd[3]})" \
                 out=${ripfile} trim trimwcs

# Check to see if the NDF has an AXIS component.  If one does not exist,
# create an array of axis centres, derived from the current WCS Frame.
         set axis = `parget axis ndftrace`

         if ( ${axis} == "FALSE" ) then
            setaxis "ndf=${ripfile} dim=1 mode=wcs comp=Centre" >& /dev/null
            echo "        Axes: Adding AXIS centres."
         endif

# Check to see if the NDF has VARIANCE.
         if ( ${variance} == "FALSE" ) then
            echo "        Variances: present."
         else
            echo "        Variances: absent."
         endif

# To compare like with like ensure, that plotting uses the AXIS frame,
# but first record the index to the current Frame.
         ndftrace "ndf=${ripfile}" >& /dev/null
         set inframe = `parget current ndftrace`

# Re-plot.
# ========

# Indicate that we do not wish to merely replot, but also possibly zoom.
         set newfit = " "

# Label for repeated fitting of the Gaussian.
manual_refit:

# Now reset the the WCS inside the refitting loop, so that the
# redisplay of the spectrum uses the axis co-ordinates, not the
# original Frame.
         wcsframe "ndf=${ripfile} frame=axis"

# Specify the units, in case these weren't know originally.
         axunits "ndf=${ripfile} units=${sunits}" >& /dev/null

# Plot the ripped spectrum with or without zooming, and obtain the fit
# estimated parameters.
         source ${DATACUBE_DIR}/getfitcon.csh -i ${ripfile} -d ${plotdev} ${newfit} -z ${gotzoom}

# Fit the line.
# =============

# Reset the WCS Frame to its original value, now we have finished
# plotting and fitting.
         wcsframe "ndf=${ripfile} frame=${inframe}"

# Create a SpecFrame if one is not present, and make it the current
# frame.  At present the earlier test for a missing SpecFrame only
# occurs for a UK data-cube format with LAMBDA as the label, so the
# system is wavelength and units Angstrom.
         if ( $create_specframe == "TRUE" ) then
            wcsadd ndf=${ripfile} frame=axis maptype=unit frmtype=spec \
                  domain=SPECTRUM attrs="'System=wave,Unit=Angstrom'" >& /dev/null

# The spectrum has a SPECTRUM or DSBSPECTRUM domain WCS Frame, so select it for
# velocity calculations.
         else if ( $change_frame == "TRUE" ) then
            wcsframe "ndf=${ripfile} frame=${new_wcsframe}" >& /dev/null
         endif

         echo "      Fitting:"

         fitgauss \
           "in=${ripfile} mask1=${low_mask} mask2=${upp_mask} cont=${cont} "\
           "peak=${peak} fwhm=${fwhm} reguess=no remask=no ncomp=1 "\
           "cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
           "centre=${position} logfil=${fitfile} device=${plotdev}"\
           "dialog=f" >& /dev/null

# Check to see whether or not fitting was successful.
         if ( ! -e $fitfile ) then
            echo "        No fit available"
            echo ""
            echo -n "Refit (yes) or abort (no): "
            set refit = $<
            set refit = `echo ${refit} | awk '{print tolower($0)}'`
            echo " "

            if ( ${refit} == "no" || ${refit} == "n" || \
                 ${refit} == "quit" || ${refit} == "q" ) then
               rm -f ${fitfile} >& /dev/null
               goto cleanup

# Indicate that we do not wish to merely replot, but also possibly zoom.
            else
               if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
                  set newfit = " "

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
               else
                  set newfit = "-r"
               endif
               goto manual_refit
            endif
         endif

# Get the fit from the temporary file.
         set results = `cat ${fitfile} | head -n 23 | tail -1`
         set array = \
           `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

         set centre_fit = $array[2]
         set centre_err = $array[3]
         set peak_height = $array[4]
         set peak_err = $array[5]
         set fwhm_fit = $array[6]
         set fwhm_err = $array[7]
         set integral = $array[8]
         set integral_err = $array[9]

# Show the user the fit.
         echo "        Centre Position: ${centre_fit} +- ${centre_err} ${sunits}"
         echo "        Peak Height: ${peak_height} +- ${peak_err} ${unit}"
         echo "        FWHM: ${fwhm_fit} +- ${fwhm_err} ${sunits}"
         echo "        Line integral: ${integral} +- ${integral_err} ${unit}"

# Fit ok?
         echo " "

         if ( ${forcefit} == "FALSE" ) then
            echo -n "Fit ok? (yes/no/quit): "
            set fitok = $<
            set fitok = `echo ${fitok} | awk '{print tolower($0)}'`
            echo " "
         else
            set fitok = yes
         endif

         if ( ${fitok} == "no" || ${fitok} == "n" ) then
            rm -f ${fitfile} >& /dev/null
            if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
               set newfit = " "
            else

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
               set newfit = "-r"
            endif
            goto manual_refit

         else if ( ${fitok} == "quit" || ${fitok} == "q" ) then
            rm -f ${fitfile} >& /dev/null
            goto dropout

         else
            rm -f ${fitfile} >& /dev/null
         endif

# Record the fit results.  Note that the previous fit for this spatial
# pixel is still present in the log.
         if ( ${gotlog} == "TRUE" ) then
            printf "%6i %6i %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g\n" \
                   ${xgrid} ${ygrid} ${centre_fit} ${centre_err} ${peak_height} ${peak_err} \
                   ${fwhm_fit} ${fwhm_err} ${integral} ${integral_err} >> ${logfile}
         endif

# Calculate the velocity.
# =======================

# Set the rest-frame value, if it's not already stored in the WCS
# attributes.  For this to work the current WCS Frame must be a
# SpecFrame.  At present there is no test.
         if ( ${rest_known} == "FALSE" ) then
            wcsattrib "ndf=${ripfile} mode=set name=RestFreq "\
                      "newval='${rest_coord} ${sunits}'" >& /dev/null
         endif

# Reset the System of the current WCS Frame to velocities in the desired
# system and units.
         wcsattrib "ndf=${ripfile} mode=set name=System newval=${velsys}"
         wcsattrib "ndf=${ripfile} mode=set name=Unit newval=${vunits}"

# Convert the line-centre position to optical velocity.
         wcstran "ndf=${ripfile} posin=${centre_fit} framein=AXIS " \
                 "frameout=${new_wcsframe}" >& /dev/null
         set velocity = `parget posout wcstran`

         if ( ${dovar} == "TRUE" ) then
            echo "        (X,Y) Pixel: ${xgrid}:${ygrid}"
            echo -n "        Line Velocity: $velocity"
         else
            echo "        (X,Y) Pixel: ${xgrid},${ygrid}"
            echo "        Line Velocity: $velocity ${vunits}"
         endif
         echo " "

# Change the pixel value.
         set pixel = "${xgrid}:${xgrid},${ygrid}:${ygrid}"
         chpix in=${outfile}_tmp out=${outfile} comp="Data"\
               newval=${velocity} section=\'${pixel}\'

         if ( -e ${outfile}.sdf ) then
            rm -f ${outfile}_tmp.sdf >& /dev/null
         else
            echo "WARNING: Inserting new pixel value failed"
            mv  -f ${outfile}_tmp.sdf ${outfile}.sdf
         endif

# Calculate the error.
         if ( ${dovar} == "TRUE" ) then
            if ( ${centre_err} == "nan" || ${centre_err} == "INF" ) then

# Set the variance to the null value.
               echo " ${vunits}"
               set vel_var = -9999.99
            else

# Derive value's error bars.
               set upp_err = \
                 `calc exp="'${centre_fit} + ${centre_err}'" prec=_double`
               wcstran "ndf=${ripfile} posin=${upp_err} framein=AXIS" \
                       "frameout=${new_wcsframe}" >& /dev/null
               set upp_vel = `parget posout wcstran`

               set low_err = \
                 `calc exp="'${centre_fit} - ${centre_err}'" prec=_double`
               wcstran "ndf=${ripfile} posin=${low_err} framein=AXIS " \
                       "frameout==${new_wcsframe}" >& /dev/null
               set low_vel = `parget posout wcstran`

               set vel_err = \
                 `calc exp="'(${upp_vel}-${low_vel})/2.0E+00'" prec=_double`

               if ( `echo "if ( $vel_err < 0.0 ) 1" | bc` ) then
                  set vel_err = `calc exp="'-($vel_err)'" prec=_double`
               endif

               set fvel_err = `printf "%-.5G" ${vel_err}`
               echo " +- ${fvel_err} ${vunits}"

               set vel_var = `calc "${vel_err}*${vel_err}"`
            endif

# Move the output file to a temporary place holder.
            mv -f ${outfile}.sdf ${outfile}_tmp.sdf

# Change the pixel variance.
            set pixel = "${xgrid}:${xgrid},${ygrid}:${ygrid}"
            chpix in=${outfile}_tmp out=${outfile} comp="Variance" \
                  newval=${vel_var} section=\'${pixel}\'

            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf >& /dev/null
            else
               echo "WARNING: Inserting new variance value failed."
               mv -f ${outfile}_tmp.sdf ${outfile}.sdf
            endif

# Set the null values to the bad value (VAL__BADR).
            mv -f ${outfile}.sdf ${outfile}_tmp.sdf >& /dev/null
            setmagic in=${outfile}_tmp out=${outfile} \
                     comp="Variance" repval=-9999.99 >& /dev/null
            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf >& /dev/null
            else
               echo "WARNING: Setting bad-pixel values failed."
               mv -f ${outfile}_tmp.sdf ${outfile}
            endif
         endif

# Plot the new velocity map.
# ==========================
         lutspec device=${plotdev}
         echo " "
         echo "      Plotting:"
         echo "        Display: Velocity map using percentile scaling."
         display "${outfile} device=${plotdev} mode=per percentiles=[2,98]"\
                 "axes=yes margin=! key keypos=0.12 " \
                  keystyle="'TextLab(1)=1,TextLabGap(1)=${gap},Label(1)=Velocity in ${vunits}'" >& /dev/null
         if ( $drawcontours == "TRUE" ) then
            echo "        Contour: White-light image with equally spaced contours."
            contour "ndf=${colfile} device=${plotdev} clear=no mode=equa"\
                    "keypos=[0.025,1] keystyle='Digits(2)=4,Size=1.2' "\
                   "axes=no ncont=${numcont} pens='colour=${ci}' margin=!" >& /dev/null
         endif
         echo " "

# Clean up temporary velmap files, salvage ${outfile}_tmp in the case
# where there is no existing $outfile (i.e. CHPIX has not run).
         if ( -e ${outfile}_tmp.sdf ) then
            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf >& /dev/null
            else
               mv -f ${outfile}_tmp.sdf ${outfile}.sdf
            endif
         endif

# End of manual-refitting loop.
      else

# Drop out of while loop.
         set loop_var = 0
      endif
   end
endif

# Dropout point for an aborted fitting, try and salvage already existing
# velocity maps that may be lying around instead of throwing them away.
dropout:
if ( -e ${outfile}_tmp.sdf ) then
   if ( -e ${outfile}.sdf ) then
      rm -f ${outfile}_tmp.sdf
   else
      mv -f ${outfile}_tmp.sdf ${outfile}.sdf
   endif
endif

# Clean up
# ========
cleanup:

rm -f ${colfile}.sdf >& /dev/null
rm -f ${ripfile}.sdf >& /dev/null
rm -f ${fitfile} >& /dev/null
rm -f ${mapfile} >& /dev/null
rm -f ${varfile} >& /dev/null
rm ${tmpdir}/${user}/vmap_time.dat >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null
