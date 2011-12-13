#!/bin/csh
#+
#  Name:
#     peakmap.csh
#
#  Purpose:
#     Builds a map of emission-line strength from a spectral-cube NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     peakmap [-a] [-c number] [-ci index] [-f] [-i filename] [-l logfile]
#             [-o filename] [-p] [-v] [-z/+z]
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
#     NDF image of the strength of the line.  These will use the same initial
#     parameters as the reference spectrum, unless option -a is selected.
#     You may view this image drawn with a key (option -d), and overlay a
#     contour plot (with a key) of the white-light image (option -c).
#
#     If you do not force the fit to be considered "good" by using the -f
#     command-line option, the script will offer the opportunity to manually
#     refit the spectral feature for individual pixels, such as those that
#     were unsuccessfully fitted by the automatic procedure.  In this case,
#     the line-strength map will be plotted and replotted after the new fit,
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
#       the line.  This is a dangerous option; if the fit is poor, or
#       unobtainable the script may terminate abruptly if it is forced to
#       accept the fit.  This will additionally suppress manual re-fitting
#       of bad pixels at the end of the run of the script.  [FALSE]
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
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
#       The filename for the output NDF of the line-strength map.
#     -p
#       The script will plot the final image map to the current display
#       as well as saving it to an NDF file.  It will additionally overplot
#       the white-light image as a contour map for comparison.  [FALSE]
#     -v
#       The script will generate a variance array from the line fits and
#       attach it to the peak-intensity-map NDF. [FALSE]
#     -z
#       The script will automatically prompt to select a region to zoom
#       before prompting for the region of interest.  [TRUE]
#     +z
#       The script will not prompt for a zoom before requesting the region
#       of interest.  [FALSE]
#
#  Notes:
#     -  The line-strength image map display scales between the 15 and 98
#     percentiles.  The map uses a false-colour lookup table.
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
#     NDF image of the velocity of the line.  You may view this image
#     drawn with a key (option -d), and overlay a contour plot of the
#     white-light image (option -c).
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
#     31-DEC-2000 (AALLAN):
#       Allowed single-character responses to yes/no prompts.
#     03-JAN-2001 (AALLAN)
#       Added manual refitting of data points.
#     08-JAN-2001 (AALLAN):
#       Variance calculation made more robust.
#       Fixed bug in magic-value propogation.
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
#     2005 October 11 (MJC):
#       Replaced unused -r with -c option, and all prompts pertaining
#       to obtaining a rest value.  Described the option to refit spectral
#       line for individual spatial pixel.  Fixed bugs converting the cursor
#       position into negative pixel indices.  Ensured that the plots of
#       spectra use the axis co-ordinate system.  Called KAPPA:CALC for
#       floating-point arithmetic using appriopriate precision, and ensure a
#       digit before a decimal point, unlike bc.  Plot spectra in histogram
#       mode for clarity.  Added defaults to parameter descriptions.
#       Appended data unit to reported peak values, or replace spurious fixed
#       "ms^-1".  Inserted blank lines to structure the commentary better.
#       Correct some output and comments: e.g. "spectra" singular to
#       "spectrum".
#     2005 November 3 (MJC):
#       Add options waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script that
#       also checks for a degenerate third axis.
#     2006 March 16 (MJC):
#       Use any supplied spectral-axis section when ripping each spectrum for
#       line fitting.  Reset KAPPA:DISPLAY parameters.
#     2007 May 5 (MJC):
#       Obtain and use spectral units.  Call the new getfitcon script to
#       determine the fit's initial-guess parameters.  Added -c option.
#     2007 May 8 (MJC):
#       Added -l option.
#     2008 June 19 (MJC):
#       Intepret non-positive number of contours to mean no contour
#       overlay required.  Added -ci option.  Provide case insensitive
#       prompting.  Remove overloading of the fitgood variable.  Fixed bug
#       that stored the peak-height error as the variance in the output NDF.
#       Remove duplicate token.  Added Licence.
#     2008 June 25 (MJC):
#       Added STL schema for log of fit parameters.
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
rm -f ${tmpdir}/${user}/pmap* >& /dev/null
rm -f ${tmpdir}/${user}/?_?.sdf >& /dev/null

# Do the variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set fitfile = "${tmpdir}/${user}/pmap_fitgauss.tmp"
set colfile = "${tmpdir}/${user}/pmap_col"
set ripfile = "${tmpdir}/${user}/pmap_rip"
set mapfile = "${tmpdir}/${user}/pmap_map.dat"
set varfile = "${tmpdir}/${user}/pmap_var.dat"

# Set options access flags.
set approve = "FALSE"
set dovar = "FALSE"
set drawcontours = "TRUE"
set forcefit = "FALSE"
set gotlog = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
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
   case -o:    # output peak map?
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot peak map?
      set plotspec = "TRUE"
      set plotdev = "${plotdev}"
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
source ${DATACUBE_DIR}/checkndf.csh -s peakmap
if ( $status == 1 ) exit

# Find out the cube bounds and units.
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`
set unit = `parget units ndftrace`

# Get the spectral units.
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

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

date > ${tmpdir}/${user}/pmap_time.dat

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
         wcsframe "ndf=${specfile} frame=axis"

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
            echo "        Spectrum at ($x,$y): $peak_height +- $peak_err ${sunits}"

            set line = "${line} ${peak_height}"
            if ( ${dovar} == "TRUE" ) then
               if ( ${centre_err} == "nan" || ${centre_err} == "INF" ) then

# Set variance to the null value.
                  set vars = "${vars} -9999.99"

# Convert error in the peak to the variance.
               else
                  set peak_var = `calc "${peak_err}*${peak_err}"`
                  set vars = "${vars} ${peak_var}"
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

date >> ${tmpdir}/${user}/pmap_time.dat

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
settitle "ndf=${outfile} title='Line-strength Map'"
echo " "

# Plot the output line-strength map.
# ==================================

# Check to see if we need to plot the output line-strength map.
if ( ${plotspec} == "TRUE" ) then

# Change to the coloured lookup table.
   lutcol device=${plotdev}
   echo "      Plotting:"
   echo "        Display: Line-strength map using percentile scaling."
   display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
           "axes=yes margin=!" reset >& /dev/null

   if ( $drawcontours == "TRUE" ) then
      echo "        Contour: White-light image with equally spaced contours."
      contour "ndf=${colfile} device=${plotdev} clear=no mode=equa "\
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
      lutcol device=${plotdev}
      echo "      Plotting:"
      echo "        Display: Line-strength map using percentile scaling."
      display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
              "axes=yes margin=!" reset >& /dev/null

      if ( $drawcontours == "TRUE" ) then
         echo "        Contour: White-light image with equally spaced contours."
         contour "ndf=${colfile} device=${plotdev} clear=no mode=equa "\
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

# To compare like with like ensure, that plotting uses the AXIS frame.
         wcsframe "ndf=${ripfile} frame=axis"

# Check to see if the NDF has VARIANCE.
         if ( ${variance} == "FALSE" ) then
            echo "        Variances: present."
         else
            echo "        Variances: absent."
         endif

# Re-plot.
# ========

# Indicate that we do not wish to merely replot, but also possibly zoom.
         set newfit = " "

# Label for repeated fitting of the Gaussian.
manual_refit:

# Plot the ripped spectrum with or without zooming, and obtain the fit
# estimated parameters.
         source ${DATACUBE_DIR}/getfitcon.csh -i ${ripfile} -d ${plotdev} ${newfit} -z ${gotzoom}

# Fit the line.
# =============
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

# Tell getfitcon to ignore zooming, and that we just want to obtain
# a new set of initial estimates of the Gaussian.
            else
               set newfit = "-r"
            endif
            goto manual_refit

         else if ( ${fitok} == "quit" || ${fitok} == "q" ) then
            rm -f ${fitfile} >& /dev/null
            goto dropout

         else
            rm -f ${fitfile} >& /dev/null
         endif

# Report the peak_height and peak_err values.
         if ( ${dovar} == "TRUE" ) then
            echo "        (X,Y) Pixel: ${xgrid}:${ygrid}"
            echo -n "        Peak Height: $peak_height "
         else
            echo "        (X,Y) Pixel: ${xgrid},${ygrid}"
            echo "        Peak Height: $peak_height ${unit}"
         endif
         echo " "

# Record the fit results.  Note that the previous fit for this spatial
# pixel is still present in the log.
         if ( ${gotlog} == "TRUE" ) then
            printf "%6i %6i %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g\n" \
                   ${xgrid} ${ygrid} ${centre_fit} ${centre_err} ${peak_height} ${peak_err} \
                   ${fwhm_fit} ${fwhm_err} ${integral} ${integral_err} >> ${logfile}
         endif

# However, we change the pixel value in the peakmap.
         set pixel = "${xgrid}:${xgrid},${ygrid}:${ygrid}"
         chpix in=${outfile}_tmp out=${outfile} comp="Data"\
               newval=${peak_height} section=\'${pixel}\'

         if ( -e ${outfile}.sdf ) then
            rm -f ${outfile}_tmp.sdf >& /dev/null
         else
            echo "WARNING: Inserting new pixel value failed"
            mv  -f ${outfile}_tmp.sdf ${outfile}.sdf
         endif

# Calculate the error.
         if ( ${dovar} == "TRUE" ) then
            echo " +- ${peak_err} ${unit}"

            if ( ${peak_err} == "nan" || ${peak_err} == "INF" ) then

# Set the variance to the null value.
               echo " ${unit}"
               set peak_err = "-9999.99"

            else
               set peak_var = `calc "${peak_err}*${peak_err}"`

            endif

# Move the output file to a temporary place holder.
            mv -f ${outfile}.sdf ${outfile}_tmp.sdf

# Change the pixel variance.
            set pixel = "${xgrid}:${xgrid},${ygrid}:${ygrid}"
            chpix in=${outfile}_tmp out=${outfile} comp="Variance" \
                  newval=${peak_var} section=\'${pixel}\'

            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf >& /dev/null
            else
               echo "WARNING: Inserting new variance value failed."
               mv -f ${outfile}_tmp.sdf ${outfile}.sdf
            endif

# Replace the null values with the bad value (VAL__BADR).
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

# Plot the new peak map.
# ======================
         lutcol device=${plotdev}
         echo " "
         echo "      Plotting:"
         echo "        Display: Line strength map using percentile scaling."
         display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
                 "axes=yes margin=!" reset >& /dev/null
         if ( $drawcontours == "TRUE" ) then
            echo "        Contour: White-light image with equally spaced contours."
            contour "ndf=${colfile} device=${plotdev} clear=no mode=equa"\
                    "keypos=[0.025,1] keystyle='Digits(2)=4,Size=1.2' "\
                   "axes=no ncont=${numcont} pens='colour=${ci}' margin=!" >& /dev/null
         endif
         echo " "

# Clean up temporary peakmap files, salvage ${outfile}_tmp in the case
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
# line-strength maps that may be lying around instead of throwing them away.
dropout:
if ( -e ${outfile}_tmp.sdf ) then
   if ( -e ${outfile}.sdf ) then
      rm -f ${outfile}_tmp.sdf >& /dev/null
   else
      mv -f ${outfile}_tmp.sdf ${outfile}.sdf
   endif
endif

# Clean up.
# ========
cleanup:

rm -f ${colfile}.sdf >& /dev/null
rm -f ${ripfile}.sdf >& /dev/null
rm -f ${fitfile} >& /dev/null
rm -f ${mapfile} >& /dev/null
rm -f ${varfile} >& /dev/null
rm -f ${tmpdir}/${user}/pmap_time.dat >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null
