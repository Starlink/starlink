#!/bin/csh
#+
#  Name:
#     peakmap.csh
#
#  Purpose:
#     Builds a map of emission-line strength from a three-dimensional IFU NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     peakmap [-i filename] [-o filename] [-c number] [-f] [-p] [-v] [-z/+z]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA,
#     and FIGARO packages.  It reads a three-dimensional IFU NDF as input
#     and presents the user with a white-light image of the cube.  You can
#     then select and X-Y position using the cursor.  The script will extract
#     and display this spectrum.  You will then be prompted to specify
#     various fitting parameters, e.g. peak position, using the cursor.  The
#     script will then attempt to fit the emission line.  The fit will be
#     displayed and you are consulted regarding the goodness of fit.  If you
#     consider the fit to be good enough, the script will attempt to perform
#     similar fits to all spectra within the cube, building a two-dimensional
#     NDF image of the strength of the line.
#
#     If you do not force the fit to be considered "good" by using the -f
#     command-line option, the script will offer the opportunity to manually
#     refit the spectral feature for individual pixels, such as those that
#     were unsuccessfully fitted by the automatic procedure.
#
#  Parameters:
#     -c number
#       Number of contours in the white-light image.  [15]
#     -f
#       Force the script to accept the first attempt to fit a Gaussian to
#       the line.  This is a dangerous option; if the fit is poor, or
#       unobtainable the script may terminate abruptly if it is forced to
#       accept the fit. [FALSE]
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF, by default the script will prompt for the
#       input file.
#     -o filename
#       The filename for the output NDF of the line-strength map.
#     -p
#       The script will plot the final image map to the current display 
#       as well as saving it to an NDF file.  Additionally, it will overplot
#       the white-light image as a contour map for comparison.  [FALSE]
#     -v
#       The script will generate a VARIANCE array from the line fits and
#       attach it to the velocity-map NDF. [FALSE]
#     -z 
#       The script will automatically prompt the user to select a region to
#       zoom before prompting for the region of interest.  [TRUE]
#     +z 
#       The program will not prompt for a zoom before requesting the region
#       of interest.  [FALSE]

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
#     31-DEC-2000 (AALLAN):
#       Allowed 1 character responses to yes/no prompts
#     03-JAN-2001 (AALLAN)
#       Added manual refitting of data points
#     08-JAN-2001 (AALLAN):
#       Variance calculation made more robust
#       Fixed bug in magic-value propogation.
#       Major bug in manual refitting routine fixed
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September  1 (MJC):
#       Replaced COPYAXIS with KAPPA:SETAXIS.
#     2005 September  2 (MJC):
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
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils

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

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/pmap_cursor.tmp"
set fitfile = "${tmpdir}/${user}/pmap_fitgauss.tmp"
set colfile = "${tmpdir}/${user}/pmap_col"
set ripfile = "${tmpdir}/${user}/pmap_rip"
set mapfile = "${tmpdir}/${user}/pmap_map.dat"
set varfile = "${tmpdir}/${user}/pmap_var.dat"

touch ${curfile}

set plotspec = "FALSE"
set plotdev = "xwin"
set fitgood = "yes"
set dovar = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotzoom = "ASK"
set forcefit = "FALSE"

# The SPECDRE extension is used to store the Gaussian fit.
set component = 1

# Set the number of contours used to display the white-light image.
set numcont = 15

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -c:    # Number of contours
      shift args
      set numcont = $args[1]
      if ( $numcont < 1 || $numcont > 100 ) then
         set numcont = 15
      endif
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
   case -o:    # output peak map?
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectra?
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
   endsw  
end

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain details of the input cube.
# =================================

# Get the input filename.
if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
   set infile = ${infile:r}
endif

echo " "

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# Check that it exists.
if ( ! -e ${infile}.sdf ) then
   echo "PEAKMAP_ERR: ${infile}.sdf does not exist."
   rm -f ${curfile} >& /dev/null
   exit  
endif

# Find out the cube dimensions.
ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`
set unit = `parget units ndftrace`

if ( $ndim != 3 ) then
   echo "PEAKMAP_ERR: ${infile}.sdf is not a datacube."
   rm -f ${curfile} >& /dev/null
   exit  
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

# Report the statistics.
echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"
echo " "

# Check to see if the NDF has VARIANCE.
set variance = `parget variance ndftrace`

# Show the white-light image.
# ===========================

# Collapse the white-light image.
echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile} axis=3" >& /dev/null 

# Display the collapsed image.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Grab the X-Y position.
echo " "
echo "  Left click on pixel to be extracted"
   
cursor showpixel=true style="Colour(marker)=2" plot=mark \
       maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# Wait for CURSOR output then get X-Y co-ordinates from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# We don't clean up the collapsed white-light image here as normal because
# it will be used later to clone both the AXIS and WCS co-ordinates for the
# new velocity map.

# Grab the spatial position.
set pos=`parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.
set xpix = `echo $pos[1] | awk '{split($0,a,"."); print int(a[1])}'`
set ypix = `echo $pos[2] | awk '{split($0,a,"."); print int(a[1])}'`

# Clean up the cursor temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}

# Extract the spectrum.
# =====================
echo " "
echo "      Extracting:"
echo "        (X,Y) pixel: ${xpix},${ypix}"

# Extract the spectrum from the cube.
ndfcopy "in=${infile}($xpix,$ypix,) out=${ripfile} trim=true trimwcs=true"

# Check to see if the NDF has an AXIS structure.  If one does not exist,
# create an array of axis centres, derived from the current WCS Frame,
# along the axis.
set axis = `parget axis ndftrace`
if ( ${axis} == "FALSE" ) then
   setaxis "ndf=${ripfile} dim=1 mode=wcs comp=Centre" >& /dev/null
   echo "        Axes: Adding AXIS centres."
endif

# To compare like with like ensure, that plotting uses the AXIS frame.
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


# Label for repeated fitting of the Gaussian.
refit:

# Obtain the spectral range interactively.
# ========================================

# Plot the ripped spectrum.
linplot "${ripfile} device=${plotdev}" mode=histogram style="Colour(curves)=1" >& /dev/null

# Zoom if required.
if ( ${gotzoom} == "ASK") then
   echo " "
   echo -n "Zoom in (yes/no): "
   set zoomit = $<
else if ( ${gotzoom} == "TRUE") then
   set zoomit = "yes"
else
   set zoomit = "no"
endif

if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Get the lower limit.
# --------------------
   echo " "
   echo "  Left click on lower zoom boundary."
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}
   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set low_z = $pos[1]

# Clean up the cursor temporary file.
   rm -f ${curfile} >& /dev/null
   touch ${curfile}
   
# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}
   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set upp_z = $pos[1]

   echo " "
   echo "      Zooming:"
   echo "        Lower Boundary: ${low_z}"
   echo "        Upper Boundary: ${upp_z}"

# Clean up the CURSOR temporary file.
   rm -f ${curfile} >& /dev/null
   touch ${curfile}

# Label for repeated plotting of the spectrum.
rezoom:
 
# Replot the spectrum.
# --------------------
   linplot ${ripfile} xleft=${low_z} xright=${upp_z} \
           mode=histogram device=${plotdev} >& /dev/null
endif

# Grab the information needed by the FITGAUSS routine.
# ====================================================

# Get the lower mask boundary.
# ----------------------------

echo " "
echo "  Left click on the lower limit of the fitting region."
   
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set low_mask = $pos[1]

# Clean up the CURSOR temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}
 
# Get the upper mask boundary.
# ----------------------------

echo "  Left click on the upper limit of the fitting region."
    
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set upp_mask = $pos[1]

echo " " 
echo "      Fit Mask:"
echo "        Lower Mask Boundary: ${low_mask}"
echo "        Upper Mask Boundary: ${upp_mask}"

# Clean up the CURSOR temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}

# Get the continuum values.
# -------------------------

echo " "
echo "  Left click on your first estimate of the continuum."

cursor showpixel=true style="Colour(curves)=4" plot=hline \
       maxpos=1 device=${plotdev} >> ${curfile}

# Wait for the cursor output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set first_cont = $pos[2]

echo "  Left click on your second estimate of the continuum."

cursor showpixel=true style="Colour(curves)=4" plot=hline \
       maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set second_cont = $pos[2]

echo " "
echo "      Continuum:"
echo "        First Estimate: ${first_cont}"
echo "        Second Estimate: ${second_cont}"

# Evaluate the average continuum.
set cont = `calc exp="0.5*((${first_cont})+(${second_cont}))" prec=${prec}`
#set cont = `echo "${first_cont}+${second_cont}" | bc`
#set scale = `echo "${cont}/2.0" | bc`
#set remainder = `echo "${cont}%2.0" | bc`
#set cont = `echo "${scale}+${remainder}" | bc`

echo "        Average Value: ${cont}"

# Get the line-peak position.
# ---------------------------

echo " " 
echo "  Left click on the line peak."

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set position = $pos[1]
set peak = $pos[2]

echo " "
echo "      Line Position:"
echo "        Peak Position: ${position}"
echo "        Peak Height: ${peak} ${unit}"

# Clean up the CURSOR temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}

# Get the fwhm left side.
# -----------------------

echo " " 
echo "  Left click on the left hand edge of the FWHM."

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set fwhm_low = $pos[1]

# Clean up the CURSOR temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}

# Get the fwhm right side.
# ------------------------

echo "  Left click on the right hand edge of the FWHM."
echo " "

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor`
set fwhm_upp = $pos[1]

echo "      FWHM:"
echo "        Lower Bound: ${fwhm_low}"
echo "        Upper Bound: ${fwhm_upp}"

# Clean up the CURSOR temporary file.
rm -f ${curfile} >& /dev/null
touch ${curfile}

# Evaluate the fwhm.
# ------------------

#set fwhm_low = `echo ${fwhm_low} | sed 's/E/\\*10\\^/' | sed 's/+//'`
#set fwhm_upp = `echo ${fwhm_upp} | sed 's/E/\\*10\\^/' | sed 's/+//'`
#set fwhm = `echo "scale = 15; ${fwhm_upp}-${fwhm_low}" | bc`
set fwhm = `calc exp="(${fwhm_upp})-(${fwhm_low})" prec=${prec}`
echo "        FWHM: ${fwhm}"
echo " "

# Get the rest-frame spectral unit.
# ---------------------------------

# Inform the user.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
echo " "

# Fit the line.
# =============

echo "      Fitting:"

fitgauss \
    "in=${ripfile} mask1=${low_mask} mask2=${upp_mask} cont=${cont} "\
    "peak=${peak} fwhm=${fwhm} reguess=no remask=no ncomp=1 "\
    "cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
    "centre=${position} logfil=${fitfile} device=${plotdev} "\
    "dialog=f" >& /dev/null 

# Check to see whether or not fitting was successful.
if ( ! -e $fitfile ) then
   echo "        No fit available"
   echo ""
   echo -n "Refit (yes/no): "
   set fitgood = $<
   echo " "
   if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
      rm -f ${fitfile} >& /dev/null
      goto cleanup 
   else
      if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
         goto rezoom
      else
         goto refit
      endif
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
echo "        Centre Position: ${centre_fit} +- ${centre_err}"
echo "        Peak Height: ${peak_height} +- ${peak_err} ${unit}"
echo "        FWHM: ${fwhm_fit} +- ${fwhm_err}"
echo "        Line integral: ${integral} +- ${integral_err} ${unit}"

# Fit ok?
echo " "

if ( ${forcefit} == "FALSE" ) then
   echo -n "Fit ok? (yes/no): "
   set fitgood = $<
   echo " "
else
   set fitgood = yes
endif

if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
   rm -f ${fitfile} >& /dev/null
   if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
      goto rezoom
   else
      goto refit
   endif
else
   rm -f ${fitfile} >& /dev/null
endif

# Loop through the entire datacube.
# =================================

# Create a couple of results files.
touch ${mapfile}
touch ${varfile}

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
endif

date > ${tmpdir}/${user}/pmap_time.dat 

# Fit the cube in a similar fashion.
echo "      Fitting:"
while( $y <= ${ubnd[2]} )
   while ( $x <= ${ubnd[1]} )

# Extract the spectrum at the current spatial position.
      set specfile = "${tmpdir}/${user}/${x}_${y}.sdf"
      ndfcopy "in=${infile}($x,$y,) out=${specfile}" \
              "trim=true trimwcs=true"

# Fit the Gaussian to the spectrum.
      fitgauss \
         "in=${specfile} mask1=${low_mask} mask2=${upp_mask} "\
         "cont=${cont} peak=${peak} fwhm=${fwhm} reguess=no remask=no "\
         "ncomp=1 cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
         "centre=${position} logfil=${fitfile} device=! "\
         "dialog=f" >& /dev/null

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

# Something has gone wrong.  Store a null value for this fit.
         set condition = `echo "if ($peak_height < 0) 1" | bc`
         if ( $condition == 1 ) then
            echo "        Spectrum at ($x,$y)"
            set line = "${line} -9999.99"  
            if ( ${dovar} == "TRUE" ) then
               set vars = "${vars} -9999.99"  
            endif
         else

            echo "        Spectrum at ($x,$y): $peak_height +- $peak_err" 

            set line = "${line} ${peak_height}"
            if ( ${dovar} == "TRUE" ) then
               if ( ${centre_err} == "nan" || ${centre_err} == "INF" ) then

# Set variance to the null value.
                  set vars = "${vars} -9999.99"

               else

# Set variance tp $peak_err.
                  set vars = "${vars} ${peak_err}"
               endif
            endif
         endif

# No fit file.  Set dummy values.
      else
         echo "       Spectrum at ($x,$y)" 
         set line = "${line} -9999.99" 
         if ( ${dovar} == "TRUE" ) then
            set vars = "${vars} -9999.99"
         endif
      endif

# Remove temporary files for the current pixel.
      rm -f ${fitfile} >& /dev/null
      rm -f ${specfile}

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

# Set the null values to bad (VAL__BADR).
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

# Attach the VARIANCE array.
if ( ${dovar} == "TRUE" ) then
   echo "        Converting: Attaching VARIANCE array." 
   ascii2ndf in=${varfile} comp="Variance" out=${outfile} \
             shape="[${dims[1]},${dims[2]}]" \
             maxlen=2048 type='_real'

# Replace its null values with the bad values.
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
           "axes=yes margin=!" >& /dev/null
   echo "        Contour: White-light image with equally spaced contours." 
   contour "ndf=${colfile} device=${plotdev} clear=no mode=equi"\
           "axes=no ncont=${numcont} pens='colour=2' margin=!" >& /dev/null 
   echo " "
endif

# Loop for manual fitting.
# ------------------------

set loop_var = 1
if ( ${forcefit} == "FALSE" ) then
   if ( ${plotspec} == "FALSE" ) then
      lutcol device=${plotdev}
      echo "      Plotting:"
      echo "        Display: Line-strength map using percentile scaling." 
      display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
              "axes=yes margin=!" >& /dev/null
      echo "        Contour: White-light image with equally spaced contours." 
      contour "ndf=${colfile} device=${plotdev} clear=no mode=equi"\
              "axes=no ncont=${numcont} pens='colour=2' margin=!" >& /dev/null 
   endif
   while ( ${loop_var} == 1 )

      echo " "
      echo -n "Refit a point (yes/no): "
      set refit = $<
      echo " "

      if ( ${refit} == "yes" || ${refit} == "y" ) then

# Copy the current output file to a _tmp file.
         mv -f ${outfile}.sdf ${outfile}_tmp.sdf

# Grab the X-Y position.
         echo " "
         echo "  Left click on pixel to be extracted."
   
         cursor showpixel=true style="Colour(marker)=2" plot=mark \
             maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# Wait for CURSOR output then get X-Y co-ordinates from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = \
           `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.
         set xpix = `echo $pos[1] | awk '{split($0,a,"."); print int(a[1])}'`
         set ypix = `echo $pos[2] | awk '{split($0,a,"."); print int(a[1])}'`

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}

# Extract the spectrum.
# =====================
         echo " "
         echo "      Extracting:"
         echo "        (X,Y) pixel: ${xpix},${ypix}"

# Extract the spectrum from the cube.
         ndfcopy in="${infile}($xpix,$ypix,)" out=${ripfile} \
                 trim=true trimwcs=true

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

# Label for repeated fitting of the Gaussian.
manual_refit: 

# Plot the ripped spectrum.
         linplot "${ripfile} mode=histogram device=${plotdev}" >& /dev/null

# Zoom if required.
         if ( ${gotzoom} == "ASK") then
            echo " "
            echo -n "Zoom in (yes/no): "
            set zoomit = $<
         else if ( ${gotzoom} == "TRUE") then
            set zoomit = "yes"
         else
            set zoomit = "no"
         endif

         if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Get the lower limit.
# --------------------
            echo " "
            echo "  Left click on lower zoom boundary"
   
            cursor showpixel=true style="Colour(curves)=3" plot=vline \
                   maxpos=1 device=${plotdev} >> ${curfile}
            while ( ! -e ${curfile} ) 
               sleep 1
            end
            set pos = `parget lastpos cursor`
            set low_z = $pos[1]

# Clean up the CURSOR temporary file.
            rm -f ${curfile} >& /dev/null
            touch ${curfile}
   
# Get the upper limit.
# --------------------
            echo "  Left click on upper zoom boundary"
   
            cursor showpixel=true style="Colour(curves)=3" plot=vline \
                   maxpos=1 device=${plotdev} >> ${curfile}
            while ( ! -e ${curfile} ) 
               sleep 1
            end
            set pos = `parget lastpos cursor`
            set upp_z = $pos[1]

            echo " "
            echo "      Zooming:"
            echo "        Lower Boundary: ${low_z}"
            echo "        Upper Boundary: ${upp_z}"

# Clean up the CURSOR temporary file.
            rm -f ${curfile} >& /dev/null
            touch ${curfile}

# Label for repeated fitting of the Gaussian.
manual_rezoom:

# Replot the spectrum.
            linplot "${ripfile} xleft=${low_z} xright=${upp_z}"\
                    mode=histogram "device=${plotdev}" >& /dev/null
         endif
       
# Grab the information needed by the FITGAUSS routine.
# ====================================================

# Get the lower mask boundary.
# ----------------------------
         echo " "
         echo "  Left click on the lower limit of the fitting region."
   
         cursor showpixel=true style="Colour(curves)=2" plot=vline \
                maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set low_mask = $pos[1]

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}
 
# Get the upper mask boundary.
# ----------------------------
         echo "  Left click on the upper limit of the fitting region."
    
         cursor showpixel=true style="Colour(curves)=2" plot=vline \
                maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set upp_mask = $pos[1]

         echo " " 
         echo "      Fit Mask:"
         echo "        Lower Mask Boundary: ${low_mask}"
         echo "        Upper Mask Boundary: ${upp_mask}"

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}

# Get the continuum values.
# -------------------------
         echo " "
         echo "  Left click on your first estimate of the continuum."

         cursor showpixel=true style="Colour(curves)=4" plot=hline \
                maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set first_cont = $pos[2]

         echo "  Left click on your second estimate of the continuum."

         cursor showpixel=true style="Colour(curves)=4" plot=hline \
                maxpos=1 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
           sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set second_cont = $pos[2]

         echo " "
         echo "      Continuum:"
         echo "        First Estimate: ${first_cont}"
         echo "        Second Estimate: ${second_cont}"

# Derive the average continuum.
         set cont = `calc exp="0.5*((${first_cont})+(${second_cont}))" prec=${prec}`
#        set cont = `echo "${first_cont}+${second_cont}" | bc`
#        set scale = `echo "${cont}/2.0" | bc`
#        set remainder = `echo "${cont}%2.0" | bc`
#        set cont = `echo "${scale}+${remainder}" | bc`

         echo "        Average Value: ${cont}"

# Get the peak position.
# ----------------------
         echo " " 
         echo "  Left click on the line peak"

         cursor showpixel=true style="Colour(marker)=3" plot=mark \
                maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set position = $pos[1]
         set peak = $pos[2]

         echo " "
         echo "      Line Position:"
         echo "        Peak Position: ${position}"
         echo "        Peak Height: ${peak} ${unit}"

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}

# Get the fwhm left side.
# -----------------------
         echo " " 
         echo "  Left click on the left-hand edge of the FWHM"

         cursor showpixel=true style="Colour(marker)=3" plot=mark \
                maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab position.
         set pos = `parget lastpos cursor`
         set fwhm_low = $pos[1]

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}

# Get the fwhm right side.
# ------------------------
         echo "  Left click on the right-hand edge of the FWHM"
         echo " "

         cursor showpixel=true style="Colour(marker)=3" plot=mark \
                maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# Wait for CURSOR output then get spectral-unit,continuum measurement from 
# the temporary file created by KAPPA:CURSOR.
         while ( ! -e ${curfile} ) 
            sleep 1
         end

# Grab the position.
         set pos = `parget lastpos cursor`
         set fwhm_upp = $pos[1]

         echo "      FWHM:"
         echo "        Lower Bound: ${fwhm_low}"
         echo "        Upper Bound: ${fwhm_upp}"

# Clean up the CURSOR temporary file.
         rm -f ${curfile} >& /dev/null
         touch ${curfile}

# Evaluate the fwhm.
# ------------------
#        set fwhm_low = `echo ${fwhm_low} | sed 's/E/\\*10\\^/' | sed 's/+//'`
#        set fwhm_upp = `echo ${fwhm_upp} | sed 's/E/\\*10\\^/' | sed 's/+//'`
#        set fwhm = `echo "scale = 15; ${fwhm_upp}-${fwhm_low}" | bc`
         set fwhm = `calc exp="(${fwhm_upp})-(${fwhm_low})" prec=${prec}`
         echo "        FWHM: ${fwhm}"
         echo " "

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
            echo -n "Refit (yes/no): "
            set fitgood = $<
            echo " "
            if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
               rm -f ${fitfile} >& /dev/null
               goto cleanup
            else
               if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
                  goto manual_rezoom
               else
                  goto manual_refit
               endif
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
         echo "        Centre Position: ${centre_fit} +- ${centre_err}"
         echo "        Peak Height: ${peak_height} +- ${peak_err} ${unit}"
         echo "        FWHM: ${fwhm_fit} +- ${fwhm_err}"
         echo "        Line integral: ${integral} +- ${integral_err} ${unit}"

# Fit ok??
         echo " "

         if ( ${forcefit} == "FALSE" ) then
            echo -n "Fit ok? (yes/no/quit): "
            set fitgood = $<
            echo " "
         else
            set fitgood = yes
         endif

         if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
            rm -f ${fitfile} >& /dev/null
            if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then
               goto manual_rezoom
            else
              goto manual_refit
            endif
         else if ( ${fitgood} == "quit" || ${fitgood} == "q" ) then
            rm -f ${fitfile} >& /dev/null
            goto dropout 
         else    
            rm -f ${fitfile} >& /dev/null
         endif

# Report the peak_height and peak_err values.
         if ( ${dovar} == "TRUE" ) then
            echo  "        (X,Y) Pixel: ${xpix}:${ypix}"
            echo -n "        Peak Height: $peak_height " 
         else
            echo "        (X,Y) Pixel: ${xpix},${ypix}"
            echo "        Peak Height: $peak_height ${unit}" 
         endif
         echo " "

# Change the pixel value.
         set pixel = "${xpix}:${xpix},${ypix}:${ypix}"
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
            endif

# Move the output file to a temporary place holder.
            mv -f ${outfile}.sdf ${outfile}_tmp.sdf 

# Change the pixel value.
            set pixel = "${xpix}:${xpix},${ypix}:${ypix}"
            chpix in=${outfile}_tmp out=${outfile} comp="Variance" \
                  newval=${peak_err} section=\'${pixel}\'

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
         lutcol device=${plotdev}
         echo " "
         echo "      Plotting:"
         echo "        Display: Line strength map using percentile scaling." 
         display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
                 "axes=yes margin=!" >& /dev/null

# Cleanup temporary peakmap files, salvage ${outfile}_tmp in the case
# where there is no existing $outfile (i.e. CHPIX has not run).
         if ( -e ${outfile}_tmp.sdf ) then
            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf >& /dev/null
            else
               mv -f ${outfile}_tmp.sdf ${outfile}.sdf
            endif
         endif

# End of manual refitting loop.
      else

# Drop out of while loop.
         set loop_var = 0
      endif
   end
endif

# Dropout point for anaborted fitting, try and salvage already existing
# velocity maps that may be lying around instead of throwing them away.
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

rm -f ${curfile} >& /dev/null
rm -f ${colfile}.sdf >& /dev/null
rm -f ${ripfile}.sdf >& /dev/null
rm -f ${fitfile} >& /dev/null
rm -f ${mapfile} >& /dev/null
rm -f ${varfile} >& /dev/null
rm -f ${tmpdir}/${user}/pmap_time.dat >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null
