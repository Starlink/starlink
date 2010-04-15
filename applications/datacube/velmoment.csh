#!/bin/csh
#+
#  Name:
#     velmoment.csh
#
#  Purpose:
#     Builds a velocity map from a three-dimensional IFU NDF from the
#     intensity-weighted spectral co-ordinates.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     velmoment [-b string] [-c number] [-ci index] [-i filename]
#               [-o filename] [-p] [-r number] [-s system] [-z/+z]
#
#  Description:
#     This shell script processes a three-dimensional IFU NDF to form
#     a velocity map.
#
#     If you request zooming the script first presents you with a
#     white-light image of the cube.  You can then select the lower
#     and upper spatial limits to plot using the cursor.  You can
#     instead supply an NDF section with the filename to define both
#     spatial and spectral limits to analyse, and from which to create
#     the output velocity map.  You may average spectra in the chosen
#     region by specifying compression factors in the spatial domain.

#     The script then derives the intensity weighted co-ordinate of
#     each spatially averaged spectrum, and converts the data units
#     into a velocity.  You may view this image drawn with a key
#     (option -d), and overlay a contour plot of the white-light image
#     (option -c).
#
#  Parameters:
#     -b string
#       The number of spectra to block average along the x and y axes
#       respectively.  This should be a comma-separated list or a single
#       number; the latter case applies the same compression factor to
#       both spatial axes.  The numbers must be positive integers.  [1]
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
#     -i filename
#       The script will use this as its input file, the specified file
#       should be a three-dimensional NDF.  By default the script will
#       prompt for the input file.  If there are multiple spectral lines
#       present, you should supply an NDF section after the name to
#       restrict the spectral range analysed to a specific line and
#       its environs.
#     -o filename
#       The filename for the output NDF of the velocity map.
#     -p
#       The script will plot the final image map to the current display
#       as well as saving it to an NDF file.  Additionally it will over-
#       plot the white-light image as a contour map for comparison.  [FALSE]
#     -r number
#       Rest-frame spectral unit of the line being fitted.
#     -s system
#       The co-ordinate system for velocities.  Allowed values are:
#          "VRAD" -- radio velocity;
#          "VOPT" -- optical velocity;
#          "ZOPT" -- redshift; and
#          "VELO" -- relativistic velocity.
#       If you supply any other value, the default is used.  ["VOPT"]
#     -z
#       The script will automatically prompt to select a region to zoom
#       before prompting for the region of interest.  [TRUE]
#     +z
#       The program will not prompt for a zoom before requesting the region
#       of interest. [FALSE]
#
#  Notes:
#     -  The compression is trimmed, so that only compression-factor
#     multiples of original pixels are included in the plot.
#     -  The spatial averaging is aligned to obtain the expected number
#     of pixels irrespective of the pixel origin of the input cube.
#     Note that this may not be suitable if you wish to preserve alignment
#     with another compressed dataset.  See KAPPA:COMPAVE parameter ALIGN
#     for more details.
#     -  The velocity map display scales between the 2 and 98 percentiles.
#     The map uses a false-colour spectrum-like colour table so that
#     low-velocity regions appear in blue and high-velocity regions
#     appear in red.
#     -  If the cube is compressed spatially, so is the contour map.
#     -  For NDFs in the UK data-cube format, where there is no SPECTRUM
#     or DSBSPECTRUM Domain in the WCS Frames, the data are first
#     collapsed in their native wavelengths in Angstrom, then the pixel
#     values are converted to VOPT using the simple formula
#     c * (w -R) / R, where w is the intensity-weighted wavelength, R is
#     the rest-frame wavelength for the chosen spectral line, and c is the
#     velocity of light in km/s.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2006 March 6 (MJC):
#       Original version.
#     2006 March 9 (MJC):
#       Added -ci option.  Corrected the NDF name extraction when both
#       the file extension and an NDF section are supplied; this is via
#       the new checkndf script that also checks for a degenerate third
#       axis.
#     2006 March 16 (MJC):
#       Formed VOPT velocities for UK data-cube format NDFs that do
#       not have a SpecFrame.  Collapsed the spatially averaged cube
#       for the contour plot so that its co-ordinate system matches
#       the velocity map.  Retain any supplied spectral-axis section
#       if a spatial region is selected by cursor.  Corrected the logic
#       when deciding whether or not to create a new SpecFrame.
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2000-2006 Central Laboratory of the Research Councils
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Clean up from previous runs.
rm -f ${tmpdir}/${user}/velmom* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/velmom_col"
set cmpfile = "${tmpdir}/${user}/velmom_cmp"

# Set options access flags.
set drawcontours = "TRUE"
set gotcmp = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotrest = "FALSE"
set gotzoom = "ASK"
set plotmap = "FALSE"

# Specify the number of contours used to display the white-light image.
set numcont = 15

# Other defaults.
set ci = 0
set velsys = "VOPT"
set vunits = "km/s"
set cmpstring = 1

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -b:    # blocking factors
      shift args
      set gotcmp = "TRUE"
      set cmpstring = $args[1]
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
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output velocity map
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectra
      set plotmap = "TRUE"
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

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain the compression factor parameter value.
# ==============================================

# Set the default.
if ( $gotcmp == "FALSE" ) then
   set cmpstring = "2"
endif

# Extract the compression factors.
set cmpfactors = `echo $cmpstring | awk 'BEGIN {FS=","}{print $1, $2}'`
if ( $#cmpfactors == 1 ) then
   set cmpfactors = ( $cmpfactors[1] $cmpfactors[1] )
endif

if ( $cmpfactors[1] < 1 || $cmpfactors[2] < 1 ) then
   echo "VELMOMENT_ERR: compression factors must be positive."
   exit
endif

# See if we actually need to compress.
set blockave = "FALSE"
if ( $cmpfactors[1] > 1 ||  $cmpfactors[1] > 1 ) then
   set blockave = "TRUE"
   set cmpfac = "${cmpfactors[1]},${cmpfactors[2]},1"
endif

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s velmoment
if ( $status == 1 ) exit
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

# Determine if we need to create or select a SPECTRUM-domain WCS Frame.
# =====================================================================

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# Check that the current frame is SPECTRUM or SKY-SPECTRUM or
# SKY-DSBSPECTRUM.   We can re-use the last trace of the input NDF.
set create_specframe = "TRUE"
set change_frame = "FALSE"
set curframe = `parget current ndftrace`
set domain = `parget fdomain"($curframe)" ndftrace`
if ( "$domain" != "SPECTRUM" && "$domain" != "SKY-SPECTRUM" && \
     "$domain" != "SKY-DSBSPECTRUM"  ) then
   set change_frame = "TRUE"
else
   set create_specframe = "FALSE"
endif

# Next question: is there a SPECTRUM frame to switch to in order to
# permit velocity calculations?  Loop through all the WCS Frames, looking
# for a SPECTRUM (or composite SKY-SPECTRUM or SKY-DSBSPECTRUM).
if ( $change_frame == "TRUE" ) then
   set nframe = `parget nframe ndftrace`
   set i = 1
   while ( $i <= $nframe && $create_specframe == "TRUE" )
      set domain = `parget fdomain"($i)" ndftrace`
      if ( "$domain" == "SPECTRUM" || "$domain" == "SKY-SPECTRUM" || \
           "$domain" == "SKY-DSBSPECTRUM" ) then
         set domindex = $i
         set $create_specframe = "FALSE"
      endif
      @ i++
   end

# We need to create a WCS SPECTRUM domain, and hence not change frame.
   if ( "$create_specframe" == "TRUE" ) then
      set $change_frame = "FALSE"

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
         echo "The input NDF does not have a SKY-SPECTRUM or SKY-DSBSPECTRUM"
         echo "WCS Domain, or it is not in the UK data-cube format.  There"
         echo "is insufficient information to convert the current WCS Frame"
         echo "to one of these and hence transform the spectral axis"
         echo "co-ordinates to type wavelength to calculate velocities."
         echo " "

         if ( "$slabel" == "" ) then
            echo "Assuming that the undefined System is Wavelength."
            set slabel = "Wavelength"
         endif
         if ( "$sunits" == "" ) then
            echo "Assuming that the undefined unit is Angstrom."
            set sunits = "Angstrom"
         endif
      endif
   endif

# Override the chosen velocity system for this radio-/submm-specific
# Domain.
else if ( "$domain" == "SKY-DSBSPECTRUM" ) then
   set velsys = "VRAD"
endif

# Select a region to plot.
# ========================

# Setup the plot device.
set plotdev = "xwin"

# Zoom if required.
# -----------------
if ( ${gotzoom} == "ASK") then
   echo " "
   echo -n "Display white-light image to select subset (yes/no): "
   set zoomit = $<
else if ( ${gotzoom} == "TRUE") then
   set zoomit = "yes"
else
   set zoomit = "no"
endif

# Show the white-light image.
# ===========================

if ( ${zoomit} == "yes" || ${zoomit} == "y" || \
     $drawcontours == "TRUE" ) then

# Collapse white-light image.
   echo " "
   echo "      Collapsing:"
   echo "        White-light image: ${dims[1]} x ${dims[2]}"
   collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null
endif

if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Display the collapsed image.
   gdclear device=${plotdev}
   paldef device=${plotdev}
   lutgrey device=${plotdev}
   display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" reset >&/dev/null

# Get the lower limit.
# --------------------
   echo " "
   echo "  Left click on lower zoom boundary."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# exterior NINT replaces the bug/feature -0 result with the desired 0.
   set x1 = $xgrid
   set y1 = $ygrid

# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
   set x2 = $xgrid
   set y2 = $ygrid

# Average the spectra in a chosen spatial region.
# ===============================================

# Just in case the user cannot follow the instructions.
   set xl = `calc exp="min($x1,$x2)" prec=_REAL`
   set yl = `calc exp="min($y1,$y2)" prec=_REAL`
   set xu = `calc exp="max($x1,$x2)" prec=_REAL`
   set yu = `calc exp="max($y1,$y2)" prec=_REAL`

   echo " "
   echo "      Zooming:"
   echo "      Extracting:"
   echo "        Lower (X,Y): ${xl},${yl}"
   echo "        Upper (X,Y): ${xu},${yu}"
   echo " "

# Set the bounds but not forgetting any spectral limits (say to focus
# on a single line) supplied with the NDF.
   set bnd = "(${xl}:${xu},${yl}:${yu},${lbnd[3]}:${ubnd[3]})"

# Use the supplied bounds' string from checkndf.csh for the cube
# age when there is no graphical selection of spatial bounds.
else
   set bnd = "$ndf_section"
endif

# Do the averaging in the selected spatial region.
if ( $blockave == "TRUE" ) then
   compave "in=${infile}${bnd} compress=[${cmpfac}] out=${cmpfile} trim align=first"

# Create a new white-light image of the selected region.  This is the
# easiest way to overlay the correct region of the white image on the
# velocity map, as the pixel scales are different between the original
# white-light image and the spatially averaged collapsed cube.
   if ( $drawcontours == "TRUE" ) then
      collapse "in=${cmpfile} out=${colfile} axis=3" >& /dev/null
   endif
else
   set cmpfile = "${infile}${bnd}"
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


# Create the velocity WCS.
# ========================

# Deal with the missing SpecFrame later for UK data-cube format
# NDFs.
if ( ${create_specframe} == "FALSE" ) then

# The spectrum has a SPECTRUM domain WCS Frame, so select it for
# velocity calculations.
   if ( $change_frame == "TRUE" ) then
      wcsframe "ndf=${cmpfile} frame=${domindex}" >& /dev/null
      endif

# Specify the units, in case these weren't known originally.
   axunits "ndf=${cmpfile} units=${sunits} dim=3" >& /dev/null

# Set the rest-frame value, if it's not already stored in the WCS
# attributes.  For this to work the current WCS Frame must be a
# SpecFrame.  At present there is no test.
   if ( ${rest_known} == "FALSE" ) then
      wcsattrib "ndf=${cmpfile} mode=set name=RestFreq(3) "\
                "newval='${rest_coord} ${sunits}'" >& /dev/null
   endif

# Reset the System of the current WCS Frame to velocities in the desired
# system.
   wcsattrib "ndf=${cmpfile} mode=set name=System(3) newval=${velsys}"
endif

# Create the intensity-weighted image.
# ====================================

# Setup output filename.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
   echo " "
endif

ndftrace ${cmpfile} >& /dev/null
set cdims = `parget dims ndftrace`

echo "      Collapsing:"
echo "        Intensity-weighted co-ordinate image: ${cdims[1]} x ${cdims[2]}"
collapse "in=${cmpfile} out=${outfile} axis=3 estimator=iwc" >& /dev/null
settitle "ndf=${outfile} title='Intensity-weighted spectral co-ordinate Image'"

# If the supplied NDF was in UK data-cube format and there was no
# SPECTRUM Domain, the collapsed data will be in the Wavelength system.
# There are two ways to handle this state.
#
# The first is to create a new SpecFrame.  As we have a cube, it would
# require ATOOLS  to form a new Compound Frame with some arbitrary spatial
# co-ordinates and a SpecFrame, both unit mapped from the AXIS Domain
# before collapsing.
#
# The alternative is to modify the values and units post collapse.
# This is more restrictive in that it's practicable to offer only
# a limited selection of velocity systems.  At present the
# earlier test for a missing SpecFrame only occurs for a UK data-cube
# format with LAMBDA as the label, with Angstrom units so the
# converted system is VOPT.
if ( $create_specframe == "TRUE" ) then
    setenv KAPPA_REPLACE 1
    maths ia=${outfile} out=${outfile} exp="'300000.0*(IA/${rest_coord}-1.0)'"
    unsetenv KAPPA_REPLACE

    setunits ndf=${cmpfile} units="km/s"
endif

# Plot the output velocity map.
# =============================

if ( $plotmap == "TRUE" ) then

# Use the whole plotting area.
   gdclear device=${plotdev}

# Leave room for the contour-heights key.
   if ( $drawcontours == "TRUE" ) then
      set keyoff = 0.12
   else
      set keyoff = 0.001
   endif

# A wild stab at a value.  This may need revision in practice with
# larger velocities.  Ideally we'd like to place the label to the right
# of the key, but it's not obvious how to obtain the length of the
# velocity-axis numerical annotations including any decimal places.
# The limiting values are insufficient.
   set gap = 0.4

# Use a spectral colour table terminating with blue and red.
   lutspec device=${plotdev}
   echo "      Plotting:"
   echo "        Display: Velocity map using percentile scaling."
   display "${outfile} device=${plotdev} mode=per percentiles=[2,98]"\
           "axes=yes margin=! key keypos=${keyoff} " \
           keystyle="'TextLab(1)=1,TextLabGap(1)=${gap},Label(1)=Velocity in ${vunits}'" >& /dev/null
   if ( $drawcontours == "TRUE" ) then
      echo "        Contour: White-light image with equally spaced contours."
      contour "ndf=${colfile} device=${plotdev} clear=no mode=equa"\
              "keypos=[0.015,1] keystyle='Digits(2)=4,Size=1.2' "\
              "axes=no ncont=${numcont} pens='colour=${ci}' margin=!" >& /dev/null
   endif
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/velmom* >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null
