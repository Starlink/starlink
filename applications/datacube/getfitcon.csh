#!/bin/csh
#+
#  Name:
#     getfitcon.csh
#
#  Purpose:
#     Uses the cursor to obtain Gaussian-fitting constraints from a
#     spectrum plot
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     source ${DATACUBE_DIR}/getfitcon.csh [-d device] -i spectrum [-r]
#            [-z zoom]
#
#  Description:
#     This shell script activates the cursor upon a selected graphics 
#     device, and returns the constraints: centre, peak level, FWHM, and
#     background to be used in the Gaussian fitting.
#
#  Parameters:
#     -d
#       The graphics device. [xwin]
#     -i
#       The input spectrum NDF.  There is no validation that the supplied
#       file has a one-dimensional data array.  The calling script
#       should do this with the checkndf script.
#     -r
#       If present, it requests that just refitting is required, i.e. the 
#       zooming enquiries are omitted.
#     -z 
#       The zoom status: allowed values are:
#          "ASK" --- prompt the user whether to zoom or not;
#          "TRUE" --- zooming required; and
#          "FALSE" --- no zooming is required.
#       Zooming involves the selction of a region of interest with the
#       graphics cursor.  [ASK]
#
#  Shell Variables:
#     $cont = REAL (Returned)
#        The continuum level, the average of two measurements with the
#        cursor.
#     $fwhm = REAL (Returned)
#        The FWHM of the Gaussian, derived from the lower and upper
#        extents measured with the cursor.
#     $low_mask = REAL (Returned)
#        Fitting region lower co-ordinate limit measured with the cursor.
#     $upp_mask = REAL (Returned)
#        Fitting region upper co-ordinate limit measured with the cursor.
#     $peak = REAL (Returned)
#        The peak intensity level of the Gaussian, measured with the cursor.
#     $position = REAL (Returned)
#        The AXIS co-ordinate of the peak of the Gaussian, measured with
#        the cursor.
#     $sunits = CHARACTER (Given)
#        The units of he spectral co-ordinates.  This must be defined even
#        if the string is null.
#     $unit = CHARACTER (Given)
#        The units of the data values.  This must be defined even if the
#        string is null.
#     $zoomit = CHARACTER (Returned)
#        Whether or not zooming was selected.  Value "yes" indicates 
#        zooming is required.

#  Notes:
#      -  It is assumed that the co-ordinate Frame is stored in the AGI
#      database.  Normally the current Frame is used.
#      -  It uses /tmp/<user>/getcurpos.tmp to record the output from 
#      KAPPA:CURSOR.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2007 May 5 (MJC):
#       Original version derived from a peakmap.csh extract.
#     2008 June 29 (MJC):
#       Fixed bug with -z option that ignored its value, insisting on 
#       zooming.
#
#  Copyright:
#     Copyright (C) 2007-2008 Science and Technology Research Council.
#     All Rights Reserved.
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"
if ( ! -e ${tmpdir}/${user} ) mkdir ${tmpdir}/${user} >& /dev/null

# Options.
set gfc_graphdev = "xwin"
set gfc_gotinfile = "FALSE"
set gfc_gotzoom = "ASK"
set gfc_refit = "FALSE"

set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -d:    # graphics device
      shift args
      set gfc_graphdev = $args[1]
      shift args
      breaksw
   case -i:    # input spectrum NDF
      shift args
      set gfc_gotinfile = "TRUE"
      set gfc_infile = $args[1]
      shift args
      breaksw
   case -r:    # only obtain fit initial guesses?
      set gfc_refit = "TRUE"
      shift args
      breaksw 
   case -z:    # zoom mode
      shift args
      set gfc_gotzoom = $args[1]
      shift args
      breaksw 
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Obtain the spectral range interactively.
# ========================================

# Plot the ripped spectrum.
linplot "${gfc_infile} device=${gfc_graphdev}" mode=histogram \
         style="Colour(curves)=1" >& /dev/null

# Skip the zooming if just want to redo fitting.
if ( ${gfc_refit} != "TRUE" ) then

# Zoom if required.
   if ( ${gfc_gotzoom} == "ASK") then
      echo " "
      echo -n "Zoom in (yes/no): "
      set zoomit = $<
   else if ( ${gfc_gotzoom} == "TRUE" ) then
      set zoomit = "yes"
   else
      set zoomit = "no"
   endif

   if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Get the lower limit.
# --------------------
      echo " "
      echo "  Left click on lower zoom boundary."
      source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
      set low_z = $xpos

# Get the upper limit.
# --------------------
      echo "  Left click on upper zoom boundary."
      source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
      set upp_z = $xpos

      echo " "
      echo "      Zooming:"
      echo "        Lower Boundary: ${low_z}"
      echo "        Upper Boundary: ${upp_z}"
 
# Replot the spectrum.
# --------------------
      linplot ${gfc_infile} xleft=${low_z} xright=${upp_z} \
              mode=histogram device=${gfc_graphdev} >& /dev/null
   endif
endif

# Grab the information needed by the FITGAUSS routine.
# ====================================================

# Get the lower mask boundary.
# ----------------------------

echo " "
echo "  Left click on the lower limit of the fitting region."
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a X
set low_mask = $xpos

# Get the upper mask boundary.
# ----------------------------

echo "  Left click on the upper limit of the fitting region."
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a X
set upp_mask = $xpos

echo " " 
echo "      Fit Mask:"
echo "        Lower Mask Boundary: ${low_mask}"
echo "        Upper Mask Boundary: ${upp_mask}"

# Get the continuum values.
# -------------------------

echo " "
echo "  Left click on your first estimate of the continuum."
source ${DATACUBE_DIR}/getcurpos.csh -ci 4 -a Y
set first_cont = $ypos

echo "  Left click on your second estimate of the continuum."
source ${DATACUBE_DIR}/getcurpos.csh -ci 4 -a Y
set second_cont = $ypos

echo " "
echo "      Continuum:"
echo "        First Estimate: ${first_cont}"
echo "        Second Estimate: ${second_cont}"

# Evaluate the average continuum.
set cont = `calc exp="0.5*((${first_cont})+(${second_cont}))" prec=${prec}`

echo "        Average Value: ${cont}"

# Get the line-peak position.
# ---------------------------

echo " " 
echo "  Left click on the line peak."
source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a XY
set position = $xpos
set peak = $ypos

echo " "
echo "      Line Position:"
echo "        Peak Position: ${position} ${sunits}"
echo "        Peak Height: ${peak} ${unit}"

# Get the fwhm left side.
# -----------------------

echo " " 
echo "  Left click on the left hand edge of the FWHM."
source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a XY
set fwhm_low = $xpos

# Get the fwhm right side.
# ------------------------

echo "  Left click on the right hand edge of the FWHM."
echo " "
source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a XY
set fwhm_upp = $xpos

echo "      FWHM:"
echo "        Lower Bound: ${fwhm_low}"
echo "        Upper Bound: ${fwhm_upp}"

# Evaluate the fwhm.
# ------------------

set fwhm = `calc exp="(${fwhm_upp})-(${fwhm_low})" prec=${prec}`
echo "        FWHM: ${fwhm}"
echo " "

cleanup:

exit