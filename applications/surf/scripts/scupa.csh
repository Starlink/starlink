#!/bin/csh

# Script to run scuquick on an observation, display and overlay array
#   Tim Jenness (1997-06-19)

# The prologue is at the end.

# 1 Argument required: the demodulated data file

# Check if Starlink was initialised
if !($?STAR_LOGIN) then

# Start up Starlink (if possible)

  if (-e ${STARLINK_DIR}/etc/login) then
    source ${STARLINK_DIR}/login
  else
    echo 'Error: Starlink system not initialised'
    exit 1
  endif

endif

# Switch off messages
alias echo "echo > /dev/null"

# First start up KAPPA
source $KAPPA_DIR/kappa.csh

# Second start up surf

source $SURF_DIR/surf.csh

# Unalias

unalias echo

# Set the output filename to something non-obvious that can be deleted

set out = "scupa"

# Ask for the input filename if necessary

if ($#argv == 0) then
   echo -n "Input data set > "
   set in = $<
else
   set in = $1
endif

# Find out which sub-instrument we are talking about

if (-e ${in}.sdf) then
  set sub = `fitslist $in | grep SUB_1 | awk -F\' '{print $2}'`
else if (-e $DATADIR/${in}.sdf) then
  set sub = `fitslist $DATADIR/$in | grep SUB_1 | awk -F\' '{print $2}'`
else
  echo File not found in current directory or in DATADIR
  exit
endif

# Run scuquick so that
#   - no questions asked
#   - AzEl regrid
#   - no tau

echo "Running data through SURF tasks..."

scuquick -quick -notau -rebin -quiet --sub=$sub MSG_FILTER=quiet OUT_COORDS=AZ OUT=$out $in

# Now I have an output file which contains the subinstrument name!

set smallsub = `echo $sub | cut -c -3 | tr 'A-Z' 'a-z'`

set file = "${out}_${smallsub}_reb"

# Now display the file

if (-e ${file}.sdf) then

   echo "Data reduction complete. Now displaying..."

   display axes clear $file device=xwindows accept

   lutbgyrw device=xwindows

# And overlay the bolometer names

   scuover msg_filter=quiet device=xwindows style='colour(bol)=white' name
endif

# Remove some of the intermediate files if necessary

if (-e ${out}.sdf) \rm ${out}.sdf
if (-e ${out}_flat.sdf) \rm ${out}_flat.sdf
if (-e ${out}_${smallsub}_ext.sdf) \rm ${out}_${smallsub}_ext.sdf
if (-e ${file}.sdf) \rm ${file}.sdf

# and maybe even remove some photometry files

if (-e ${out}_${smallsub}_pht.sdf) \rm ${out}_${smallsub}_pht.sdf
if (-e ${out}_${smallsub}_pht.dat) \rm ${out}_${smallsub}_pht.dat

exit

# Prologue goes here until SST can use # as comments

*+
*  Name:
*    SCUPA
*
*  Purpose:
*    Show position angle of array
*
*  Type of Module:
*    C-shell script
*
*  Usage:
*    scupa [NDF]
*
*  Description:
*    This script reduces the specified demodulated data file, displays
*    the image using Az/El coordinates, and overlays the array in order to
*    show the position angle of the array during the observation.
*
*  ADAM Parameters:
*    NDF = NDF (Read)
*       Name of raw data file. Can be located in $DATADIR. The filename
*       will be requested if not specified on the command line.
*
*  Examples:
*    scupa 19970623_dem_0012
*       Reduce the data with scuquick, displays the image and overlays the
*       array using SCUOVER.
*
*  Notes:
*    Only JIGGLE/MAP, POINTING and PHOTOM observations can be used.
*
*  Related Applications:
*    SURF: SCUQUICK, SCUOVER;
*    KAPPA: DISPLAY
*
*  Implementation Status:
*    - Requires KAPPA.
*    - All files created by this task are removed.
*
*  Authors:
*    Tim Jenness (JACH)
*    {enter_new_authors_here}
*
*  History:
*    1997 June 19 (TIMJ):
*       Original version
*    2001 October 31 (TIMJ):
*       Do not want to bother with /star/etc/cshrc
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*

*  Copyright:
*     Copyright (C) 1995-2001 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*-


