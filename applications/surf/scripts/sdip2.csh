#!/bin/csh
#
#  Prologue at end since the UNIX SST routines do not understand
#  That hash is a comment character
#

# 1 Argument required: the skydip processed data suitable for skydip2

# Check if starlinkg was initialised

if !($?STAR_LOGIN) then

# Start up Starlink (if possible)

  if (-e ${STARLINK_DIR}/etc/login) then
    source ${STARLINK_DIR}/etc/login
  else
    echo 'Error: Starlink system not initialised'
    exit 1
  endif

endif


# This is the location of the psmerge command on the starlink system
# Need to make sure we do not use the non-starlink version
# Allow us to fallback to /star
if ( -d ${STARLINK_DIR}/bin ) then
    set starbin = ${STARLINK_DIR}/bin
else
    set starbin = /star/bin
endif

# Switch off messages
alias echo "echo > /dev/null"

# First start up KAPPA

# KAPPA
# use kappa.csh rather than the alias so that we dont have
# to source /star/etc/cshrc
source $KAPPA_DIR/kappa.csh

# Second start up surf

source $SURF_DIR/surf.csh

# Unalias

unalias echo

# Set the output filename to something non-obvious that can be deleted

set out = "sdip2_m"

# Run skydip

if ($#argv == 0) then
   skydip2  out=${out}
   # We need the input file for plotting
   set in = `parget in skydip2`
else
   set in = $1
   skydip2 out=${out} in=$in
endif

# Need a local copy of the input
set incp = "sdip2_cp"
ndfcopy ${in} ${incp}

# Get current value of LINCOL and SYMCOL from LINPLOT so that
# I can reset them at the end -only for old KAPPA (pre 0.13)

set old_style  = `parget style linplot`


# Now display the result with linplot

if (-e ${out}.sdf) then

   #  HARDCOPY
   # Generate the hardcopy first since I would like to leave 'symcol'
   # in a usable state for Xwindows (ie white)
   # Remove old plots (create one with touch to prevent an error)
    touch sdip_p1.eps
    touch sdip_p2.eps  # For linux
    \rm sdip_p1.eps* sdip_p2.eps*

    # Now we can setup a postscript plot if needed
    wcsframe $incp axis
    set kapargs = "mode=mark marker=2 style='Colour(Symbols)=black,colour(numlab)=black,colour(border)=black,Colour(ticks)=black'"
    linplot $incp $kapargs device='epsfcol_l;sdip_p1.eps'


    # then plot the model
    wcsframe ${out} axis
    set kapargs = "style='Colour(Lines)=red'"

    linplot ${out} mode=line noclear $kapargs device='epsfcol_l;sdip_p2.eps'

    # Remove the old output file so that we can tell if we really
    # created one here
    if (-e ${out}.eps) \rm ${out}.eps

    # And merge them - for the old KAPPA we want sdip_p1.eps.1
    # for the new KAPPA we can use sdip_p1.eps
    if ( -x ${starbin}/psmerge) then
       if (-e sdip_p1.eps && -e sdip_p2.eps) ${starbin}/psmerge -e sdip_p1.eps sdip_p2.eps >! ${out}.eps
    else
       echo Unable to locate Starlink psmerge command in directory ${starbin}.
    endif

    # Tell everyone where to look
    if (-e ${out}.eps) echo Hardcopy written to ${out}.eps

    # And now remove the intermediate files

    if (-e sdip_p2.eps) \rm sdip_p2.eps*
    if (-e sdip_p1.eps) \rm sdip_p1.eps*


#  XWINDOWS

    set kapargs = "mode=mark marker=2 style='Colour(Symbols)=green'"

    # Display the data first
    linplot $incp $kapargs device=xwindows

    # Now overlay the model

    set kapargs = "style='Colour(Lines)=red'"
    linplot ${out} mode=line $kapargs device=xwindows noclear

endif

# Reset linplot parameters
linplot device=! ndf=! style=$old_style > /dev/null

# Remove the intermediate files

if (-e ${out}.sdf) \rm ${out}.sdf
if (-e ${incp}.sdf) \rm ${incp}.sdf


exit


# Prologue goes here for SST

*+
*  Name:
*    SDIP2
*
*  Purpose:
*    Reduces and displays pre-processed skydip data using skydip2
*
*  Type of Module:
*    C-shell script
*
*  Usage:
*    sdip [NDF]
*
*  Description:
*    This script first runs the skydip2 task in order to fit the sky
*    parameters to the data. The sky input data and model are written to files
*    and are then displayed using KAPPA's linplot. A hardcopy is also
*    written to file sdip2.eps
*
*  ADAM Parameters:
*    NDF = NDF (Read)
*       Name of raw data file. The filename
*       will be requested if not specified on the command line.
*
*  Examples:
*    sdip2 s4a20110719_00032
*       Fits the skydip data in s4a20110719_00032.sdf and plots the result.
*
*  Implementation Status:
*    - Requires KAPPA.
*    - All files created by this task are removed.
*
*  Related Applications:
*    SURF: SKYDIP2;
*    KAPPA: LINPLOT
*
*  Authors:
*    Tim Jenness (JACH)
*    {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1997,1998,2001 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*
*  History:
*    1997 June 20 (TIMJ):
*       Original version
*    1997 July 7 (TIMJ):
*       Make sure that the plot title from the second plot does not get in
*       in the way.
*    1997 July 8 (TIMJ):
*       Add code to create a postscript file as well as using xwindows
*    1998 Dec 8 (TIMJ):
*       Make compatible with KAPPA V0.13
*    2001 October 31 (TIMJ):
*       Remove need for /star/etc/cshrc
*    2011-08-22 (TIMJ):
*       Sdip2 copied from sdip.csh and simplified
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*
*-
