#!/bin/csh
#
#  Prologue at end since the UNIX SST routines do not understand
#  That hash is a comment character
#

# 1 Argument required: the demodulated data file

# Check if starlinkg was initialised

if !($?STAR_LOGIN) then

# Start up Starlink (if possible)
# note that should probably put in something that gets
# updated during INSTALLation.

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

set out = "sdip"

# Run skydip

if ($#argv == 0) then
   skydip  out=$out model_out=${out}_m
else
   set in = $1
   skydip out=$out model_out=${out}_m in=$in
endif

# Work out whether we are using the new (STYLE parameter) KAPPA
# or the old (pre 0.13 is old)

if (-e $KAPPA_DIR/style.def || -e $KAPPA_DIR/kappa_style.def) then
  set newkappa = 1
else
  set newkappa = 0
endif

# Get current value of LINCOL and SYMCOL from LINPLOT so that
# I can reset them at the end -only for old KAPPA (pre 0.13)

if ($newkappa == 0) then
  set old_lincol = `parget lincol linplot`
  set old_symcol = `parget symcol linplot`
else
  set old_style  = `parget style linplot`
endif

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
    if ($newkappa == 0) then
      set kapargs = "cosys=data mode=2 symcol=black"
    else
      wcsframe $out axis
      set kapargs = "mode=mark marker=2 style='Colour(Symbols)=black,colour(numlab)=black,colour(border)=black,Colour(ticks)=black'"
    endif

    linplot $out $kapargs device='epsfcol_l;sdip_p1.eps'


    # If we have a fit then plot the model
    if (-e ${out}_m.sdf) then

        if ($newkappa == 0) then
          set kapargs = 'cosys=data lincol=red pltitl="" ordlab=""'
        else
          wcsframe ${out}_m axis
	  set kapargs = "style='Colour(Lines)=red'"
        endif

	linplot ${out}_m mode=line noclear $kapargs device='epsfcol_l;sdip_p2.eps'

	# Remove the old output file so that we can tell if we really
	# created one here
        if (-e ${out}.eps) \rm ${out}.eps

        # And merge them - for the old KAPPA we want sdip_p1.eps.1
	# for the new KAPPA we can use sdip_p1.eps
	if ( -x ${starbin}/psmerge) then
	  if ($newkappa == 0) then
            if (-e sdip_p1.eps.1 && -e sdip_p2.eps) ${starbin}/psmerge -e sdip_p1.eps.1 sdip_p2.eps >! ${out}.eps
	  else
	    if (-e sdip_p1.eps && -e sdip_p2.eps) ${starbin}/psmerge -e sdip_p1.eps sdip_p2.eps >! ${out}.eps
          endif
        else
          echo Unable to locate Starlink psmerge command in directory ${starbin}.
        endif

   else
        # If we don't have a fit then just cp the sky temps to the output
        if (-e sdip_p1.eps.1) cp sdip_p1.eps.1 ${out}.eps

   endif

   # Tell everyone where to look
   if (-e ${out}.eps) echo Hardcopy written to ${out}.eps

   # And now remove the intermediate files

   if (-e sdip_p2.eps) \rm sdip_p2.eps*
   if (-e sdip_p1.eps) \rm sdip_p1.eps*


   #  XWINDOWS

   if ($newkappa == 0) then
     set kapargs = "cosys=data mode=2 symcol=green"
   else
     set kapargs = "mode=mark marker=2 style='Colour(Symbols)=green'"
   endif


   # Display the data first
   linplot $out $kapargs device=xwindows

   # Now overlay the model if necessary

   if (-e ${out}_m.sdf) then

     if ($newkappa == 0) then
       set kapargs = 'cosys=data lincol=red ordlab="" pltitl="" '
     else
       set kapargs = "style='Colour(Lines)=red'"
     endif
     linplot ${out}_m mode=line $kapargs device=xwindows noclear
   endif

endif

# Reset linplot parameters
if ($newkappa == 0) then
  linplot device=! ndf=! lincol=$old_lincol symcol=$old_symcol > /dev/null
else
  linplot device=! ndf=! style=$old_style > /dev/null
endif

# Remove the intermediate files

if (-e ${out}.sdf) \rm ${out}.sdf
if (-e ${out}_m.sdf) \rm ${out}_m.sdf


exit


# Prologue goes here for SST

*+
*  Name:
*    SDIP
*
*  Purpose:
*    Reduces and displays skydip data
*
*  Type of Module:
*    C-shell script
*
*  Usage:
*    sdip [NDF]
*
*  Description:
*    This script first runs the skydip task in order to fit the sky
*    parameters to the data. The sky data and model are written to files
*    and are then displayed using KAPPA's linplot. A hardcopy is also
*    written to file sdip.eps
*
*  ADAM Parameters:
*    NDF = NDF (Read)
*       Name of raw data file. Can be located in $DATADIR. The filename
*       will be requested if not specified on the command line.
*
*  Examples:
*    sdip 19970623_dem_0008
*       Reduce the skydip data in 19970623_dem_008.sdf and plots the result.
*
*  Implementation Status:
*    - Requires KAPPA.
*    - All files created by this task are removed.
*
*  Related Applications:
*    SURF: SKYDIP;
*    KAPPA: LINPLOT
*
*  Authors:
*    Tim Jenness (JACH)
*    {enter_new_authors_here}

*  Copyright:
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
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*
*-
