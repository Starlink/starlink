#!/bin/csh 
#
#  Prologue at end since the UNIX SST routines do not understand
#  That hash is a comment character
#

# 1 Argument required: the demodulated data file

# Start up Starlink just in case

source /star/etc/login
source /star/etc/cshrc

# Switch off messages
alias echo "echo > /dev/null"

# First start up kappa

kappa

# Second start up surf

surf

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


# Now display the result with linplot

if (-e ${out}.sdf) then

   # Display the data first
   linplot $out mode=2 symcol=white device=xwindows

   # Now overlay the model 
   linplot ${out}_m mode=line lincol=red device=xwindows noclear ordlab="''" pltitl="''"

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
*    and are then displayed using Kappa's linplot.
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
*    - Requires Kappa.
*    - All files created by this task are removed.
*
*  Related Applications:
*    SURF: SKYDIP;
*    KAPPA: LINPLOT
*
*  Authors:
*    Tim Jenness (JACH)
*    {enter_new_authors_here}
*
*  History:
*    1997 June 20 (TIMJ):
*       Original version
*    1997 July 7 (TIMJ):
*       Make sure that the plot title from the second plot does not get in
*       in the way.
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*
*-
