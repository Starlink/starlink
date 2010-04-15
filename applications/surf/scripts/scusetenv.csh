# Script to set the startup environment variables for SURF
#
#  Usage:   source scusetenv.csh <UTDATE>
#
#  Author: Tim Jenness (t.jenness@jach.hawaii.edu)
#
#  Full Starlink prologue is at the end
#
#  Current UTdate is the default
#
#  This script is designed for use at the JAC (since only at
#  the JAC do I know the location of the data directory)


# Check to see if $1 has been used (ie if an argument has been
# supplied)

if (`echo $1 | egrep '.'`) then
  # Use the supplied argument as UT date
  set utdate = $1
else

  # Check for location of date in two places -- /bin/date (linux)
  # and /usr/bin/date (solaris)
  if (-e '/usr/bin/date' ) then
    set mydate = /usr/bin/date
  else if (-e '/bin/date' ) then
    set mydate = /bin/date
  else
    echo Could not find date in /bin/date or /usr/bin/date
    exit -1
  endif


  # No argument so find the current UT date
  set utdate = `$mydate -u +%Y%m%d`
endif

# Echo setting up SURF for UT date $utdate
echo Setting up SURF for UT date $utdate


# Can set SCUBA_PREFIX
setenv SCUBA_PREFIX $utdate
echo SCUBA_PREFIX set to $utdate

# Now set DATADIR - this depends on our location
# There are 3 possibilities. We are in Hilo, we are at the JCMT
# or we are somewhere else.
#
#
# At the JCMT we need to set DATADIR to /jcmtarchive/UTdate
# In this case the current UT date is the sensible choice
#
# In Hilo we need to set DATADIR to /scuba/Semester/UTdate/
# In this case current UT is meaningless and an argument should be
# used
#
# Somewhere else - we have no idea where DATADIR should be
# so we do nothing

# Use domainname to work out where we are

set dname = `domainname`

if ($dname == 'JAC.jcmt') then
  setenv DATADIR /jcmtdata/raw/scuba/${utdate}/dem
else if ($dname == 'JAC.Hilo') then

  # Hilo is a bit more complicated since now we need to
  # find the semester name

  # Start by splitting the YYYYMMDD string into bits
  set yy = `echo $utdate | cut -c3-4`
  set prev_yy = `expr $utdate - 10000 | cut -c3-4`
  set mmdd = `echo $utdate | cut -c5-8`

  # Need to put the month in the correct semester
  # Note that 199?0201 is in the previous semester
  # Same for 199?0801
  # The semester changes on UT Feb 2 and Aug 2:
  if ( $mmdd > 201 && $mmdd < 802 ) then
    set sem = "m${yy}a"
  else if ( $mmdd < 202 ) then
    set sem = "m${prev_yy}b"
  else
   set sem = "m${yy}b"
  endif

  unset yy
  unset prev_yy
  unset mmdd

  # Now set the directory
  setenv DATADIR /scuba/${sem}/$utdate

  unset sem

else
  echo This routine does not know where data is stored outside of JCMT or the JAC
  echo Not setting DATADIR
endif

# Finally set ORAC_DATA_IN to be the same as DATADIR
# Assuming DATADIR has been set

if ($?DATADIR) then
  setenv ORAC_DATA_IN $DATADIR
  echo DATADIR has been set to $DATADIR
  echo ORAC_DATA_IN has also been set to this value
endif


# clean up
unset mydate
unset utdate
unset dname


exit

*+
*  Name:
*    SCUSETENV
*
*  Purpose:
*    Set the startup environment variables for SURF
*
*  Type of Module:
*    C-shell script
*
*  Usage:
*    scusetenv [UTdate]
*
*  Description:
*    This script sets the standard DATADIR and SCUBA_PREFIX
*    environment variables given a UT date.

*  Parameters:
*    UTdate = YYYYMMDD format string (Optional)
*      The UT date of the data to be processed (in YYYYMMDD format).
*      The default value is to use the current UT date

*  Examples:
*    scusetenv
*      Set DATADIR and SCUBA_PREFIX for the current UT date
*    scusetenv 19980201
*      Set DATADIR and SCUBA_PREFIX for the data observed on date
*      19980201

*  Notes:
*    - Currently this routine only works for the JAC and JCMT systems.
*      This is because the data are stored in standard
*      directories and indexed by YYYYMMDD UT date.
*    - ORAC_DATA_IN environment variable is also set (only relevant
*      for users of the ORAC-DR pipeline)
*    - If this routine is run from a non-JAC/JCMT site DATADIR
*      will not be set but SCUBA_PREFIX will be set.

*  Implementation Status:
*    This script should be sourced (not executed) since the
*    environment variables should be set after the script has been read.

*  Authors:
*    Tim Jenness (JACH)
*    {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*    $Log$
*    Revision 1.8  2002/08/13 06:40:33  timj
*    switch datadir to add dem
*
*    Revision 1.7  2002/08/02 01:19:16  frossie
*    Look at /jcmtdata at the summit
*
*    Revision 1.6  2001/02/01 02:02:28  timj
*    Fix problem with 20010105 becoming semester m0b rather than m00b
*
*    Revision 1.5  2000/02/04 03:16:58  timj
*    Fix 00 problem with semester calculation
*
*    Revision 1.4  1999/08/03 19:32:35  timj
*    Add copyright message to header.
*
*    Revision 1.3  1999/06/19 03:01:25  timj
*    Correct name in example
*
*    Revision 1.2  1999/06/18 03:19:50  timj
*    Documentation patch
*
*    Revision 1.1  1999/01/26 19:32:41  timj
*    New script.
*
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*
*-
