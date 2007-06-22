#!/bin/csh -f

#+
# file:        CGS3DR_DIR/cgs3dr
# type:        C-shell script
# author:      Phil Daly
# description: Invokes the Portable-CGS3DR command line software
# history:
#   13-Dec-95: Original version (pnd@jach.hawaii.edu)
#   27-Feb-96: change ref of cgs3_dr to cgs3dr
# endhistory
# parameters: $1 is a directory /$HOME/
#             $2 is a UT-date   /UT=today/
#-

# If environmental variables are not set, set them
if ( ${?CGS3DR_DIR} == 0 || ${?RED3_DIR} == 0 || ${?FIG_DIR} == 0 ) then
  setenv CGS3DR_DIR /star/bin/cgs3dr
  setenv RED3_DIR   /star/bin/cgs3dr
  setenv FIG_DIR    /star/bin/figaro
endif

# Set some default values
set defdir = "${HOME}"
set defutdate = "`date -u +%Y%m%d`"

# If not arguments supplied, get them all
if ( ${#argv} == 0 ) then
  # Get data directory
  echo -n "Path to data directory?  <${defdir}> > "
  set datadir = $<
  if ( "${datadir}" == "" ) set datadir = "${defdir}"
  # Get the UT date
  echo -n "UT date?  <${defutdate}> > "
  set utdate = $<
  if ( "${utdate}" == "" ) set utdate = "${defutdate}"

else if ( ${#argv} == 1 ) then
  # Get data directory
  set datadir = "${argv[1]}"
  # Get the UT date
  echo -n "UT date?  /${defutdate}/ > "
  set utdate = $<
  if ( "${utdate}" == "" ) set utdate = "${defutdate}"

else if ( ${#argv} == 2 ) then
  # Get data directory
  set datadir = "${argv[1]}"
  # Get the UT date
  set utdate = "${argv[2]}"
endif

# Check the data directory is readable
if ( ! -r "${datadir}" ) then
  echo "${0}-f-norread, ${datadir} is not readable from `uname -n`\!"
  exit 1
endif

# Check the UT date is of the form YYMMDD or YYYYMMDD
if ! ( "${utdate}" =~ [0-9][0-9][0-9][0-9][0-9][0-9] ) then
  if ! ( "${utdate}" =~ [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9] ) then
    echo "${0}-f-invutdate, invalid UT date\!"
    exit 1
  endif
endif

# Set the CGS3 environmental variables
setenv TODAY2  ${utdate}
setenv DATADIR ${datadir}

# Check directories exist
if ( ! -e ${HOME}/adam )     mkdir ${HOME}/adam
if ( ! -e ${datadir}/rodir ) mkdir ${datadir}/rodir

# Print out a welcome message
echo ""
echo "      Welcome to Portable-CGS3DR VPKG_VERS"
echo ""

# Start icl and invoke cgs4dr
icl ${CGS3DR_DIR}/cgs3dr.icl
