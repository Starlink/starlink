#!/bin/csh
#+
#  Name:
#     squash.csh
#
#  Purpose:
#     Extracts a two-dimensional white-light image from a three-dimensional
#     IFU NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     squash  [-i filename] [-l number] [-o filename] [-p] [-u number]
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and allows
#     you to extract a specific spectral range from the cube to form a
#     white-light image.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF, by default the script will prompt for the
#       input file.
#     -l number
#       Lower spectral-axis bound of the region of interest.
#     -o filename
#       The filename for the output white-light or passband image.  By default
#       the script will prompt for the name of the output file.
#     -p
#       The script will plot the extracted image to the current display
#       as well as saving it to an NDF file.  [FALSE]
#     -u number
#       Upper spectral-axis bound of the region of interest.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
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
#       Modified to use PUTAXIS A-Task
#     20-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     23-NOV-2000 (AALLAN):
#       Added -i, -o, -l and -u command line options.
#       Added on interrupt handler.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September  1 (MJC):
#       Replaced GETBOUND with NDFTRACE, accessing frame-bound
#       parameters FLBND/FUBND.
#     2005 September  2 (MJC):
#       Replaced PUTAXIS with KAPPA:SETAXIS in WCS mode.  Some tidying:
#       remove tabs, spelling & punctuation corrections, sorted parameters in
#       alphabetical order.  Added section headings in the code.  Replace
#       explicit wavelength in prompts with the current WCS Frame's label for
#       the spectral axis, and also used the corresponding units in the
#       output commentary.
#     2005 November 3 (MJC):
#       Add options waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script that
#       also checks for a degenerate third axis.
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
rm -f ${tmpdir}/${user}/squash* >& /dev/null

# Do variable initialisation.
set pltimg = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotupper = "FALSE"
set gotlower = "FALSE"

mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/squash_col"

# Setup the plot device.
set plotdev = "xwin"

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input three-dimensional IFU NDF file
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -l:    # lower spectral-axis bound
      shift args
      set gotlower = "TRUE"
      set lower = $args[1]
      shift args
      breaksw
   case -o:    # output white-light file
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output image?
      set pltimg = "TRUE"
      shift args
      breaksw
   case -u:    # upper spectral-axis bound
      shift args
      set gotupper = "TRUE"
      set upper = $args[1]
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Do package setup.
alias echo "echo > /dev/null"
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s squash
if ( $status == 1 ) exit

# Get the spectral range.
set wlbnd = `parget flbnd ndftrace`
set wubnd = `parget fubnd ndftrace`

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# Inform the user.
echo "        ${slabel} bounds : ${wlbnd[3]}:${wubnd[3]} ${sunits}"

# Get the upper and lower bounds and chunk size.
if ( ${gotlower} == "FALSE" ) then
   echo -n "Lower ${slabel}-axis bound: "
   set lower = $<
endif

if ( ${gotupper} == "FALSE" ) then
   echo -n "Upper ${slabel}-axis bound: "
   set upper = $<
endif

echo " "
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
echo "        ${slabel} range : ${lower}--${upper} ${sunits}"
echo " "

# Form and display the collapsed image.
# =====================================

# Collapse the white-light image.
collapse "in=${infile}${ndf_section} out=${colfile} " \
         "axis=3 low=${lower} high=${upper}" >& /dev/null
settitle "ndf=${colfile} title='${lower} - ${upper}'"

# Display the collapsed image, if required.
if ( ${pltimg} == "TRUE" ) then
   gdclear device=${plotdev}
   paldef device=${plotdev}
   lutgrey device=${plotdev}
   display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" reset >& /dev/null
endif

# Create the output NDF.
# ======================

# Get the output filename, stripping any file extension.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
   set outfile =  ${outfile:r}
   echo " "
endif

# Output the chunk.
echo "      Output NDF:"
echo "        File: ${outfile}.sdf"
ndfcopy "in=${colfile} out=${outfile}"

# Test for an AXIS structure.  If one does not exist, create an array of
# axis centres, derived from the current WCS Frame, along each axis.
set axis = `parget axis ndftrace`
if ( ${axis} == "FALSE" ) then
   setaxis "ndf=${outfile} dim=1 mode=wcs comp=Centre" >& /dev/null
   setaxis "ndf=${outfile} dim=2 mode=wcs comp=Centre" >& /dev/null
   echo "        Axes: Adding AXIS centres."
endif

# Clean up.
# =========
cleanup:

rm -f ${colfile}.sdf >& /dev/null
rmdir ${tmpdir}/${user}  >& /dev/null
