#!/bin/csh
#+
#  Name:
#     checkndf.csh
#
#  Purpose:
#     Obtains and validates the NDF and reports its vital statistics.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     source ${DATACUBE_DIR}/checkndf.csh [-d dimensionality] [-q]
#            [-p prompt_string] [-s script]
#
#  Description:
#     This shell script serves various DATACUBE application scripts.
#     If first checks whether the input NDF has been supplied on the
#     command line, if not it prompts for one.  It then checks whether
#     or not the file exists, and then confirms that the NDF has the
#     desired dimensionality (defaulting to a cube).  If either test
#     fails, the script is exited with an error report.
#
#     The name of the file and its vital statistics: dimensionality,
#     shape, bounds, and total number of pixels are reported, unless
#     the -q option is present.
#
#  Parameters:
#     -d dimensionality
#       The required dimensionality.  Allowed values are 2 or 3.  [3]
#     -p prompt_string
#       The string to prompt for the file name.  ["NDF input file"]
#     -q
#       If present it disables the reporting the file name and its
#       statistics.
#     -s script
#       The name of the calling script.  This name in uppercase without
#       any path or file extension is used in error reports.  [""]
#
#  Shell Variables:
#     $gotinfile = CHARACTER (Given)
#        If set to "FALSE" the NDF has not been supplied on the command
#        line.
#     $dims[] = INTEGER (Returned)
#        The NDF's dimensions.
#     $infile = CHARACTER (Returned)
#        The name of the NDF with any .sdf and any supplied section
#        removed.
#     $ndf_section = CHARACTER (Returned)
#        The supplied bounds string including the bounding parentheses.
#        If no NDF section was specified, this is a null string.
#
#  Notes:
#      -  It invokes KAPPA:NDFTRACE on the supplied NDF, so output
#      parameters may be accessed, but any previous NDFTRACE information
#      is lost.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2006 March 9 (MJC):
#       Original version.
#     2010 July 13 (MJC):
#       Added -q option.  Use DSB's method involving NDFTRACE to validate
#       the input NDF.
#     2010 September 7 (MJC):
#       Added -d and -p options.
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2006 Central Laboratory of the Research Councils
#     Copyright (C) 2010 Science and Technology Facilities Council.
#     All Rights Reserved.
#-

# Preliminaries
# =============

# Get command-line options.
set cn_script = ""
set quiet = "FALSE"
set dimens = 3
set file_prompt = "NDF input file"

# Specify the options and whether they expect an argument.  The
# parentheses are needed because the result of getopt is a list.
# The q modifier prevents any subsitutions in the command-line list
# in $argv.
set opt = ( `getopt -s csh -o d:p:qs: -- $argv:q` )
if ($? != 0 ) then
   echo "Problem with getopt parsing options."
   exit 1
endif

eval set argv = \( $opt:q \)
while ( 1 )
   switch ( $argv[1]:q )

   case -d:    # dimensionality
      shift argv
      set dimens = $argv[1]:q
      if ( $dimens != 2 && $dimens != 3 ) then
         set dimens = 3
      endif
      shift argv
      breaksw

   case -p     # prompt string
      shift argv
      set file_prompt = $argv[1]:q
      shift argv
      breaksw

   case -q:    # suppress reporting
      set quiet = "TRUE"
      shift argv
      breaksw

   case -s:    # calling script name

# Exclude file path and extension.
      shift argv
      set cn_script = `echo $argv[1]:r:t:q | awk '{print toupper($0)"_"}'`
      shift argv
      breaksw

   case --:
      shift argv
      break

   case *:     # rubbish disposal
      shift argv
      breaksw
   endsw
end

# Obtain details of the input cube.
# =================================

# Get the input filename.
if ( ${gotinfile} == "FALSE" ) then
   echo -n "${file_prompt}: "
   set infile = $<
endif

# Obtain the name sans any section.
set cn_name = `echo $infile | \
              awk '{if (index($0,"(") > 0) print substr($0,1,index($0,"(")-1); else print $0}'`

# Obtain the section, if present.
set ndf_section = `echo $infile | \
      awk '{if (index($0,"(") > 0) print substr($0,index($0,"(")); else print ""}'`

# This must occur after stripping the section, as the section
# specification must come after the file extension in file names
# presented to the NDF library (cf. SSN/20).  Also this step is
# necessary when the version of KAPPA does not support the .sdf file
# extension being supplied in the name.
set infile = ${cn_name:r}

# Check the supplied NDF exists (it may be an NDF inside another NDF so
# we cannot just check that the .sdf file exists).
$KAPPA_DIR/ndftrace ${infile} | grep \!\! > /dev/null
if ( $status == 0 ) then
   echo "${cn_script}ERR: ${infile} does not exist."
   exit 1
endif

# Check the dimensionality matches the requirement.
$KAPPA_DIR/ndftrace ${infile}${ndf_section} >& /dev/null
set cn_ndim = `$KAPPA_DIR/parget ndim ndftrace`
set dims = `$KAPPA_DIR/parget dims ndftrace`
set cn_lbnd = `$KAPPA_DIR/parget lbound ndftrace`
set cn_ubnd = `$KAPPA_DIR/parget ubound ndftrace`

if ( $cn_ndim != $dimens ) then
   if ( $dimens == 3 ) then
      echo "${cn_script}ERR: ${infile}.sdf is not a datacube."
   else
      echo "${cn_script}ERR: ${infile}.sdf is not an image."
   endif
   exit 1

# Check for a degenerate final axis.
else if ( $dims[$dimens] == 1 ) then
   if ( $dimens == 3 ) then
      echo "${cn_script}ERR: ${infile}.sdf${ndf_section} is not a datacube."
   else
      echo "${cn_script}ERR: ${infile}.sdf${ndf_section} is not an image."
   endif
   exit 1
endif

if ( $dimens == 3 ) then
   set cn_bounds = "${cn_lbnd[1]}:${cn_ubnd[1]}, ${cn_lbnd[2]}:${cn_ubnd[2]}, ${cn_lbnd[3]}:${cn_ubnd[3]}"
   @ pixnum = $dims[1] * $dims[2] * $dims[3]
else
   set cn_bounds = "${cn_lbnd[1]}:${cn_ubnd[1]}, ${cn_lbnd[2]}:${cn_ubnd[2]}"
   @ pixnum = $dims[1] * $dims[2]
endif

if ( $quiet == "FALSE" ) then
   echo " "
   echo "      Input NDF:"
   echo "        File: ${infile}.sdf"

   echo "      Shape:"
   echo "        No. of dimensions: ${cn_ndim}"
   if ( $dimens == 3 ) then
      echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
   else
      echo "        Dimension size(s): ${dims[1]} x ${dims[2]}"
   endif
   echo "        Pixel bounds     : ${cn_bounds}"
   echo "        Total pixels     : $pixnum"
endif

exit
