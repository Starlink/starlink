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
#     source ${DATACUBE_DIR}/checkndf.csh [-s script]
#
#  Description:
#     This shell script serves various DATACUBE application scripts.
#     If first checks whether the input NDF has been supplied on the
#     command line, if not it prompts for one.  It then checks whether
#     or not the file exists, and then confirms that the NDF is a cube.
#     If either fails, the script is exited with an error report.
#
#     The name of the file and its vital statistics: dimensionality,
#     shape, bounds, and total number of pixels are reported, unless
#     the -q option is present.
#
#  Parameters:
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
#       Added -q option.  Use DSB's method involving  NDFTRACE to validate
#       the input NDF.
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

set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -q:    # supress reporting
      set quiet = "TRUE"
      shift args
      breaksw
 
   case -s:    # calling script name
      shift args

# Exclude file path and extension.
      set cn_script = `echo $args[1]:r:t | awk '{print toupper($0)"_"}'`
      shift args
      breaksw

   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Obtain details of the input cube.
# =================================

# Get the input filename.
if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
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

# Check that it is a cube.
$KAPPA_DIR/ndftrace ${infile}${ndf_section} >& /dev/null
set cn_ndim = `$KAPPA_DIR/parget ndim ndftrace`
set dims = `$KAPPA_DIR/parget dims ndftrace`
set cn_lbnd = `$KAPPA_DIR/parget lbound ndftrace`
set cn_ubnd = `$KAPPA_DIR/parget ubound ndftrace`

if ( $cn_ndim != 3 ) then
   echo "${cn_script}ERR: ${infile}.sdf is not a datacube."
   exit 1

# Check for a degenerate third axis.
else if ( $dims[3] == 1 ) then
   echo "${cn_script}ERR: ${infile}.sdf${ndf_section} is not a datacube."
   exit 1
endif

set cn_bounds = "${cn_lbnd[1]}:${cn_ubnd[1]}, ${cn_lbnd[2]}:${cn_ubnd[2]}, ${cn_lbnd[3]}:${cn_ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

if ( $quiet == "FALSE" ) then
   echo " "
   echo "      Input NDF:"
   echo "        File: ${infile}.sdf"

   echo "      Shape:"
   echo "        No. of dimensions: ${cn_ndim}"
   echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
   echo "        Pixel bounds     : ${cn_bounds}"
   echo "        Total pixels     : $pixnum"
endif

exit
