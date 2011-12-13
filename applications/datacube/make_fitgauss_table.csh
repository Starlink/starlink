#+
#  Name:
#     make_fitgauss_table.csh
#
#  Purpose:
#     Creates an STL file for recording spectral-cube Gaussian-fitting results.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     make_fitgauss_table.csh [-c units] [-f filename] -l logfile [-v units]
#
# Description:
#    This primitive creates a Starlink small text-file (STL) catalogue.
#    It contains the scxhema for the tabulated data made by FITGAUSS
#    invocations in the DATACUBE package, such as in scripts peakmap and
#    velmap.  In addition to the regular headers there is a formatted
#    comment line immediately before the BEGINTABLE presenting the column
#    headings.  The co-ordinate and value units, if supplied, are recorded
#    in the header in each relevant field.  The name of the cube being
#    analysed, if supplied, is recorded as a parameter of the table.
#
# Notes:
#    -  The format for output values is
#    %6i %6i %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g
#    representing the followingf parameters; X abnd Y pixel indices,
#    centre and its error, peak height and its error, FWHM and its error,
#    and the intgral flux and its error respectively.
#
#  Parameters:
#     -c units
#       The units of the spectral co-ordinates used for the centre and FWHM
#       fields, and  their respective errors.  If not supplied, the UNITS
#       field of the schema will be not written for these fields.
#     -f filename
#       The name of the NDF being analysed.
#     -l filename
#       The name of the log file to contain the table of fit parameters.
#     -v units
#       The data value units used for the peak height and flux fields, and
#       their respective errors.  If not supplied, the UNITS field of the
#       table schema will be not written for these fields.
#
# Copyright:
#    Copyright (C) 2008 Science and Technology Facilities Council.
#    All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either Version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#     02110-1301, USA.
#
# Authors:
#    MJC: Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}
#
#  History:
#     2008 June 24 (MJC):
#       Original version.
#     {enter_further_changes_here}
#
#-

# Set options defaults.  Use different name for the logfile variable.
set mft_cube = " "
set mft_cunits = " "
set mft_lf = " "
set mft_vunits = " "

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -c:    # co-ordinate units
      shift args
      set mft_cunits = $args[1]
      shift args
      breaksw
   case -f:    # input NDF
      shift args
      set mft_cube = $args[1]
      shift args
      breaksw
   case -l:    # output log file
      shift args
      set mft_lf = $args[1]
      shift args
      breaksw
   case -v:    # value units
      shift args
      set mft_vunits = $args[1]
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

if ( "${mft_lf}" == " " ) then
   echo "make_fitgauss_table.csh: Programming error.  Must supply a logfile with -l option."
   exit
endif

echo "\!+" >> ${mft_lf}
echo "\!  Simple STL file: Spectral cube Gaussian-fitting results" >> ${mft_lf}
echo "\!" >> ${mft_lf}
echo "\!  Created by the make_fitgauss_table.csh script in DATACUBE." >> ${mft_lf}
echo "\!-" >> ${mft_lf}
echo " " >> ${mft_lf}
echo "C XINDEX   INTEGER  1 EXFMT=I6    TBLFMT=I6     COMMENTS='X spatial pixel index'" >> ${mft_lf}
echo "C YINDEX   INTEGER  8 EXFMT=I6    TBLFMT=I6     COMMENTS='Y spatial pixel index'" >> ${mft_lf}
echo "C CENTRE   REAL    15 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Gaussian centre'" >> ${mft_lf}
if ( "${mft_cunits}" != " " ) echo ":          UNITS='${mft_cunits}'" >> ${mft_lf}
echo "C CEN_ERR  REAL    28 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Error in Gaussian centre'" >> ${mft_lf}
if ( "${mft_cunits}" != " " ) echo ":          UNITS='${mft_cunits}'" >> ${mft_lf}
echo "C PEAK     REAL    41 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Gaussian peak height'" >> ${mft_lf}
if ( "${mft_vunits}" != " " ) echo ":          UNITS='${mft_vunits}'" >> ${mft_lf}
echo "C PEAK_ERR REAL    54 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Error in Gaussian peak height'" >> ${mft_lf}
if ( "${mft_vunits}" != " " ) echo ":          UNITS='${mft_vunits}'" >> ${mft_lf}
echo "C FWHM     REAL    67 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Gaussian full-width half-maximum'" >> ${mft_lf}
if ( "${mft_cunits}" != " " ) echo ":          UNITS='${mft_cunits}'" >> ${mft_lf}
echo "C FWHM_ERR REAL    80 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Error in Gaussian full-width half-maximum'" >> ${mft_lf}
if ( "${mft_cunits}" != " " ) echo ":          UNITS='${mft_cunits}'" >> ${mft_lf}
echo "C INTEGRAL REAL    93 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Gaussian integral flux'" >> ${mft_lf}
if ( "${mft_vunits}" != " " ) echo ":          UNITS='${mft_vunits}'" >> ${mft_lf}
echo "C INTE_ERR REAL   106 EXFMT=G12.5 TBLFMT=G12.5  COMMENTS='Error in Gaussian integral flux'" >> ${mft_lf}
if ( "${mft_vunits}" != " " ) echo ":          UNITS='${mft_vunits}'" >> ${mft_lf}
if ( "${mft_cube}" != " " ) echo "P FILE     CHAR*117   '${mft_cube}'"  >> ${mft_lf}
echo " " >> ${mft_lf}
echo "D POSITION='CHARACTER'" >> ${mft_lf}
echo " " >> ${mft_lf}
echo "#   x      y      centre      cent_err        peak      peak_err       fwhm       fwhm_err     integral     inte_err" >> ${mft_lf}
echo "BEGINTABLE" >> ${mft_lf}

exit