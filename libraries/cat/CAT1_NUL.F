#if HAVE_CONFIG_H
#include "config.h"
#endif
*+
*  Name:
*     CAT1_NUL
*  Purpose:
*     Default null values for StarBase.
*  Language:
*     FORTRAN 77.
*  Type of Module:
*     INCLUDE FILE.
*  Description:
*     An include file of parametric constants to hold the default
*     null values used by StarBase.  These default null values are
*     deliberately identical to the Starlink null values, in terms
*     of which they are defined.
*
*     See SUN/39 for a description of the Starlink null values.
*
*     See also StarBase/ACD/3.4, p25, Table 4.2 for further discussion
*     of the StarBase null values.
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council
*     Copyright (C) 2020 East Asian Observatory
*     All Rights Reserved.

*  Authors:
*     ACD: A C Davenhall (Leicester)
*     PWD: Peter W. Draper (JAC, Durham University)
*     DSB: David S Berry (EAO)
*  History:
*     27/1/94  (ACD): Original version.
*     11/4/95  (ACD): Changed the name given in the 'Name:' section of
*        the prologue to reflect Unix rather than VMS conventions.
*     25/11/96 (ACD): Incorporated a portable and standard declaration
*        for CAT1__DNULC from the Linux port of CAT.
*     28-FEB-2007 (PWD):
*        Make a BOZ assignments when there's no typeless support.
*        Better than nothing.
*     27-JUL-2020 (DSB):
*        Remove support for NULL logical values, since there seems to be
*        no portable way to provide a distinct third value for a Fortran
*        LOGICAL.
*  Global Constants:
C      INCLUDE 'PRM_PAR'
*-

*
*    Null values for the numeric data types.  These parameters are
*    defined in terms of the equivalent Starlink null value.

      BYTE CAT1__DNULUB             ! Unsigned byte.
      PARAMETER (CAT1__DNULUB = VAL__BADUB)

      BYTE CAT1__DNULB              ! Byte.
      PARAMETER (CAT1__DNULB = VAL__BADB)

      INTEGER*2 CAT1__DNULUW        ! Unsigned word.
      PARAMETER (CAT1__DNULUW = VAL__BADUW)

      INTEGER*2 CAT1__DNULW         ! Word.
      PARAMETER (CAT1__DNULW = VAL__BADW)

      INTEGER CAT1__DNULI           ! Integer.
      PARAMETER (CAT1__DNULI = VAL__BADI)

      INTEGER*8 CAT1__DNULK         ! 64-bit Integer.
      PARAMETER (CAT1__DNULK = VAL__BADK)

      REAL CAT1__DNULR              ! Real.
      PARAMETER (CAT1__DNULR = VAL__BADR)

      DOUBLE PRECISION CAT1__DNULD  ! Double precision.
      PARAMETER (CAT1__DNULD = VAL__BADD)

*
*    Null character value. The ASCII standard defines a byte with a value
*    of 0 (that is, all its bits set to zero) as null.  This value is
*    adopted as the default for data type CHARACTER.

      CHARACTER CAT1__DNULC*1
*     PARAMETER (CAT1__DNULC = CHAR(0) )
      PARAMETER (CAT1__DNULC = '\000' )
