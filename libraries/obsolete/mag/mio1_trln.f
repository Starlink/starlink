
      SUBROUTINE MIO1_TRLN(IN, OUT)
*+
*  Name:
*     MIO1_TRLN

*  Purpose:
*     Translate environment variable.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO1_TRLN(IN, OUT)

*  Description:
*     The input Name is translated recursively until it produces an
*     absolute reference.

*  Arguments:
*     IN=CHARACTER*(*) (Given)
*        The input Name.
*     OUT=CHARACTER*(*) (Returned)
*        The returned translated name.

*  Algorithm:
*     Check to see if the device name is of the form /dev/... ;
*     if not then use the PSX_GETENV routine to try to translate it.

*  Copyright:
*     Copyright (C) 1980, 1983, 1991, 1992, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

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

*  Authors:
*     Sid Wright (ZUVAD::SLW)
*     Jack Giddings (ZUVAD::JRG)
*     {enter_new_authors_here}

*  History:
*     12-APR-1980:  Original.  (ZUVAD::SLW)
*     01-FEB-1983:  Fortran 77 Version. (ZUVAD::JRG)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     15-Jan-1992:  re-written for Unix (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MIO_$TRLN.
*
*     This is the Unix version.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) IN          ! Input logical name

*  Arguments Returned:
      CHARACTER*(*) OUT         ! Output reference

*  External References:
      EXTERNAL PSX_GETENV       ! Get eenvironment variable string
      INTEGER CHR_LEN           ! String length

*  Local Constants:
      INTEGER MAXLOG            ! Maximum size of logical name
      PARAMETER (MAXLOG=64)

*  Local Variables:
      CHARACTER*(MAXLOG) TEMP   ! Temporary name
      INTEGER STATUS            ! PSX status return
      INTEGER NI                ! Length of input/temporary name

*.

      TEMP = IN
      CALL CHR_LCASE(TEMP)
      NI = CHR_LEN(TEMP)

*    If it is a sensible device name copy to output

      IF ( TEMP(1:5).EQ.'/dev/' ) THEN
         OUT = TEMP(1:NI)

      ELSE

*      Otherwise try to transalate it as an environment variable.

         CALL PSX_GETENV(TEMP, OUT, STATUS)

*      If this fails then copy input to output

         IF ( STATUS.NE.SAI__OK ) OUT = TEMP(1:NI)

      END IF

*    Clean it
      CALL CHR_CLEAN(OUT)
      CALL CHR_RMBLK(OUT)

      END
