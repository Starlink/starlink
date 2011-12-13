      SUBROUTINE TEST_ETOM(STATUS)
*+
*  Name:
*     TEST_ETOM

*  Purpose:
*     Test CHR_ETOM.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ETOM(STATUS)

*  Description:
*     Test CHR_ETOM.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
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
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     14-SEP-1993 (ACC)
*        Modularised version: broken into one routine for each of 5 main
*        categories of tests.
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_ETOM

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
*     None

*  Arguments Returned:
      INTEGER STATUS

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  Local Constants:
      CHARACTER*1 NUL            ! Null character
      INTEGER E_NUL
      PARAMETER (E_NUL = 0)
      INTEGER E_SPACE
      PARAMETER (E_SPACE = 64)
      INTEGER E_A
      PARAMETER (E_A = 193)
      INTEGER E_B
      PARAMETER (E_B = 194)
      INTEGER E_C
      PARAMETER (E_C = 195)
      INTEGER E_INVALID
      PARAMETER (E_INVALID = 48)

*  Local Variables:
      INTEGER ISTAT              ! Local status
      CHARACTER*1 CHAR1          ! Strings
      CHARACTER*10 CHARS

*.

      NUL = CHAR(0)

*    Test CHR_ETOM

      ISTAT = SAI__OK

      CALL CHR_ETOM( CHAR(E_SPACE), CHAR1 )
      IF ( CHAR1 .NE. ' ' ) THEN
         PRINT *,
     :   'CHR_ETOM FAILS - ASCII value returned for blank:', CHAR1
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_ETOM( CHAR(E_INVALID), CHAR1 )
      IF ( CHAR1 .NE. ' ' ) THEN
         PRINT *,
     :   'CHR_ETOM FAILS - ASCII value returned for invalid value:',
     :   ICHAR(CHAR1)
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_ETOM( CHAR(E_NUL), CHAR1 )
      IF ( CHAR1 .NE. NUL ) THEN
         PRINT *,
     :   'CHR_ETOM FAILS - ASCII value returned for null:', CHAR1
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_ETOM( CHAR(E_A)//CHAR(E_B)//CHAR(E_C), CHARS )
      IF ( CHARS(1:3) .NE. 'ABC' ) THEN
         PRINT *, 'CHR_ETOM FAILS - ASCII value returned for ''ABC'':',
     :   CHARS(1:3)
         ISTAT = SAI__ERROR
      END IF

      IF (ISTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ETOM OK'
      ELSE
         STATUS = SAI__ERROR
      END IF

      END
