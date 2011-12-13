      SUBROUTINE TEST_MTOE(STATUS)
*+
*  Name:
*     TEST_MTOE

*  Purpose:
*     Test CHR_MTOE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_MTOE(STATUS)

*  Description:
*     Test CHR_MTOE.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     12-APR-1994 (ACC)
*        Superfluous variables removed.
*      3-DEC-2001 (AJC):
*        Allow for ICHAR returning negatives
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_MTOE

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
      INTEGER A_INVALID
      PARAMETER (A_INVALID = 132)
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

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER ICHR               ! INTEGER value
      INTEGER ICHR1              ! INTEGER value
      INTEGER ICHR2              ! INTEGER value
      INTEGER ICHR3              ! INTEGER value
      CHARACTER*1 CHAR1          ! Strings
      CHARACTER*10 CHARS

*.

      NUL = CHAR(0)

*    Test CHR_MTOE

      ISTAT = SAI__OK

      CALL CHR_MTOE( ' ', CHAR1 )
      ICHR = ICHAR( CHAR1 )
      IF ( ICHR .NE. E_SPACE ) THEN
         PRINT *,
     :   'CHR_MTOE FAILS - numeric value returned for blank:', ICHR
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_MTOE( CHAR(A_INVALID), CHAR1 )
      ICHR = ICHAR( CHAR1 )
      IF ( ICHR .NE. E_SPACE ) THEN
         PRINT *,
     :   'CHR_MTOE FAILS - numeric value returned for invalid value:',
     :   ICHR
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_MTOE( NUL, CHAR1 )
      ICHR = ICHAR( CHAR1 )
      IF ( ICHR .NE. E_NUL ) THEN
         PRINT *,
     :   'CHR_MTOE FAILS - numeric value returned for null:', ICHR
         ISTAT = SAI__ERROR
      END IF

      CALL CHR_MTOE( 'ABC', CHARS )
      ICHR1 = ICHAR( CHARS(1:1) )
      IF ( ICHR1 .LT. 0 ) ICHR1 = ICHR1 + 256
      ICHR2 = ICHAR( CHARS(2:2) )
      IF ( ICHR2 .LT. 0 ) ICHR2 = ICHR2 + 256
      ICHR3 = ICHAR( CHARS(3:3) )
      IF ( ICHR2 .LT. 0 ) ICHR2 = ICHR2 + 256
      IF ( ICHR1 .NE. E_A .AND.
     :     ICHR2 .NE. E_B .AND.
     :     ICHR3 .NE. E_C ) THEN
         PRINT *,
     :   'CHR_MTOE FAILS - incorrect value returned for ''ABC'':',
     :   ICHR1, ICHR2, ICHR3
         ISTAT = SAI__ERROR
      END IF

      IF (ISTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_MTOE OK'
      ELSE
         STATUS = SAI__ERROR
      END IF

      END
