      SUBROUTINE TEST_LASTO(STATUS)
*+
*  Name:
*     TEST_LASTO

*  Purpose:
*     Test CHR_LASTO.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_LASTO(STATUS)

*  Description:
*     Test CHR_LASTO.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     14-SEP-1993 (ACC):
*        Modularised version: broken into one routine for each of 5 main
*        categories of tests.
*     02-MAR-1994 (ACC):
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     23-DEC-2005 (TIMJ):
*        Copy from TEST_FANDL for CHR_FPARX
*     27-DEC-2005 (TIMJ):
*        Copy from TEST_FPARX for CHR_LASTO
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_LASTO

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

*  Local Variables:
      INTEGER L                  ! Location
      CHARACTER*120 STRING

*.

*    Test CHR_LASTO

*    First a string without a match
      CALL CHR_LASTO( 'Hello', 'X', L )
      IF ( L .NE. 0 ) THEN
         PRINT *, 'CHR_LASTO FAILS - Found character where no match', L
         STATUS = SAI__ERROR
      END IF

*    Now a string with a match
      STRING = 'Hello X and X<--'
      CALL CHR_LASTO( STRING, 'X', L )
      IF ( L .EQ. 13) THEN
         PRINT *, 'CHR_LASTO OK'
      ELSE
         PRINT *, 'CHR_LASTO FAILS - STRING is:',STRING,
     :        ' Positions is', L
         STATUS = SAI__ERROR
      ENDIF

      END
