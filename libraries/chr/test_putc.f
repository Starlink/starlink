      SUBROUTINE TEST_PUTC(STATUS)
*+
*  Name:
*     TEST_PUTC

*  Purpose:
*     Test CHR_PUTC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_PUTC(STATUS)

*  Description:
*     Test CHR_PUTC.
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
*     01-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_PUTC

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
      INTEGER PTR1               ! String indexes
      CHARACTER*10 STARS

*.

*    Test CHR_PUTC -- Character to character at given position

      PTR1 = 5
      STARS = '*****'
      CALL CHR_PUTC ('*****', STARS, PTR1)
      IF ((STARS .EQ. '**********') .AND. (PTR1 .EQ. 10)) THEN
         PRINT *, 'CHR_PUTC OK'
      ELSE
         PRINT *, 'CHR_PUTC FAILS - STRING is:',STARS
         STATUS = SAI__ERROR
      ENDIF

      END
