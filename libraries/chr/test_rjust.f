      SUBROUTINE TEST_RJUST(STATUS)
*+
*  Name:
*     TEST_RJUST

*  Purpose:
*     Test CHR_RJUST.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_RJUST(STATUS)

*  Description:
*     Test CHR_RJUST.
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
*     CHR_RJUST

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
      CHARACTER*30 LINE
      CHARACTER*120 MARY
      CHARACTER*30 JUSLIN

      DATA MARY(1:24)/'Mary had a little lamb. '/
      DATA MARY(25:55)/'It''s fleece was white as snow. '/
      DATA MARY(56:85)/'And everywhere that Mary went '/
      DATA MARY(86:109)/'the lamb was sure to go.'/
      DATA JUSLIN/'Mary  had   a   little   lamb.'/

*.

*    Test CHR_RJUST

      LINE = MARY(1:24)
      CALL CHR_RJUST( LINE )
      IF ( LINE .EQ. JUSLIN ) THEN
         PRINT *, 'CHR_RJUST OK'
      ELSE
         PRINT *, 'CHR_RJUST FAILS - LINE is:',LINE
         STATUS = SAI__ERROR
      END IF

      END
