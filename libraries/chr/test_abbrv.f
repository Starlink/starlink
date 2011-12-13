      SUBROUTINE TEST_ABBRV(STATUS)
*+
*  Name:
*     TEST_ABBRV

*  Purpose:
*     Test CHR_ABBRV.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_ABBRV(STATUS)

*  Description:
*     Test CHR_ABBRV.
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
*     CHR_ABBRV

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

*  External References:
      LOGICAL CHR_ABBRV

*  Local Variables:
      INTEGER JSTAT              ! Local status
      LOGICAL LSTAT

*.

*    Test CHR_ABBRV

*    Equal strings, NCHAR too big
      LSTAT = .TRUE.
      JSTAT = SAI__OK
      LSTAT = CHR_ABBRV ('ABCDEF','ABCDEF',7)
      IF ((.NOT. LSTAT)) THEN
         PRINT *, 'CHR_ABBRV equal strings FAILS'
         JSTAT = SAI__ERROR
      ENDIF

*    Abbreviated string
      LSTAT = CHR_ABBRV ('ABC','ABCDEF',3)
      IF ((.NOT. LSTAT)) THEN
         PRINT *, 'CHR_ABBRV abbreviated string FAILS'
         JSTAT = SAI__ERROR
      ENDIF

*    Invalid Abbreviation
      LSTAT = CHR_ABBRV ('ABCF','ABCDEF',3)
      IF (( LSTAT)) THEN
         PRINT *, 'CHR_ABBRV invalid match detection FAILS'
         JSTAT = SAI__ERROR
      ENDIF

      IF (JSTAT .EQ. SAI__OK) THEN
         PRINT *, 'CHR_ABBRV OK'
         STATUS = JSTAT
      END IF

      END
