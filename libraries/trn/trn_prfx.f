      SUBROUTINE TRN_PRFX( LOCTR1, LOCTR2, STATUS )







*+
*  Name:
*     TRN_PRFX

*  Purpose:
*     prefix transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_PRFX( LOCTR1, LOCTR2, STATUS )

*  Description:
*     The routine concatenates two transformation structures passed by
*     HDS locator.  The second transformation is modified by prefixing
*     the first one to it.  The first transformation is not altered.

*  Arguments:
*     LOCTR1 = CHARACTER * ( * ) (given)
*        HDS locator to first transformation structure.
*     LOCTR2 = CHARACTER * ( * ) (given)
*        HDS locator to second transformation structure, which is to
*        be altered.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Call TRN_JOIN to concatenate the two transformation structures,
*       producing a new temporary structure.
*     - Erase the contents of the second transformation structure
*       and replace them with the contents of the temporary structure.
*     - Erase the temporary structure.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     1-DEC-1988:  Fixed bug in argument list to TRN_JOIN (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      CHARACTER * ( * ) LOCTR1  ! Locator to first transformation

      CHARACTER * ( * ) LOCTR2  ! Locator to second transformation


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCTMP
                                ! Locator to temporary transformation


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Join the two transformations to create a new temporary object.
      CALL TRN_JOIN( LOCTR1, LOCTR2, ' ', ' ', LOCTMP, STATUS )


*   If there is no error, empty the second transformation structure and
*   copy the contents of the new temporary one into it.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL TRN1_EMPST( LOCTR2, STATUS )
        CALL TRN1_CPYST( LOCTMP, LOCTR2, STATUS )
      ENDIF


*   Erase the temporary structure.
      CALL TRN1_ANTMP( LOCTMP, STATUS )


*   Exit routine.
      END
